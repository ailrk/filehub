{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Filehub.Index where

import Lucid
import Lens.Micro
import Lens.Micro.Platform ()
import Filehub.Template qualified as Template
import Filehub.Env (Env(..))
import Filehub.Domain (NewFile (..), NewFolder(..), SearchWord(..), SortFileBy (..), sortFiles, ClientPath (..), UpdatedFile (..))
import Filehub.Domain qualified as Domain
import Effectful (Eff, (:>))
import Effectful.Reader.Dynamic (asks)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack, throwError)
import System.FilePath (takeFileName)
import GHC.Generics (Generic)
import Data.String (IsString(..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant.Server.Generic (AsServerT)
import Servant (Get, errBody, err500, (:-), ServerError (..), QueryParam, Post, Put, ReqBody, FormUrlEncoded, OctetStream, addHeader, Delete)
import Servant qualified as S
import Servant.HTML.Lucid (HTML)
import Servant.Server (err400)
import Effectful.Concurrent.STM (readTVarIO)
import Text.Printf (printf)
import Filehub (Filehub)


data Api mode = Api
  { index             :: mode :- Get '[HTML] (Html ())
  , cd                :: mode :- "cd" S.:> QueryParam "dir" ClientPath S.:> Get '[HTML] (Html ())
  , newFile           :: mode :- "files" S.:> "new" S.:> ReqBody '[FormUrlEncoded] NewFile S.:> Post '[HTML] (Html ())
  , updateFile        :: mode :- "files" S.:> "update" S.:> ReqBody '[FormUrlEncoded] UpdatedFile S.:> Put '[HTML] (Html ())
  , deleteFile        :: mode :- "files" S.:> "delete" S.:> QueryParam "file" ClientPath S.:> Delete '[HTML] (Html ())
  , newFolder         :: mode :- "folders" S.:> "new" S.:> ReqBody '[FormUrlEncoded] NewFolder S.:> Post '[HTML] (Html ())
  , newFileModal      :: mode :- "modal" S.:> "new-file" S.:> Get '[HTML] (Html ())
  , newFolderModal    :: mode :- "modal" S.:> "new-folder" S.:> Get '[HTML] (Html ())
  , fileDetailModal   :: mode :- "modal" S.:> "file" S.:> "detail" S.:> QueryParam "file" ClientPath S.:> Get '[HTML] (Html ())
  , uploadModal       :: mode :- "modal" S.:> "upload" S.:> Get '[HTML] (Html ())
  , editorModal       :: mode :- "modal" S.:> "editor" S.:> QueryParam "file" ClientPath S.:> Get '[HTML] (Html ())
  , search            :: mode :- "search" S.:> ReqBody '[FormUrlEncoded] SearchWord S.:> Post '[HTML] (Html ())
  , sortTable         :: mode :- "table" S.:> "sort" S.:> QueryParam "by" SortFileBy S.:> Get '[HTML] (Html ())
  , upload            :: mode :- "upload" S.:> MultipartForm Mem (MultipartData Mem) S.:> Post '[HTML] (Html ())
  , download          :: mode :- "download" S.:> QueryParam "file" ClientPath
                            S.:> Get '[OctetStream] (S.Headers '[S.Header "Content-Disposition" String] LBS.ByteString)
  , contextMenu       :: mode :- "contextmenu" S.:> QueryParam "file" ClientPath S.:> Get '[HTML] (Html ())
  }
  deriving (Generic)


server :: Api (AsServerT Filehub)
server = Api
  { index = index

  , cd = \path -> do
      root <- asks @Env (.root)
      Domain.changeDir (maybe "/" (Domain.fromClientPath root) path) & withServerError >> view ByName

  , newFile = \(NewFile path) -> do
      Domain.newFile (Text.unpack path) & withServerError
      view ByName

  , updateFile = \(UpdatedFile clientPath content) -> do
      root <- asks @Env (.root)
      let path = Domain.fromClientPath root clientPath
      Domain.writeFile path (Text.encodeUtf8 content ^. lazy) & withServerError
      view ByName

  , deleteFile = \case
      Just path -> do
        p <- Domain.fromClientPath <$> asks @Env (.root) <*> pure path
        Domain.deleteFile p & withServerError
        view ByName
      Nothing -> throwError err400

  , newFolder = \(NewFolder path) -> Domain.newFolder (Text.unpack path) & withServerError >> index

  , newFileModal = pure Template.newFileModal

  , newFolderModal = pure Template.newFolderModal

  , fileDetailModal = \case
      Just path -> do
        p <- Domain.fromClientPath <$> asks @Env (.root) <*> pure path
        file <- Domain.getFile p & withServerError
        pure (Template.fileDetailModal file)
      Nothing -> throwError err400

  , uploadModal = pure Template.uploadModal

  , editorModal = \case
      Just path -> do
        p <- Domain.fromClientPath <$> asks @Env (.root) <*> pure path
        content <- Domain.getFile p & withServerError >>= Domain.readFileContent
        let filename = takeFileName p
        pure $ Template.editorModal filename content
      Nothing -> throwError err400

  , search = \searchWord ->
      Template.search searchWord
        <$> asks @Env (.root)
        <*> withServerError Domain.lsCurrentDir

  , sortTable = \order -> view (fromMaybe ByName order)

  , upload = \multipart -> Domain.upload multipart & withServerError >> index

  , download = \case
      Just path@(ClientPath p) -> do
        bs <- Domain.download path & withServerError
        pure $ addHeader (printf "attachement; filename=%s.zip" (takeFileName p)) bs
      Nothing -> throwError err400

  , contextMenu = \case
      Just path -> pure $ Template.contextMenu path
      Nothing -> throwError err400
  }


withServerError :: (Error ServerError :> es) => Eff (Error String : es) b -> Eff es b
withServerError action = do
  runErrorNoCallStack @String action >>= \case
    Left err -> throwError err500 { errBody = fromString err }
    Right res -> pure res


index :: Filehub (Html ())
index = Template.index <$> view ByName


view :: SortFileBy -> Filehub (Html ())
view byOrder = Template.view <$> table <*> pathBreadcrumb
  where table = Template.table <$> asks @Env (.root) <*> (sortFiles byOrder <$> withServerError Domain.lsCurrentDir)


pathBreadcrumb :: Filehub (Html ())
pathBreadcrumb = Template.pathBreadcrumb <$>  currentDir <*> root
  where
    currentDir = asks @Env (.currentDir) >>= readTVarIO
    root = asks @Env (.root)
