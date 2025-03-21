{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Filehub.Index where

import Lucid
import Lens.Micro
import Lens.Micro.Platform ()
import Effectful (Eff, (:>))
import Filehub.Template qualified as Template
import Filehub.Env qualified as Env
import Filehub.Domain (NewFile (..), NewFolder(..), SearchWord(..), SortFileBy (..), sortFiles, ClientPath (..), UpdatedFile (..), Theme (..), Viewer(..))
import Filehub.Domain qualified as Domain
import Effectful.Error.Dynamic (runErrorNoCallStack, throwError, Error)
import Effectful.FileSystem.IO.ByteString.Lazy (readFile)
import System.FilePath ((</>), takeFileName, takeDirectory)
import GHC.Generics (Generic)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as LBS
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Servant.Server.Generic (AsServerT)
import Servant (Get, (:-), QueryParam, Post, Put, ReqBody, FormUrlEncoded, OctetStream, addHeader, noHeader, Delete, ServerError (errBody))
import Servant qualified as S
import Servant.HTML.Lucid (HTML)
import Servant.Server (err400, err500)
import Text.Printf (printf)
import Filehub (Filehub)
import Prelude hiding (readFile)


data Api mode = Api
  { index             :: mode :- Get '[HTML] (Html ())
  , cd                :: mode :- "cd" S.:> QueryParam "dir" ClientPath S.:> Get '[HTML] (S.Headers '[S.Header "HX-Trigger" Domain.FilehubError] (Html ()))
  , newFile           :: mode :- "files" S.:> "new" S.:> ReqBody '[FormUrlEncoded] NewFile S.:> Post '[HTML] (S.Headers '[S.Header "HX-Trigger" Domain.FilehubError] (Html ()))
  , updateFile        :: mode :- "files" S.:> "update" S.:> ReqBody '[FormUrlEncoded] UpdatedFile S.:> Put '[HTML] (Html ())
  , deleteFile        :: mode :- "files" S.:> "delete" S.:> QueryParam "file" ClientPath S.:> Delete '[HTML] (Html ())
  , newFolder         :: mode :- "folders" S.:> "new" S.:> ReqBody '[FormUrlEncoded] NewFolder S.:> Post '[HTML] (S.Headers '[S.Header "HX-Trigger" Domain.FilehubError] (Html ()))
  , newFileModal      :: mode :- "modal" S.:> "new-file" S.:> Get '[HTML] (Html ())
  , newFolderModal    :: mode :- "modal" S.:> "new-folder" S.:> Get '[HTML] (Html ())
  , fileDetailModal   :: mode :- "modal" S.:> "file" S.:> "detail" S.:> QueryParam "file" ClientPath S.:> Get '[HTML] (Html ())
  , uploadModal       :: mode :- "modal" S.:> "upload" S.:> Get '[HTML] (Html ())
  , editorModal       :: mode :- "modal" S.:> "editor" S.:> QueryParam "file" ClientPath S.:> Get '[HTML] (Html ())
  , search            :: mode :- "search" S.:> ReqBody '[FormUrlEncoded] SearchWord S.:> Post '[HTML] (Html ())
  , sortTable         :: mode :- "table" S.:> "sort" S.:> QueryParam "by" SortFileBy S.:> Get '[HTML] (Html ())
  , upload            :: mode :- "upload" S.:> MultipartForm Mem (MultipartData Mem) S.:> Post '[HTML] (Html ())
  , download          :: mode :- "download" S.:> QueryParam "file" ClientPath S.:> Get '[OctetStream] (S.Headers '[S.Header "Content-Disposition" String] LBS.ByteString)
  , contextMenu       :: mode :- "contextmenu" S.:> QueryParam "file" ClientPath S.:> Get '[HTML] (Html ())
  , initViewer     :: mode :- "viewer" S.:> QueryParam "file" ClientPath S.:> Get '[HTML] (S.Headers '[S.Header "HX-Trigger" Domain.Viewer ] (Html ()))
  , themeCss          :: mode :- "theme.css" S.:> Get '[OctetStream] LBS.ByteString
  }
  deriving (Generic)


server :: Api (AsServerT Filehub)
server = Api
  { index = index


  , cd = \mClientPath -> do
      root <- Env.getRoot
      r <- Domain.changeDir (maybe "/" (Domain.fromClientPath root) mClientPath) & runErrorNoCallStack
      let withHeader =
            case r of
              Left err -> addHeader err
              Right _ -> noHeader
      withHeader <$> view


  , newFile = \(NewFile path) -> do
      r <- Domain.newFile (Text.unpack path) & runErrorNoCallStack
      let withHeader =
            case r of
              Left err -> addHeader err
              Right _ -> noHeader
      withHeader <$> view


  , updateFile = \(UpdatedFile clientPath content) -> do
      let path = clientPath.unClientPath
      Domain.writeFile path (Text.encodeUtf8 content ^. lazy)
      view


  , deleteFile = \case
      Just clientPath -> do
        p <- Domain.fromClientPath <$> Env.getRoot <*> pure clientPath
        Domain.deleteFile p
        view
      Nothing -> throwError err400


  , newFolder = \(NewFolder path) -> do
      r <- Domain.newFolder (Text.unpack path) & runErrorNoCallStack
      let withHeader =
            case r of
              Left err -> addHeader err
              Right _ -> noHeader
      withHeader <$> view


  , newFileModal = pure Template.newFileModal


  , newFolderModal = pure Template.newFolderModal


  , fileDetailModal = \case
      Just clientPath -> do
        p <- Domain.fromClientPath <$> Env.getRoot <*> pure clientPath
        file <- Domain.getFile p & withServerError
        pure (Template.fileDetailModal file)
      Nothing -> throwError err400


  , uploadModal = pure Template.uploadModal


  , editorModal = \case
      Just clientPath -> do
        p <- Domain.fromClientPath <$> Env.getRoot <*> pure clientPath
        content <- Domain.getFile p & withServerError >>= Domain.readFileContent
        let filename = takeFileName p
        pure $ Template.editorModal filename content
      Nothing -> throwError err400


  , search = \searchWord ->
      Template.search searchWord
        <$> Env.getRoot
        <*> withServerError Domain.lsCurrentDir


  , sortTable = \order -> do
      Env.setSortFileBy (fromMaybe ByName order)
      view


  , upload = \multipart -> Domain.upload multipart & withServerError >> index


  , download = \case
      Just clientPath@(ClientPath path) -> do
        bs <- Domain.download clientPath & withServerError
        pure $ addHeader (printf "attachement; filename=%s.zip" (takeFileName path)) bs
      Nothing -> throwError err400


  , contextMenu = \case
      Just clientPath -> do
        root <- Env.getRoot
        let filePath = Domain.fromClientPath root clientPath
        file <- Domain.getFile filePath & withServerError
        pure $ Template.contextMenu root file
      Nothing -> throwError err400


  , initViewer = \case
      Just clientPath -> do
        root <- Env.getRoot
        payload <- Domain.initViewer root clientPath & withServerError
        pure $ addHeader payload mempty
      Nothing -> throwError err400 { errBody = "No image path" }


  , themeCss = do
      theme <- Env.getTheme
      dir <- Env.getDataDir
      readFile $
        case theme of
          Dark1 -> dir </> "dark1.css"
          Dark2 -> dir </> "dark2.css"
          Dark3 -> dir </> "dark3.css"
          Light1 -> dir </> "light1.css"
          Light2 -> dir </> "light2.css"
          Light3 -> dir </> "light3.css"
  }


withServerError :: (Error ServerError :> es) => Eff (Error Domain.FilehubError : es) b -> Eff es b
withServerError action = do
  runErrorNoCallStack action >>= \case
    Left err -> throwError err500 { errBody = fromString $ show err }
    Right res -> pure res


index :: Filehub (Html ())
index = Template.index <$> view


view :: Filehub (Html ())
view = do
  order <- Env.getSortFileBy
  let table = Template.table
          <$> Env.getRoot
          <*> (sortFiles order <$> withServerError Domain.lsCurrentDir)
  Template.view <$> table <*> pathBreadcrumb


pathBreadcrumb :: Filehub (Html ())
pathBreadcrumb = Template.pathBreadcrumb <$> Env.getCurrentDir <*> Env.getRoot
