{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
module Filehub.Server.File
  ( cd
  , delete
  , rename
  , newFile
  , updateFile
  , newFolder
  , copy
  , copy1
  , paste
  , move
  , download
  , upload
  , serve
  , thumbnail
  ) where

import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, MonadIO (..), runResourceT, runConduit, (.|))
import Conduit qualified
import Control.Monad (void, when, replicateM, join)
import Control.Monad.Reader (MonadReader(..))
import Data.Binary.Builder qualified as Builder
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ClientPath (ClientPath (..), AbsPath (..), (<./>), Root (..))
import Data.ClientPath qualified as ClientPath
import Data.ClientPath.IO (validateAbsPath)
import Data.ClientPath.View (ClientPathView(..), asClientPathView)
import Data.Coerce (coerce)
import Data.File (FileType(..), File(..), FileContent (..), defaultFileWithContent, FileInfo, IsLink (..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Traversable (for)
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.File.Paste (paste)
import Filehub.Server.File.Delete (delete)
import Filehub.Server.UI qualified as UI
import Filehub.Server.Util (withQueryParam)
import Filehub.Session (SessionGet(..))
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Types (Selected(..))
import Filehub.Sort qualified as Sort
import Filehub.Types ( NewFile(..) , NewFolder(..), UpdatedFile(..) , UpdatedFile(..) , FilehubEvent (..), RenameFile (..), MoveFile (..), Env)
import Lucid hiding (for_)
import Lucid.Htmx (HxSwapOOB(..), hxOn, Trigger (..))
import Network.HTTP.Types (ByteRange(..), parseByteRanges)
import Network.HTTP.Types.Status (status404, status206, status200)
import Network.Mime.Extended (isMime)
import Network.Wai (Request(..), responseLBS, responseStream)
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader, Tagged (..), Application, ServerError (..), FromHttpApiData (..), err404)
import Servant.Multipart (MultipartData(..), Mem)
import System.Directory (removeFile)
import System.FilePath (takeFileName, (</>), makeRelative, takeDirectory)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Text.Printf (printf)
import UnliftIO (throwIO, try)
import UnliftIO.Async (async, forConcurrently_)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue)
import Worker.Task (newTaskId)


cd :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub (Headers '[ Header "HX-Trigger-After-Swap" FilehubEvent ] (Html ()))
cd sessionId _ mClientPath = do
  root       <- Session.get sessionId (.root)
  storage    <- Session.get sessionId (.storage)
  clientPath <- withQueryParam mClientPath
  storage.cd (ClientPath.fromClientPath root clientPath)
  html <- do
    toolBar' <- UI.toolBar sessionId
    view'    <- UI.view sessionId
    pure do
      toolBar' `with` [ hxSwapOOB True ]
      view'
  pure $ addHeader DirChanged html


rename :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> RenameFile
  -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
rename sessionId _ _ (RenameFile old new) = do
  storage <- Session.get sessionId (.storage)
  root    <- Session.get sessionId (.root)
  storage.rename
    (ClientPath.fromClientPath root old)
    new
  html <- UI.view sessionId
  pure $ addHeader FileRenamed html


updateFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> UpdatedFile -> Filehub (Html ())
updateFile sessionId _ _ (UpdatedFile clientPath content) = do
  storage <- Session.get sessionId (.storage)
  root    <- Session.get sessionId (.root)
  let path  = ClientPath.fromClientPath root clientPath
  storage.write $ defaultFileWithContent
    { path     = path
    , content  = FileContentRaw (Text.encodeUtf8 content)
    }
  UI.view sessionId


newFile' :: SessionId -> Text -> (AbsPath -> Filehub FileInfo) -> Filehub (Html ())
newFile' sessionId name create = do
  storage <- Session.get sessionId (.storage)
  dir     <- Session.get sessionId (.currentDir)
  order   <- Session.get sessionId (.sortedFileBy)
  root    <- Session.get sessionId (.root)
  path    <- validateAbsPath (coerce dir </> Text.unpack name) (FilehubError InvalidPath ("<redacted>/" <> show name))
  file    <- create path
  files   <- Sort.sortFiles order <$> storage.ls dir
  entry'  <- UI.entry sessionId file

  let target :: Text
      target = case getPrev file files of
                 Just prevFile -> let ClientPathView { hashPath } = asClientPathView root prevFile.path
                                   in [i|afterend:\#tr-#{hashPath}|]
                 Nothing       -> [i|afterbegin:\#table|]

  pure do
    div_  [ hxSwapOOB target ] do
      entry' `with` [ hxOn Load "this.focus();"
                    , tabindex_ "-1" ]

  where
    getPrev target list =
      case break (== target) list of
          (before, _) | not (null before) -> Just (last before)
          _                               -> Nothing


newFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFile -> Filehub (Html ())
newFile sessionId _ _ (NewFile name) = do
  storage <- Session.get sessionId (.storage)
  newFile' sessionId name storage.new


newFolder :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFolder -> Filehub (Html ())
newFolder sessionId _ _ (NewFolder name) = do
  storage <- Session.get sessionId (.storage)
  newFile' sessionId name storage.newFolder


copy :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Filehub (Html ())
copy sessionId _ _ = do
  Copy.select sessionId
  Copy.copy sessionId
  UI.controlPanel sessionId


copy1 :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
copy1 sessionId _ _ mClientPath = do
  clientPath <- withQueryParam mClientPath
  UI.clear sessionId
  Session.set sessionId (.selected) (Selected clientPath [])
  Copy.select sessionId
  Copy.copy sessionId
  UI.index sessionId


move :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MoveFile
     -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
move sessionId _ _ (MoveFile src tgt) = do
  storage       <- Session.get sessionId (.storage)
  root          <- Session.get sessionId (.root)
  notifications <- Session.get sessionId (.notifications)
  env           <- ask
  taskId        <- newTaskId
  let srcPaths  =  fmap (ClientPath.fromClientPath root) src
  let tgtPath   =  ClientPath.fromClientPath root tgt

  -- check before take action
  checkedSrcPaths <- catMaybes <$> for srcPaths \srcPath -> do
    isTgtDir <- storage.isDirectory tgtPath
    when (not isTgtDir) do
      throwIO (FilehubError InvalidDir "Target is not a directory")

    when (srcPath == tgtPath)  do
      throwIO (FilehubError InvalidDir "Can't move to the same directory")

    if | coerce takeDirectory srcPath == tgtPath -> do pure Nothing
       | otherwise -> do
           let dstPath = tgtPath <./> coerce takeFileName srcPath

           eFile <- try @_ @FilehubError $ storage.get dstPath

           case eFile of
             Right _  -> pure Nothing
             Left  _  -> pure $ Just srcPath

  void . async . liftIO . runFilehub env $ do
    storage.mv do
      fmap (\srcPath -> (srcPath, tgtPath <./> coerce takeFileName srcPath)) checkedSrcPaths

    view' <- UI.view sessionId
    atomically do
      writeTBQueue notifications $ TaskCompleted
        { taskId       = taskId
        , htmxResponse = Just $ view' `with` [ hxSwapOOB True
                                             , tabindex_ "-1"
                                             ]
        }

  UI.clear sessionId
  addHeader FileMoved <$>
    (do controlPanel' <- UI.controlPanel sessionId
        sideBar'      <- UI.sideBar sessionId
        pure do
          controlPanel' `with` [ hxSwapOOB True ]
          sideBar' `with` [ hxSwapOOB True ])



download :: SessionId -> ConfirmLogin -> [ClientPath]
         -> Filehub (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))
download sessionId _ clientPaths = do
  root    <- Session.get sessionId (.root)
  storage <- Session.get sessionId (.storage)
  case clientPaths of
    [clientPath@(ClientPath path)] -> do
      file    <- storage.get (ClientPath.fromClientPath root clientPath)

      case file.isLink of
        BrokenLink -> throwIO (FilehubError undefined "Can't download a broken link")
        _          -> pure ()

      conduit <- storage.download clientPath

      let filename = case file.content of
                       Dir -> printf "attachement; filename=%s.zip" (takeFileName path)
                       _   -> printf "attachement; filename=%s" (takeFileName path)

      pure $ addHeader filename conduit

    _ -> do
      (zipPath, _) <- liftIO do
        tempDir <- Temp.getCanonicalTemporaryDirectory
        Temp.openTempFile tempDir "DXXXXXX.zip"

      files <- traverse (storage.get . ClientPath.fromClientPath root) clientPaths

      tasks <- for files \file -> do
        conduit <- storage.readStream file Nothing Nothing
        pure (file.path, conduit)

      Zip.createArchive zipPath do
        for_ tasks \(path, conduit) -> do
          m <- Zip.mkEntrySelector (coerce makeRelative root path)
          Zip.sinkEntry Zip.Zstd conduit m

      tag <- Text.pack <$> replicateM 8 (randomRIO ('a', 'z'))

      let conduit =
            Conduit.bracketP
              (pure ())
              (\_ -> removeFile zipPath)
              (\_ -> Conduit.sourceFile zipPath)
      pure $ addHeader (printf "attachement; filename=%s.zip" tag) conduit


upload :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MultipartData Mem
       -> Filehub (Html ())
upload sessionId _ _ multipart = do
  notifications <- Session.get sessionId (.notifications)
  taskId        <- newTaskId
  uploadCounter <- newTVarIO @_ @Integer 0
  env           <- ask
  let taskCount =  fromIntegral $ length multipart.files

  void . async . liftIO . runFilehub env $ do
    atomically do
      writeTBQueue notifications $ UploadProgressed
        { taskId       = taskId
        , progress     = 0
        , htmxResponse = Nothing
        }

    storage <- Session.get sessionId (.storage)
    forConcurrently_ multipart.files \filedata -> do
      storage.upload filedata
      atomically do
        modifyTVar' uploadCounter (+ 1)
        n <- readTVar uploadCounter
        writeTBQueue notifications $ UploadProgressed
          { taskId       = taskId
          , progress     = n % max 1 taskCount
          , htmxResponse = Nothing
          }

    view' <- UI.view sessionId
    atomically do
      writeTBQueue notifications $ UploadProgressed
        { taskId       = taskId
        , progress     = 1
        , htmxResponse = Nothing
        }
      writeTBQueue notifications $ TaskCompleted
        { taskId       = taskId
        , htmxResponse = Just $ view' `with` [ hxSwapOOB True ]
        }

  UI.index sessionId


serve :: Env -> SessionId -> ConfirmLogin -> Tagged Filehub Application
serve env sessionId _ = Tagged $ \req respond -> do
  let query = queryString req
  let mFile = join $ lookup "file" query

  -- Lookup the file
  res <- runFilehub env do
    root       <- Session.get sessionId (.root)
    storage    <- Session.get sessionId (.storage)
    clientPath <- do
      text <- Text.decodeUtf8 <$> withQueryParam mFile
      case parseUrlPiece @ClientPath text of
        Right c  -> pure c
        Left err -> throwIO do HTTPError err404 { errBody = [i|#{err}|] }

    let path = ClientPath.fromClientPath root clientPath

    file <- storage.get path
    pure (Just (file, storage))

  case res of
    Left err                         -> throwIO err
    Right Nothing                    -> respond $ responseLBS status404 [] "File not found"
    Right (Just (fileInfo, storage)) -> do
      let fileSize = fromMaybe 0 fileInfo.size

      let mReqRange = lookup "Range" (requestHeaders req) >>= parseByteRanges

      -- We need handle the Range header to support video seek.
      let (status, mOff, mLen) = case mReqRange of
                                   Just (ByteRangeFromTo s e : _) -> (status206, Just s, Just (e - s + 1))
                                   Just (ByteRangeFrom s : _)     -> (status206, Just s, Just (fileSize - s))
                                   _                              -> (status200, Nothing, Nothing)

      let renderRangeHeader s e t = Char8.pack $ printf "bytes %d-%d/%d" s e t

      let from = fromMaybe 0 mOff
      let len  = fromMaybe 0 mLen
      let to   = from + len - 1

      let headers = [ ("Content-Type", fileInfo.mimetype)
                    , ("Accept-Ranges", "bytes")
                    ]
                ++ if status == status206
                      then [ ("Content-Range", renderRangeHeader from  to fileSize)
                           , ("Content-Length", Char8.pack (show len))
                           ]
                      else [ ("Content-Length", Char8.pack (show fileSize)) ]

      respond do
        responseStream status headers $ \send flush -> do

          mStream <- runFilehub env do
            storage.readStream fileInfo mOff mLen

          case mStream of
            Left err     -> throwIO do HTTPError err404 { errBody = [i|#{err}|] }
            Right stream ->
              runResourceT . runConduit
                $ stream
                .| Conduit.mapM_C \chunk -> liftIO do send (Builder.fromByteString chunk); flush


thumbnail :: SessionId -> ConfirmLogin -> Maybe ClientPath
          -> Filehub (Headers '[ Header "Content-Type" String
                               , Header "Content-Disposition" String
                               , Header "Cache-Control" String
                               ]
                               (ConduitT () ByteString (ResourceT IO) ()))
thumbnail sessionId _ mFile = do
  root       <- Session.get sessionId (.root)
  storage    <- Session.get sessionId (.storage)
  clientPath <- withQueryParam mFile
  let path   =  ClientPath.fromClientPath root clientPath
  file       <- storage.get path
  conduit    <- serveOriginal storage file

  pure
    . addHeader (ByteString.unpack file.mimetype)
    . addHeader (printf "inline; filename=%s" (coerce takeFileName path :: String))
    . addHeader "public, max-age=31536000, immutable"
    $ conduit

  where
    serveOriginal storage file =
      if
        | file.mimetype `isMime` "image" -> storage.readStream file Nothing Nothing
        | otherwise                      -> throwIO (FilehubError FormatError "Invalid mime type for thumbnail")
