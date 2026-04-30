{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.File (cd, delete, rename, newFile, updateFile, newFolder, copy, copy1, paste, move, download, upload, serve) where

import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, MonadIO (..), runResourceT, runConduit, (.|))
import Conduit qualified
import Control.Monad (void, when, replicateM, join)
import Control.Monad.Fix (fix)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath (..), AbsPath (..), (<./>), Root (..))
import Data.ClientPath qualified as ClientPath
import Data.ClientPath.IO (validateAbsPath)
import Data.Coerce (coerce)
import Data.File (FileType(..), File(..), FileContent (..), withContent, defaultFileWithContent, FileInfo, IsLink (..))
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly)
import Filehub.Monad
import Filehub.Notification.Types (Notification(..))
import Filehub.Orphan ()
import Filehub.Server.Util (withQueryParam)
import Filehub.Session (SessionId(..), TargetView (..), withTarget)
import Filehub.Session qualified as Session
import Filehub.Session (SessionGet(..))
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Types ( NewFile(..) , NewFolder(..)    , Selected (..)    , UpdatedFile(..) , UpdatedFile(..) , FilehubEvent (..), RenameFile (..), CopyState (..), TargetSessionData (..), Selected(..), MoveFile (..), Env)
import Lens.Micro ((.~))
import Lucid hiding (for_)
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader, Tagged (..), Application, ServerError (..), FromHttpApiData (..), err404)
import Servant.Multipart (MultipartData(..), Mem)
import System.Directory (removeFile)
import System.FilePath (takeFileName, (</>), makeRelative, takeDirectory)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Target.Types (AnyTarget)
import Text.Printf (printf)
import Worker.Task (newTaskId)
import UnliftIO (throwIO, try)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue, newTQueueIO, writeTQueue)
import Log (logAttention_)
import UnliftIO.Async (async, forConcurrently_)
import Filehub.Server.UI qualified as UI
import Network.Wai (Request(..), responseLBS, responseStream)
import Network.HTTP.Types.Status (status404, status206, status200)
import Data.Binary.Builder qualified as Builder
import Network.HTTP.Types (ByteRange(..), parseByteRanges)
import Data.ByteString.Char8 qualified as Char8
import Filehub.Sort qualified as Sort
import Data.ClientPath.View (ClientPathView(..), asClientPathView)
import Data.Text (Text)


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
      toolBar' `with` [ term "hx-swap-oob" "true" ]
      view'
  pure $ addHeader DirChanged html


-- | Delete files.
-- This handler returns immediately, which command the frontend to open a
-- /listen connection. Meanwhile it spawns a new thread running the deletion
-- task. The new thread periodically report progress to the frontend via
-- /listen, and on complete it will send a htmx response that the frontend can
-- use to update the UI.
delete :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Bool
       -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                            , Header "HX-Trigger" FilehubEvent
                            ] (Html ()))
delete sessionId _ _ clientPaths deleteSelected = do
  root          <- Session.get sessionId (.root)
  storage       <- Session.get sessionId (.storage)
  notifications <- Session.get sessionId (.notifications)
  count         <- length <$> Selected.allSelecteds sessionId
  taskId        <- newTaskId
  deleteCounter <- newTVarIO @_ @Integer 0
  deleted       <- newTQueueIO @_ @ClientPath

  -- Record on each successful delete.
  let jot clientPath = do
        modifyTVar' deleteCounter (+ 1)
        writeTQueue deleted clientPath
        readTVar deleteCounter

  void $ async do
    -- Make sure the frontend opens a /listen connection otherwise this will block.
    atomically do
      writeTBQueue notifications $ DeleteProgressed
        { taskId       = taskId
        , progress     = 0
        , htmxResponse = Nothing
        }

    -- Delete from parameters
    forConcurrently_ clientPaths \clientPath -> do
      let ClientPathView { path, hashPath } = asClientPathView root clientPath
      storage.delete path
      atomically do
        n <- jot clientPath
        writeTBQueue notifications $ DeleteProgressed
          { taskId       = taskId
          , progress     = n % max 1 (fromIntegral count)
          , htmxResponse = Just $ div_ [ id_ [i|tr-#{hashPath}|], term "hx-swap-oob" "delete" ] mempty
          }

    when deleteSelected do
      allSelecteds <- Selected.allSelecteds sessionId
      for_ allSelecteds \(target, selected) -> do
        withTarget sessionId target do
          case selected of
            NoSelection -> pure ()
            Selected x xs -> do
              let ps = fmap (asClientPathView root) (x:xs)
              forConcurrently_  ps \(ClientPathView { path, clientPath, hashPath }) -> do
                storage.delete path
                atomically do
                  n <- jot clientPath
                  writeTBQueue notifications $ DeleteProgressed
                    { taskId       = taskId
                    , progress     = n % max 1 (fromIntegral count)
                    , htmxResponse = Just $ div_ [ id_ [i|tr-#{hashPath}|], term "hx-swap-oob" "delete" ] mempty
                    }

    atomically do
      writeTBQueue notifications $ TaskCompleted
        { taskId      = taskId
        , htmxResponse = Nothing
        }

  UI.clear sessionId
  newCount <- length <$> Selected.allSelecteds sessionId
  addHeader newCount . addHeader SSEStarted
    <$> (do controlPanel' <- UI.controlPanel sessionId
            sideBar'      <- UI.sideBar sessionId
            pure do
              controlPanel' `with` [ term "hx-swap-oob" "true" ]
              sideBar' `with` [ term "hx-swap-oob" "true" ])


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

  let target = case getPrev file files of
                 Just prevFile -> let ClientPathView { hashPath } = asClientPathView root prevFile.path
                                   in [i|afterend:\#tr-#{hashPath}|]
                 Nothing       -> [i|afterbegin:\#table|]

  pure do
    div_  [ term "hx-swap-oob" target ] do
      entry' `with` [ term "hx-on::load" "this.focus();"
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
  Selected.setSelected sessionId (Selected clientPath [])
  Copy.select sessionId
  Copy.copy sessionId
  UI.index sessionId


data PasteTask
  = PasteFile { from  :: AnyTarget
              , to    :: AnyTarget
              , file  :: FileInfo
              , dst   :: AbsPath
              }
  | CreateDir { to  :: AnyTarget
              , dst :: AbsPath
              }


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly
      -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                           , Header "HX-Trigger" FilehubEvent
                           ] (Html ()))
paste sessionId _ _ = do
  notifications <- Session.get sessionId (.notifications)
  pasteCounter  <- newTVarIO @_ @Integer 0
  taskId        <- newTaskId
  state         <- Copy.getCopyState sessionId
  pasted        <- newTQueueIO @_ @PasteTask

  let jot clientPath = do
        modifyTVar' pasteCounter (+ 1)
        writeTQueue pasted clientPath
        readTVar pasteCounter

  case state of
    Paste selections -> void $ async do
      tasks <- do
        TargetView to sdata <- Session.get sessionId (.currentTarget)
        createPasteTasks sdata.currentDir to selections

      let taskCount = fromIntegral (length tasks)

      forConcurrently_ tasks $ \task -> do
        case task of
          PasteFile { from, to, file, dst } -> do

            conduit <- withTarget sessionId from do
              storage <- Session.get sessionId (.storage)
              storage.readStream file Nothing Nothing

            withTarget sessionId to do
              storage <- Session.get sessionId (.storage)
              storage.write $ file
                & flip withContent (FileContentConduit conduit)
                & #path .~ dst

            atomically do
              n <- jot task
              writeTBQueue notifications $ PasteProgressed
                { taskId       = taskId
                , progress     = (n % max 1 taskCount)
                , htmxResponse = Nothing
                }

          CreateDir to dst -> do
            withTarget sessionId to do
              storage <- Session.get sessionId (.storage)
              void $ storage.newFolder dst

      Copy.setCopyState sessionId NoCopyPaste
      Selected.clearSelectedAllTargets sessionId

      view' <- UI.view sessionId
      atomically do
        writeTBQueue notifications $ TaskCompleted
          { taskId       = taskId
          , htmxResponse = Just $ view' `with` [ term "hx-swap-oob" "true" ]
          }

    _ -> do
      logAttention_ [i|[v8dsaz] #{sessionId}, not in pastable state.|]
      throwIO (FilehubError SelectError "Not in a pastable state")

  UI.clear sessionId
  selectedCount <- length <$> Selected.allSelecteds sessionId

  addHeader selectedCount . addHeader SSEStarted
    <$> (do controlPanel' <- UI.controlPanel sessionId
            sideBar'      <- UI.sideBar sessionId
            pure do
              controlPanel' `with` [ term "hx-swap-oob" "true" ]
              sideBar' `with` [ term "hx-swap-oob" "true" ])


  where
    createPasteTasks fromDir to selections = fmap (mconcat . mconcat) do
      for selections \(from, files) -> do
        for files $
          flip fix fromDir \rec (AbsPath currentDir) file -> do

          let name = coerce takeFileName file.path
          let path = (currentDir </> takeFileName name)

          dst <- validateAbsPath path (FilehubError InvalidPath "Invalid path")

          case file.isLink of
            BrokenLink -> pure []
            _          ->
              case file.content of
                Regular    -> pure [ PasteFile from to file dst ]
                Dir -> do
                  withTarget sessionId from do
                    storage <- Session.get sessionId (.storage)
                    (TargetView _ (TargetSessionData { currentDir = savedDir })) <- Session.get sessionId (.currentTarget)
                    storage.cd file.path
                    result <- do
                      dirFiles <- storage.lsCwd
                      for dirFiles \dfile -> rec dst dfile
                    storage.cd savedDir -- go back
                    pure $ ([CreateDir to dst] ++ mconcat result)



move :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MoveFile
     -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent
                          , Header "HX-Trigger" FilehubEvent
                          ] (Html ()))
move sessionId _ _ (MoveFile src tgt) = do
  storage       <- Session.get sessionId (.storage)
  root          <- Session.get sessionId (.root)
  notifications <- Session.get sessionId (.notifications)
  taskId        <- newTaskId
  let srcPaths  =  fmap (ClientPath.fromClientPath root) src
  let tgtPath   =  ClientPath.fromClientPath root tgt

  -- check before take action
  for_ srcPaths \srcPath -> do
    isTgtDir <- storage.isDirectory tgtPath
    when (not isTgtDir) do
      throwIO (FilehubError InvalidDir "Target is not a directory")

    when (srcPath == tgtPath)  do
      throwIO (FilehubError InvalidDir "Can't move to the same directory")

    when (coerce takeDirectory srcPath == tgtPath)  do
      throwIO (FilehubError InvalidDir "Already in the current directory")

    let dstPath = tgtPath <./> coerce takeFileName srcPath

    eFile <- try @_ @FilehubError $ storage.get dstPath

    case eFile of
      Right _  -> throwIO (FilehubError InvalidPath "The destination already exists")
      Left  _  -> pure ()

  void $ async do
    atomically do
      writeTBQueue notifications $ MoveProgressed
        { taskId       = taskId
        , progress     = 0
        , htmxResponse = Nothing
        }

    storage.mv do
      fmap (\srcPath -> (srcPath, tgtPath <./> coerce takeFileName srcPath)) srcPaths

    view' <- UI.view sessionId
    atomically do
      writeTBQueue notifications $ TaskCompleted
        { taskId       = taskId
        , htmxResponse = Just $ view' `with` [ term "hx-swap-oob" "true"
                                             , tabindex_ "-1"
                                             ]
        }

  UI.clear sessionId
  addHeader FileMoved . addHeader SSEStarted <$>
    (do controlPanel' <- UI.controlPanel sessionId
        sideBar'      <- UI.sideBar sessionId
        pure do
          controlPanel' `with` [ term "hx-swap-oob" "true" ]
          sideBar' `with` [ term "hx-swap-oob" "true" ])



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
       -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
upload sessionId _ _ multipart = do
  notifications <- Session.get sessionId (.notifications)
  taskId        <- newTaskId
  uploadCounter <- newTVarIO @_ @Integer 0
  let taskCount =  fromIntegral $ length multipart.files

  void $ async do
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

    atomically do
      writeTBQueue notifications $ UploadProgressed
        { taskId       = taskId
        , progress     = 1
        , htmxResponse = Nothing
        }
      writeTBQueue notifications $ TaskCompleted
        { taskId       = taskId
        , htmxResponse = Nothing
        }

  addHeader SSEStarted <$> UI.index sessionId


serve :: Env -> SessionId -> ConfirmLogin -> Tagged Filehub Application
serve env sessionId _ = Tagged $ \req respond -> do
  let query = queryString req
  let mFile = join $ lookup "file" query

  -- Lookup the file
  res <- runFilehub env $ do
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
