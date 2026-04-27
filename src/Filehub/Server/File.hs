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
import Data.File (FileType(..), File(..), FileContent (..), withContent, defaultFileWithContent, FileInfo)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Function ((&))
import Data.Maybe (catMaybes, isJust)
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
import Filehub.Session (SessionId(..), TargetView (..))
import Filehub.Session qualified as Session
import Filehub.Session (SessionGet(..))
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Types ( NewFile(..) , NewFolder(..)    , Selected (..)    , UpdatedFile(..) , UpdatedFile(..) , FilehubEvent (..), RenameFile (..), CopyState (..), TargetSessionData (..), Selected(..), MoveFile (..), Env)
import Lens.Micro ((.~), (<&>))
import Lucid hiding (for_)
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader, Tagged (..), Application, ServerError (..), FromHttpApiData (..), err404         )
import Servant.Multipart (MultipartData(..), Mem)
import System.Directory (removeFile)
import System.FilePath (takeFileName, (</>), makeRelative, takeDirectory)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Target.Types (AnyTarget)
import Target.Types qualified as Target
import Text.Printf (printf)
import Worker.Task (newTaskId)
import UnliftIO (throwIO)
import UnliftIO.STM (atomically, modifyTVar', readTVar, newTVarIO, writeTBQueue)
import Log (logAttention_)
import UnliftIO.Async (async, forConcurrently_)
import Filehub.Server.UI ( clear, index, view, controlPanel, toolBar )
import Network.Wai.Application.Static (StaticSettings(..), defaultFileServerSettings, staticApp)
import WaiAppStatic.Types qualified
import Network.Wai (Request(..), responseLBS, responseStream)
import Network.HTTP.Types.Status (status404)
import WaiAppStatic.Types (LookupResult(..), unsafeToPiece)
import Data.Binary.Builder qualified as Builder
import Foreign.C (CTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)


cd :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub (Headers '[ Header "HX-Trigger-After-Swap" FilehubEvent ] (Html ()))
cd sessionId _ mClientPath = do
  root       <- Session.get sessionId (.root)
  storage    <- Session.get sessionId (.storage)
  clientPath <- withQueryParam mClientPath
  storage.cd (ClientPath.fromClientPath root clientPath)
  html <- do
    toolBar' <- toolBar sessionId
    view'    <- view sessionId
    pure do
      toolBar' `with` [ term "hx-swap-oob" "true" ]
      view'
  pure $ addHeader DirChanged html


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

  void $ async do
    atomically do writeTBQueue notifications (DeleteProgressed taskId 0)

    do
      forConcurrently_ clientPaths \clientPath -> do
        let path = ClientPath.fromClientPath root clientPath
        storage.delete path
        atomically do
          modifyTVar' deleteCounter (+ 1)
          n <- readTVar deleteCounter
          writeTBQueue notifications (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    when deleteSelected do
      allSelecteds <- Selected.allSelecteds sessionId
      for_ allSelecteds \(target, selected) -> do
        let targetId = Target.getTargetId target
        Session.withTarget sessionId  targetId do
          case selected of
            NoSelection -> pure ()
            Selected x xs -> do
              forConcurrently_ (fmap (ClientPath.fromClientPath root) (x:xs)) \path -> do
                storage.delete path
                atomically do
                  modifyTVar' deleteCounter (+ 1)
                  n <- readTVar deleteCounter
                  writeTBQueue notifications (DeleteProgressed taskId (n % max 1 (fromIntegral count)))

    atomically do writeTBQueue notifications (TaskCompleted taskId)
  clear sessionId
  newCount <- length <$> Selected.allSelecteds sessionId
  addHeader newCount . addHeader SSEStarted <$> index sessionId


rename :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> RenameFile
  -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
rename sessionId _ _ (RenameFile old new) = do
  storage <- Session.get sessionId (.storage)
  root    <- Session.get sessionId (.root)
  storage.rename
    (ClientPath.fromClientPath root old)
    new
  html <- view sessionId
  pure $ addHeader FileRenamed html


newFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFile -> Filehub (Html ())
newFile sessionId _ _ (NewFile name) = do
  storage     <- Session.get sessionId (.storage)
  AbsPath dir <- Session.get sessionId (.currentDir)
  path        <- validateAbsPath (dir </> Text.unpack name) (FilehubError InvalidPath ("<redacted>/" <> show name))
  storage.new path
  view sessionId


updateFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> UpdatedFile -> Filehub (Html ())
updateFile sessionId _ _ (UpdatedFile clientPath content) = do
  storage <- Session.get sessionId (.storage)
  root    <- Session.get sessionId (.root)
  let path  = ClientPath.fromClientPath root clientPath
  storage.write $ defaultFileWithContent
    { path     = path
    , content  = FileContentRaw (Text.encodeUtf8 content)
    }
  view sessionId


newFolder :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFolder -> Filehub (Html ())
newFolder sessionId _ _ (NewFolder name) = do
  AbsPath dir <- Session.get sessionId (.currentDir)
  storage     <- Session.get sessionId (.storage)
  path <- validateAbsPath
            (dir </> Text.unpack name)
            (FilehubError InvalidPath ("<redacted>/" <> show name))
  storage.newFolder path
  view sessionId


copy :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Filehub (Html ())
copy sessionId _ _ = do
  Copy.select sessionId
  Copy.copy sessionId
  controlPanel sessionId


copy1 :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
copy1 sessionId _ _ mClientPath = do
  clientPath <- withQueryParam mClientPath
  clear sessionId
  Selected.setSelected sessionId (Selected clientPath [])
  Copy.select sessionId
  Copy.copy sessionId
  index sessionId


type TargetFrom  = AnyTarget
type TargetTo    = AnyTarget
type Destination = AbsPath


data PasteTask
  = PasteFile TargetFrom TargetTo FileInfo Destination
  | PasteDir TargetTo Destination [PasteTask]


paste :: SessionId -> ConfirmLogin -> ConfirmReadOnly
      -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int
                           , Header "HX-Trigger" FilehubEvent
                           ] (Html ()))
paste sessionId _ _ = do
  notifications <- Session.get sessionId (.notifications)
  pasteCounter  <- newTVarIO @_ @Integer 0
  taskId        <- newTaskId
  state         <- Copy.getCopyState sessionId
  case state of
    Paste selections -> do
      tasks <- do
        TargetView to sessionData <- Session.get sessionId (.currentTarget)
        createPasteTasks sessionData.currentDir to selections
      let taskCount = fromIntegral (length tasks)

      (void . async) do
        forConcurrently_ tasks $ fix \rec task -> do
          case task of
            PasteFile from to file dst -> do
              let fromId = Target.getTargetId from
              let toId   = Target.getTargetId to
              conduit <- Session.withTarget sessionId fromId do
                storage <- Session.get sessionId (.storage)
                storage.readStream file
              Session.withTarget sessionId toId do
                storage <- Session.get sessionId (.storage)
                storage.write $ file
                  & flip withContent (FileContentConduit conduit)
                  & #path .~ dst
              atomically do
                modifyTVar' pasteCounter (+ 1)
                n <- readTVar pasteCounter
                writeTBQueue notifications (PasteProgressed taskId (n % max 1 taskCount) )

            PasteDir to dst subTasks -> do
              let targetId = Target.getTargetId to
              Session.withTarget sessionId targetId do
                storage <- Session.get sessionId (.storage)
                storage.newFolder dst
              forConcurrently_ subTasks rec

        Copy.setCopyState sessionId NoCopyPaste
        Selected.clearSelectedAllTargets sessionId
        atomically $ writeTBQueue notifications (TaskCompleted taskId)
    _ -> do
      logAttention_ [i|[v8dsaz] #{sessionId}, not in pastable state.|]
      throwIO (FilehubError SelectError "Not in a pastable state")

  clear sessionId
  selectedCount <- length <$> Selected.allSelecteds sessionId
  addHeader selectedCount . addHeader SSEStarted <$> index sessionId

  where
    createPasteTasks fromDir to selections = fmap (mconcat . mconcat) do
      for selections \(from, files) -> do
        for files $ flip fix fromDir \rec (AbsPath currentDir) file -> do
          let name  =  coerce takeFileName file.path
          let fromId = Target.getTargetId from
          dst <- validateAbsPath (currentDir </> takeFileName name) (FilehubError InvalidPath "Invalid path")
          case file.content of
            Regular -> pure [ PasteFile from to file dst ]
            Dir -> do
              Session.withTarget sessionId fromId do
                storage <- Session.get sessionId (.storage)
                (TargetView _ (TargetSessionData { currentDir = savedDir })) <- Session.get sessionId (.currentTarget)
                storage.cd file.path
                result <- do
                  dirFiles <- storage.lsCwd
                  for dirFiles \dfile -> rec dst dfile
                storage.cd savedDir -- go back
                pure [ PasteDir to dst (mconcat result) ]


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
    mFile <- storage.get dstPath
    when (isJust mFile) do
      throwIO (FilehubError InvalidPath "The destination already exists")

  void $ async do
    atomically do
      writeTBQueue notifications (MoveProgressed taskId 0)

    storage.mv do
      fmap (\srcPath -> (srcPath, tgtPath <./> coerce takeFileName srcPath)) srcPaths

    atomically do
      writeTBQueue notifications  (TaskCompleted taskId)

  clear sessionId
  addHeader FileMoved . addHeader SSEStarted <$> index sessionId


download :: SessionId -> ConfirmLogin -> [ClientPath]
         -> Filehub (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))
download sessionId _ clientPaths = do
  root    <- Session.get sessionId (.root)
  storage <- Session.get sessionId (.storage)
  case clientPaths of
    [clientPath@(ClientPath path)] -> do
      mFile   <- storage.get (ClientPath.fromClientPath root clientPath)
      conduit <- storage.download clientPath
      case mFile of
        Just file -> do
          let filename =
                case file.content of
                  Regular -> printf "attachement; filename=%s" (takeFileName path)
                  Dir     -> printf "attachement; filename=%s.zip" (takeFileName path)
          pure $ addHeader filename conduit
        Nothing -> do
          throwIO (FilehubError InvalidPath "can't download, invalid file path")
    _ -> do
      (zipPath, _) <- liftIO do
        tempDir <- Temp.getCanonicalTemporaryDirectory
        Temp.openTempFile tempDir "DXXXXXX.zip"

      files <- traverse (storage.get . ClientPath.fromClientPath root) clientPaths <&> catMaybes

      tasks <- for files \file -> do
        conduit <- storage.readStream file
        pure (file.path, conduit)

      Zip.createArchive zipPath do
        for_ tasks \(path, conduit) -> do
          m <- Zip.mkEntrySelector  (coerce makeRelative root path)
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
      writeTBQueue notifications (UploadProgressed taskId 0)

    storage <- Session.get sessionId (.storage)
    forConcurrently_ multipart.files \filedata -> do
      storage.upload filedata
      atomically do
        modifyTVar' uploadCounter (+ 1)
        n <- readTVar uploadCounter
        writeTBQueue notifications (UploadProgressed taskId (n % max 1 taskCount) )

    atomically do
      writeTBQueue notifications (UploadProgressed taskId 1)
      writeTBQueue notifications (TaskCompleted taskId)
  addHeader SSEStarted <$> index sessionId



serve :: Env -> SessionId -> ConfirmLogin -> Tagged Filehub Application
serve env sessionId _ = Tagged $ \req respond -> do
  let query = queryString req
  let mFile = join $ lookup "file" query

  res <- runFilehub env $ do
    root       <- Session.get sessionId (.root)
    storage    <- Session.get sessionId (.storage)
    clientPath <- do
      text <- Text.decodeUtf8 <$> withQueryParam mFile
      case parseUrlPiece @ClientPath text of
        Right c  -> pure c
        Left err -> throwIO do HTTPError err404 { errBody = [i|#{err}|] }

    let path = ClientPath.fromClientPath root clientPath

    storage.get path >>= \case
      Just file -> do
        stream <- storage.readStream file
        pure (Just (file, stream))
      Nothing   -> pure Nothing

  case res of
    Left err -> throwIO err

    Right Nothing -> respond $ responseLBS status404 [] "File not found"

    Right (Just (fileInfo, stream)) -> do
      let streamResponse =
            \status headers -> responseStream status headers $ \send flush ->
              runResourceT . runConduit
              $ stream .| Conduit.mapM_C \chunk -> liftIO do send (Builder.fromByteString chunk)
                                                             flush

      let settings = (defaultFileServerSettings "")
                      { ssLookupFile = \_ -> do
                          let absPath = coerce fileInfo.path :: FilePath
                          pure $ LRFile WaiAppStatic.Types.File
                            { fileGetSize     = maybe 0 id fileInfo.size
                            , fileToResponse  = streamResponse
                            , fileName        = unsafeToPiece (Text.pack (takeFileName absPath))
                            , fileGetHash     = pure Nothing
                            , fileGetModified = (CTime . round . utcTimeToPOSIXSeconds) <$> fileInfo.mtime
                            }
                      , ssGetMimeType  = \_file -> pure fileInfo.mimetype
                      , ssIndices = []
                      }
      staticApp settings req respond
