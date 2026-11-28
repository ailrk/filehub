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
  , cut
  , cut1
  , paste
  , move
  , download
  , upload
  , serve
  ) where

import Filehub.Prelude
import Data.Zip qualified as Zip
import Conduit (ConduitT, ResourceT, runResourceT, runConduit, (.|), sinkList)
import Conduit qualified
import Control.Monad (replicateM)
import Control.Monad.Reader (MonadReader(..))
import Data.Binary.Builder qualified as BB
import Data.ClientPath (ClientPath (..), AbsPath (..), (<./>), Root (..), validateAbsPath, ClientPathView(..), asClientPathView)
import Data.ClientPath qualified as ClientPath
import Data.File (FileType(..), File(..), FileContent (..), defaultFileWithContent, FileInfo, IsLink (..))
import Data.Ratio ((%))
import Filehub.Error ( FilehubError(..), Error' (..) )
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly, ConfirmRateLimitUpload(..), ConfirmRateLimitDownload(..))
import Filehub.Monad
import Filehub.Notification.Types (Notification(..), newTaskId)
import Filehub.Orphan ()
import Filehub.Server.File.Paste (paste)
import Filehub.Server.File.Delete (delete)
import Filehub.Server.UI qualified as UI
import Filehub.Server.Util (withQueryParam)
import Filehub.History (Operation(..), withHistory, recordHistory)
import Network.URI.Encode qualified as URI.Encode
import Target.Types qualified
import Filehub.Session (SessionId(..), Session(..), Storage(..), getRoot, TargetView (..), getCurrentTarget, makeStorageCurrentTarget, makeStorageForTarget, targetRoot, fromClientPathChecked, CopyState(..), withSession, TargetSessionData(..))
import Target.Types (TargetId)
import Filehub.Session.Copy qualified as Copy
import Filehub.Sort qualified as Sort
import Filehub.Types ( NewFile(..) , NewFolder(..), UpdatedFile(..) , UpdatedFile(..) , FilehubEvent (..), RenameFile (..), MoveFile (..), Env(..))
import Lucid hiding (for_)
import Network.HTTP.Types (ByteRange(..), parseByteRanges)
import Network.HTTP.Types.Status (status404, status206, status200)
import Network.Wai (Request(..), responseLBS, responseStream)
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader, Tagged (..), Application, ServerError (..), FromHttpApiData (..), err404)
import Servant.Multipart (MultipartData(..), Mem, FileData(..))
import System.Directory (removeFile, getTemporaryDirectory, canonicalizePath)
import System.FilePath (takeFileName, (</>), makeRelative, takeDirectory)
import System.IO (openTempFile)
import System.Random (randomRIO)
import Text.Printf (printf)
import UnliftIO (newEmptyMVar, takeMVar, putMVar)
import UnliftIO.STM (writeTBQueue)
import Filehub.Server.UI.Render (runUIPartialUpdate, UIPartialUpdate (..))
import Log (logAttention_)


cd :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub (Headers '[ Header "HX-Trigger-After-Swap" FilehubEvent ] (Html ()))
cd sessionId _ mClientPath = do
  root       <- withSession sessionId . getRoot =<< ask
  storage    <- makeStorageCurrentTarget sessionId
  clientPath <- withQueryParam mClientPath
  path <- fromClientPathChecked root clientPath
  storage.cd path
  addHeader DirChanged <$> runUIPartialUpdate sessionId
    [ UpdateToolBar
    , UpdateView
    ]


rename :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> RenameFile
  -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
rename sessionId _ _ (RenameFile old new) = do
  env     <- ask
  storage <- makeStorageCurrentTarget sessionId
  root    <- withSession sessionId . getRoot $ env
  targetId <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    pure (Target.Types.getTargetId t)
  let newPath = ClientPath . URI.Encode.encode $ new
  withHistory sessionId (OpRename targetId old newPath) do
    oldPath <- fromClientPathChecked root old
    storage.rename oldPath new
  addHeader FileRenamed <$> runUIPartialUpdate sessionId [ UpdateView ]


updateFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> UpdatedFile -> Filehub (Html ())
updateFile sessionId _ _ (UpdatedFile clientPath content) = do
  storage <- makeStorageCurrentTarget sessionId
  root    <- withSession sessionId . getRoot =<< ask
  path  <- fromClientPathChecked root clientPath
  storage.write $ defaultFileWithContent
    { path     = path
    , content  = FileContentRaw (txt_encode content)
    }
  UI.view sessionId


newFile' :: SessionId -> Text -> (AbsPath -> Filehub FileInfo) -> Filehub (Html ())
newFile' sessionId name create = do
  env     <- ask
  storage <- makeStorageCurrentTarget sessionId

  (dir, order, root) <- withSession sessionId \s -> do
    TargetView _ td
         <- getCurrentTarget env s
    root  <- getRoot env s
    dir   <- readTVar td.currentDir
    order <- readTVar td.sortedFileBy

    pure ( dir, order, root)

  let nameStr = txt_unpack name
  when (null nameStr || '/' `elem` nameStr || '\0' `elem` nameStr || nameStr == "..") do
    throwIO (FilehubError InvalidPath "Invalid filename")
  path    <- validateAbsPath (coerce dir </> nameStr) (FilehubError InvalidPath ("<redacted>/" <> show name))
  already <- storage.doesExist path
  when already do
    throwIO (FilehubError FileExists ("Already exists: " <> show name))
  file    <- create path
  files   <- Sort.sortFiles order <$> storage.ls dir

  let updates = case getPrev file files of
        Just prevFile ->
          let ClientPathView { hashPath } = asClientPathView root prevFile.path
           in [ UpdateEntry [i|afterend:\#tr-#{hashPath}|] file ]
        Nothing -> [ UpdateView ]

  runUIPartialUpdate sessionId updates

  where
    getPrev target list =
      case break (== target) list of
          (before, _) | not (null before) -> Just (last before)
          _                               -> Nothing


newFile :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFile -> Filehub (Html ())
newFile sessionId _ _ (NewFile name) = do
  env     <- ask
  storage <- makeStorageCurrentTarget sessionId
  (targetId, root, dir) <- withSession sessionId \s -> do
    TargetView t td <- getCurrentTarget env s
    root            <- getRoot env s
    dir             <- readTVar td.currentDir
    pure (Target.Types.getTargetId t, root, dir)
  let clientPath = ClientPath.toClientPath root (AbsPath (coerce dir </> txt_unpack name))
  withHistory sessionId (OpNewFile targetId clientPath) do
    newFile' sessionId name storage.new


newFolder :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> NewFolder -> Filehub (Html ())
newFolder sessionId _ _ (NewFolder name) = do
  env     <- ask
  storage <- makeStorageCurrentTarget sessionId
  (targetId, root, dir) <- withSession sessionId \s -> do
    TargetView t td <- getCurrentTarget env s
    root            <- getRoot env s
    dir             <- readTVar td.currentDir
    pure (Target.Types.getTargetId t, root, dir)
  let clientPath = ClientPath.toClientPath root (AbsPath (coerce dir </> txt_unpack name))
  withHistory sessionId (OpNewFolder targetId clientPath) do
    newFile' sessionId name storage.newFolder


copy :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Filehub (Html ())
copy sessionId _ _ clientPaths = do
  env <- ask
  Copy.selectFiles sessionId clientPaths
  (targetId, count) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    let n = case s.copyState of
              Paste sels -> sum (map (length . snd) sels)
              _          -> 0
    pure (Target.Types.getTargetId t, n)
  recordHistory sessionId (OpCopy targetId count) True
  UI.controlPanel sessionId


copy1 :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
copy1 sessionId _ _ mClientPath = do
  env        <- ask
  clientPath <- withQueryParam mClientPath
  Copy.selectFiles sessionId [clientPath]
  (targetId, count) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    let n = case s.copyState of
              Paste sels -> sum (map (length . snd) sels)
              _          -> 0
    pure (Target.Types.getTargetId t, n)
  recordHistory sessionId (OpCopy targetId count) True
  UI.controlPanel sessionId


cut :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> [ClientPath] -> Filehub (Html ())
cut sessionId _ _ clientPaths = do
  env <- ask
  Copy.cutFiles sessionId clientPaths
  (targetId, count) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    let n = case s.copyState of
              Cut sels -> sum (map (length . snd) sels)
              _        -> 0
    pure (Target.Types.getTargetId t, n)
  recordHistory sessionId (OpCopy targetId count) True
  UI.controlPanel sessionId


cut1 :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
cut1 sessionId _ _ mClientPath = do
  env        <- ask
  clientPath <- withQueryParam mClientPath
  Copy.cutFiles sessionId [clientPath]
  (targetId, count) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    let n = case s.copyState of
              Cut sels -> sum (map (length . snd) sels)
              _        -> 0
    pure (Target.Types.getTargetId t, n)
  recordHistory sessionId (OpCopy targetId count) True
  UI.controlPanel sessionId


move :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> MoveFile
     -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
move sessionId _ _ (MoveFile src tgt) = do
  env     <- ask
  storage <- makeStorageCurrentTarget sessionId

  (notifications, root, targetId) <- withSession sessionId \s -> do
    TargetView t _ <- getCurrentTarget env s
    root           <- getRoot env s
    pure ( s.notifications
         , root
         , Target.Types.getTargetId t
         )

  taskId <- newTaskId
  withSession sessionId \s -> modifyTVar' s.pendingTasks (set_insert taskId)
  lk     <- newEmptyMVar

  srcPaths <- traverse (fromClientPathChecked root) src
  tgtPath  <- fromClientPathChecked root tgt

  -- check before take action
  checkedSrcPaths <- catMaybes <$> for srcPaths \srcPath -> do
    isTgtDir <- storage.isDirectory tgtPath
    when (not isTgtDir) do
      throwIO (FilehubError InvalidDir "Target is not a directory")

    when (srcPath == tgtPath)  do
      throwIO (FilehubError InvalidDir "Can't move to the same directory")

    let srcStr = coerce srcPath :: FilePath
        tgtStr = coerce tgtPath :: FilePath

    if | (srcStr ++ "/") `isPrefixOf` (tgtStr ++ "/") -> do
           logAttention_ [i|[k4zm9x] move: circular move detected, skipping #{show srcPath}|]
           pure Nothing
       | coerce takeDirectory srcPath == tgtPath -> do
           logAttention_ [i|[k4zm9x] move: skipping #{show srcPath}, already in target directory|]
           pure Nothing
       | otherwise -> do
           let
               fileName = coerce takeFileName srcPath
               dstPath = tgtPath <./> fileName

           exists <- storage.doesExist dstPath
           if exists
             then do
               logAttention_ [i|[k4zm9x] move: skipping #{show srcPath}, name conflict at #{show dstPath}|]
               pure Nothing
             else pure $ Just srcPath

  let handleMoveFailed =
        atomically $ writeTBQueue notifications TaskFailed { taskId, htmxResponse = Nothing }

  forkFilehub_ env $ do
    _ <- takeMVar lk
    flip onException handleMoveFailed do
      unless (null checkedSrcPaths) do
        storage.mv
          let
              merge srcPath =
                let
                    fileName = coerce takeFileName srcPath
                 in
                    tgtPath <./> fileName

              check a = (a, merge a)
           in
              fmap check checkedSrcPaths

      recordHistory sessionId (OpMove targetId src tgt) True

      htmx <- runUIPartialUpdate sessionId [UpdateView]

      atomically do
        writeTBQueue notifications TaskCompleted
          { taskId       = taskId
          , htmxResponse = Just htmx
          }

  UI.clear sessionId

  htmx <- runUIPartialUpdate sessionId
    [ UpdateSideBar
    , UpdateControlPanel
    ]

  putMVar lk ()

  addHeader FileMoved <$> pure htmx


download :: SessionId -> ConfirmLogin -> ConfirmRateLimitDownload -> [ClientPath]
         -> Filehub (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))
download sessionId _ _ clientPaths = do
  env     <- ask
  storage <- makeStorageCurrentTarget sessionId
  root    <- withSession sessionId (getRoot env)

  case clientPaths of
    [clientPath@(ClientPath path)] -> do
      absPath <- fromClientPathChecked root clientPath
      file    <- storage.get absPath

      case file.isLink of
        BrokenLink -> throwIO (FilehubError InvalidPath "Can't download a broken link")
        _          -> pure ()

      conduit <- storage.download clientPath

      let filename = case file.content of
                       Dir -> printf "attachment; filename=%s.zip" (takeFileName path)
                       _   -> printf "attachment; filename=%s" (takeFileName path)

      pure $ addHeader filename conduit

    _ -> do
      (zipPath, _) <- liftIO do
        tempDir <- getTemporaryDirectory >>= canonicalizePath
        openTempFile tempDir "DXXXXXX.zip"

      absPaths <- traverse (fromClientPathChecked root) clientPaths
      files <- traverse storage.get absPaths

      tasks <- for files \file -> do
        conduit <- storage.readStream file Nothing Nothing
        pure (file.path, conduit)

      entries <- for tasks \(path, conduit) -> do
        chunks <- liftIO . runResourceT . runConduit $ conduit .| sinkList
        pure Zip.Entry { entryName = coerce makeRelative root path, entryBytes = mconcat chunks }
      liftIO $ Zip.writeZip zipPath entries

      tag <- txt_pack <$> replicateM 8 (randomRIO ('a', 'z'))

      let conduit =
            Conduit.bracketP
              (pure ())
              (\_ -> removeFile zipPath)
              (\_ -> Conduit.sourceFile zipPath)
      pure $ addHeader (printf "attachement; filename=%s.zip" tag) conduit


upload :: SessionId -> ConfirmLogin -> ConfirmReadOnly -> ConfirmRateLimitUpload -> MultipartData Mem
       -> Filehub (Html ())
upload sessionId _ _ _ multipart = do
  env           <- ask
  taskId        <- newTaskId
  withSession sessionId \s -> modifyTVar' s.pendingTasks (set_insert taskId)
  notifications <- withSession sessionId (pure . (.notifications))
  (targetId, root, dir) <- withSession sessionId \s -> do
    TargetView t td <- getCurrentTarget env s
    root            <- getRoot env s
    dir             <- readTVar td.currentDir
    pure (Target.Types.getTargetId t, root, dir)
  let taskCount =  fromIntegral $ length multipart.files

  uploadCounter <- newTVarIO @_ @Integer 0
  lk            <- newEmptyMVar

  forkFilehub_ env do
    _ <- takeMVar lk
    atomically do
      writeTBQueue notifications UploadProgressed
        { taskId       = taskId
        , progress     = 0
        , htmxResponse = Nothing
        }

    storage   <- makeStorageCurrentTarget sessionId
    forConcurrently_ multipart.files \filedata -> do
      storage.upload filedata
      let clientPath = ClientPath.toClientPath root (AbsPath (coerce dir </> txt_unpack filedata.fdFileName))
      recordHistory sessionId (OpUpload targetId clientPath) True
      atomically do
        modifyTVar' uploadCounter (+ 1)
        n <- readTVar uploadCounter
        writeTBQueue notifications UploadProgressed
          { taskId       = taskId
          , progress     = n % max 1 taskCount
          , htmxResponse = Nothing
          }

    htmx <- runUIPartialUpdate sessionId [UpdateView, UpdateSideBar]

    atomically do
      writeTBQueue notifications UploadProgressed
        { taskId       = taskId
        , progress     = 1
        , htmxResponse = Nothing
        }
      writeTBQueue notifications TaskCompleted
        { taskId       = taskId
        , htmxResponse = Just htmx
        }

  htmx <- UI.index sessionId
  putMVar lk ()

  pure htmx


serve :: Env -> SessionId -> ConfirmLogin -> ConfirmRateLimitDownload -> Tagged Filehub Application
serve env sessionId _ _ = Tagged $ \req respond -> do
  let query    = queryString req
  let mFile    = join $ lookup "file"   query
  let mTargetQ = join $ lookup "target" query

  -- Lookup the file
  res <- runFilehub env do
    -- Optionally serve from a specific target (bypasses session's current target)
    (root, storage) <- case mTargetQ >>= either (const Nothing) Just . parseUrlPiece @TargetId . txt_decode of
      Just tid -> do
        targets <- readTVarIO env.targets
        case lookup tid targets of
          Just target -> do
            s <- makeStorageForTarget sessionId target
            pure (targetRoot target, s)
          Nothing -> do
            r <- withSession sessionId (getRoot env)
            s <- makeStorageCurrentTarget sessionId
            pure (r, s)
      Nothing -> do
        r <- withSession sessionId (getRoot env)
        s <- makeStorageCurrentTarget sessionId
        pure (r, s)

    clientPath <- do
      text <- txt_decode <$> withQueryParam mFile
      case parseUrlPiece @ClientPath text of
        Right c  -> pure c
        Left err -> throwIO do HTTPError err404 { errBody = [i|#{err}|] }

    path <- fromClientPathChecked root clientPath

    file <- storage.get path
    pure (Just (file, storage))

  case res of
    Left err                         -> throwIO err
    Right Nothing                    -> respond $ responseLBS status404 [] "File not found"
    Right (Just (fileInfo, storage)) -> do
      let fileSize = fromMaybe 0 fileInfo.size

          mReqRange = lookup "Range" (requestHeaders req) >>= parseByteRanges

          -- We need handle the Range header to support video seek.
          (status, mOff, mLen) = case mReqRange of
                                   Just (ByteRangeFromTo s e : _) -> (status206, Just s, Just (e - s + 1))
                                   Just (ByteRangeFrom s : _)     -> (status206, Just s, Just (fileSize - s))
                                   _                              -> (status200, Nothing, Nothing)

          renderRangeHeader s e t = bs_packChars $ printf "bytes %d-%d/%d" s e t

          from = fromMaybe 0 mOff
          len  = fromMaybe 0 mLen
          to   = from + len - 1

          headers = [ ("Content-Type", fileInfo.mimetype)
                    , ("Accept-Ranges", "bytes")
                    ]
                ++ if status == status206
                      then [ ("Content-Range", renderRangeHeader from  to fileSize)
                           , ("Content-Length", bs_packChars (show len))
                           ]
                      else [ ("Content-Length", bs_packChars (show fileSize)) ]

      respond do
        responseStream status headers $ \send flush -> do

          mStream <- runFilehub env do
            storage.readStream fileInfo mOff mLen

          case mStream of
            Left err     -> throwIO do HTTPError err404 { errBody = [i|#{err}|] }
            Right stream ->
              runResourceT . runConduit
                $ stream
                .| Conduit.mapM_C \chunk -> liftIO do send (BB.fromByteString chunk); flush
