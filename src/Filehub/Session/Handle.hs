{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Handle
  ( getCurrentTarget
  , getTarget
  , getDisplay
  , getRoot
  , getTargetViews
  , getControlPanelState
  , setCurrentTarget
  , modifyCurrentTarget
  , makeStorageDummy
  , makeStorage
  , makeStorageDyn
  , withTarget
  )
  where

import Conduit (yield)
import Control.Applicative (asum)
import Control.Concurrent.STM (throwSTM)
import Control.Handle.Storage (Storage(..))
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader (..))
import Data.ClientPath (AbsPath (..), Root (..))
import Data.ClientPath (fromClientPath)
import Data.Coerce (coerce)
import Data.File (File (..), FileWithContent, FileContent (..), extractFileInfo)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (cast)
import Filehub.Display qualified as Display
import Filehub.Error (Error' (..))
import Filehub.Error (FilehubError(..))
import Filehub.Monad (Filehub)
import Filehub.Session.Types (TargetView(..), SessionId, Session, TargetSessionData(..), Session(..), ControlPanelState (..), CopyState (..))
import Filehub.Storage.File qualified as File
import Filehub.Storage.S3 qualified as S3
import Filehub.Types (Display (..), Env(..))
import Filehub.UserAgent qualified as UserAgent
import Log (logAttention)
import Prelude hiding (read, readFile, writeFile)
import Target.File (Target(..), FileSys)
import Target.S3 (S3)
import Target.Types (handleTarget, targetHandler, AnyTarget (..), HasTargetId (..), TargetId)
import UnliftIO (throwIO, finally, writeTVar, STM, readTVar, TVar, atomically, newTVar)
import UnliftIO.Directory (doesDirectoryExist)
import Filehub.Session.Pool (withSession, modifySession)
import Filehub.Session.Selected (AllSelected (..))


getTarget :: HasTargetId t => Env -> Session -> t -> STM TargetView
getTarget env s t = do
  targets  <- readTVar env.targets

  let
      targetId = getTargetId t

      mTargetView = do
        targetSessionData <- M.lookup targetId s.targets
        target            <- lookup targetId targets
        pure (TargetView target targetSessionData)

   in
      case mTargetView of
        Just tv -> pure tv
        Nothing -> throwSTM (FilehubError InvalidSession "Invalid session")


getCurrentTarget :: Env -> Session -> STM TargetView
getCurrentTarget env s = do
  targetId <- readTVar s.currentTargetId
  getTarget env s targetId


getDisplay :: Session -> STM Display
getDisplay s = do
  case s.resolution of
    Just resolution ->
      case s.deviceType of
        UserAgent.Desktop -> pure $ Desktop
        UserAgent.Mobile  -> pure $ Display.classify resolution
        UserAgent.Tablet  -> pure $ Display.classify resolution
        UserAgent.Bot     -> pure $ Display.classify resolution
        UserAgent.Unknown -> pure $ Display.classify resolution
    Nothing -> pure NoDisplay


getRoot :: Env -> Session -> STM Root
getRoot env s = do
  (TargetView (AnyTarget tgt) _) <- getCurrentTarget env s
  fromMaybe (pure $ Root (AbsPath "")) . asum $
    [ cast tgt <&> \(x :: Target FileSys) -> pure x.root
    , cast tgt <&> \(_ :: Target S3) -> pure (Root (AbsPath ""))
    ]


getControlPanelState :: AllSelected -> Session -> STM ControlPanelState
getControlPanelState AllSelected { count } s = do
  case s.copyState of
    Paste {}                    -> pure ControlPanelCopied
    CopySelected {} | count > 0 -> pure ControlPanelSelecting
    NoCopyPaste {} | count > 0  -> pure ControlPanelSelecting
    _                           -> pure ControlPanelDefault


getTargetViews :: Env -> Session -> STM [TargetView]
getTargetViews env s = do
  targets <- readTVar env.targets
  let tvs = flip mapMaybe targets \(targetId, target) ->
              case M.lookup targetId s.targets of
                Just targetData -> pure (TargetView target targetData)
                Nothing         -> Nothing
  pure tvs


setCurrentTarget :: Env -> Session -> TargetId -> STM ()
setCurrentTarget env s tid = do
  TargetView target _ <- getCurrentTarget env s
  targets             <- readTVar env.targets
  if getTargetId target == tid
     then pure ()
     else do
       case lookup tid targets of
         Just _ -> writeTVar s.currentTargetId tid
         Nothing -> do
           throwSTM (FilehubError InvalidSession "Invalid session")


modifyCurrentTarget :: SessionId -> (TargetSessionData -> TargetSessionData) -> Filehub ()
modifyCurrentTarget sessionId f = do
  env <- ask
  modifySession sessionId \s -> do
    TargetView target _ <- getCurrentTarget env s
    let targetId = getTargetId target
    pure $ (s { targets =  M.adjust f targetId s.targets } :: Session)


makeStorageDummy :: [(AbsPath, FileWithContent)] -> Storage Filehub
makeStorageDummy mockFS =
  Storage
    { get = \path -> let
                         mRes = lookup path mockFS
                      in
                         case mRes of
                           Just res -> pure do extractFileInfo res
                           Nothing  -> error "storge dummy: get"

    , read = \file ->
        case lookup file.path mockFS of
          Just (File { content = FileContentRaw bytes }) -> pure bytes
          _                                              -> error "storage dummy: read"

    , readStream = \file _ _ ->
        case lookup file.path mockFS of
          Just (File { content = FileContentRaw bytes }) -> pure (yield bytes)
          _                                              -> error "storage dummy: readStream"

    , ls = \case
        AbsPath "/" -> pure $ fmap (extractFileInfo . snd) mockFS
        path        -> case lookup path mockFS of
                         Just (File { content = FileContentDir dir }) -> pure $ fmap extractFileInfo dir
                         _                                            -> pure []

    , cd = \_ -> pure ()

    , isDirectory = \path ->
        case lookup path mockFS of
          Just (File { content = FileContentDir _ }) -> pure True
          _                                          -> pure False

    , write       = error "not implemented"
    , mv          = error "not implemented"
    , rename      = error "not implemented"
    , delete      = error "not implemented"
    , new         = error "not implemented"
    , newFolder   = error "not implemented"
    , lsCwd       = error "not implemented"
    , upload      = error "not implemented"
    , download    = error "not implemented"
    }


makeStorageS3 :: AnyTarget -> Storage Filehub
makeStorageS3 target =
  Storage
    { get = \path -> do
        s3 <- getS3
        S3.get s3 path

    , read = \file -> do
        s3 <- getS3
        S3.read s3 file

    , readStream = \file mOff mMax -> do
        s3 <- getS3
        S3.readStream s3 file mOff mMax

    , write = \fileWithContent -> do
        s3 <- getS3
        S3.write s3 fileWithContent

    , mv = \mvPairs -> do
        s3 <- getS3
        S3.mv s3 mvPairs

    , rename = \o n -> do
        s3 <- getS3
        S3.rename s3 o n

    , delete = \filePath -> do
        s3 <- getS3
        S3.delete s3 filePath

    , new = \filePath -> do
        s3 <- getS3
        S3.new s3 filePath

    , newFolder = \_ -> pure (error "not supported")

    , ls = \filePath -> do
        s3 <- getS3
        S3.ls s3 filePath

    , cd = \_ -> pure ()

    , lsCwd = do
        s3 <- getS3
        S3.lsCwd s3

    , upload = \filedata -> do
        s3 <- getS3
        S3.upload s3 filedata

    , download = \clientPath -> do
        s3 <- getS3
        S3.download s3 (fromClientPath (Root (AbsPath "")) clientPath)
    , isDirectory = \filePath -> do
        s3 <- getS3
        S3.isDirectory s3 filePath
    }
  where
    getS3 :: Filehub (Target S3)
    getS3 = do
      case handleTarget target [ targetHandler @S3 id ] of
        Just r  -> pure r
        Nothing -> throwIO (FilehubError TargetError "Target is not valid file system direcotry")


makeStorageFileSys :: AnyTarget -> TVar AbsPath -> Storage Filehub
makeStorageFileSys target currentDir =
  Storage
    { get         = File.get
    , read        = File.read
    , readStream  = File.readStream
    , ls          = File.ls
    , cd          = \dir -> do
                      exists <- doesDirectoryExist (coerce dir)
                      unless exists do
                        logAttention "[nmb224] dir doesn't exists:" dir
                        throwIO (FilehubError InvalidDir "Can't enter, not a directory")
                      atomically do writeTVar currentDir dir
    , isDirectory = File.isDirectory
    , write       = File.write
    , mv          = File.mv
    , rename      = File.rename
    , delete      = File.delete
    , new         = File.new
    , newFolder   = File.newFolder
    , lsCwd       = atomically (readTVar currentDir) >>= File.lsCwd
    , upload      = \filedata -> atomically (readTVar currentDir) >>= flip File.upload filedata
    , download    = \clientPath -> getFileSys >>= flip File.download clientPath
    }
  where
    getFileSys :: Filehub (Target FileSys)
    getFileSys = do
      case handleTarget target [ targetHandler @FileSys id ] of
        Just r  -> pure r
        Nothing -> throwIO (FilehubError TargetError "Target is not valid file system direcotry")


makeStorage' :: AnyTarget -> TVar AbsPath -> Filehub (Storage Filehub)
makeStorage' target currentDir = do
  let s3Storage   = makeStorageS3 target
      fileStorage = makeStorageFileSys target currentDir
      onError     = do
        throwIO (FilehubError TargetError "Invalid target")

  fromMaybe onError $ handleTarget target
    [ targetHandler @FileSys \_ -> pure fileStorage
    , targetHandler @S3      \_ -> pure s3Storage
    ]


makeStorage :: TargetView -> Filehub (Storage Filehub)
makeStorage (TargetView target td) = do
  currentDir <- atomically do
    cd <- readTVar td.currentDir
    newTVar cd
  makeStorage' target currentDir


makeStorageDyn :: SessionId -> Filehub (Storage Filehub)
makeStorageDyn sessionId = do
  env <- ask
  TargetView target td <- withSession sessionId \s -> do
    getCurrentTarget env s
  makeStorage' target td.currentDir


withTarget :: HasTargetId t => SessionId -> t -> Filehub a -> Filehub a
withTarget sid t action = do
  env <- ask
  oldTid <- withSession sid \s -> do
    setCurrentTarget env s (getTargetId t)
    readTVar s.currentTargetId

  let cleanup = withSession sid \s -> do
        setCurrentTarget env s oldTid

  action `finally` cleanup
