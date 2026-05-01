module Filehub.Session.Handle where

import Conduit (yield)
import Control.Applicative (asum)
import Control.Handle.Storage (Storage(..))
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Data.ClientPath (AbsPath (..), Root (..))
import Data.ClientPath (fromClientPath)
import Data.Coerce (coerce)
import Data.File (File (..), FileWithContent, FileContent (..), extractFileInfo)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Typeable (cast)
import Filehub.Display qualified as Display
import Filehub.Error (Error' (..))
import Filehub.Error (FilehubError(..))
import Filehub.Monad (Filehub)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (TargetView(..), SessionGet (..), SessionSet (..), SessionId, Session, TargetSessionData(..), Session(..), CopyState (..), ControlPanelState (..))
import Filehub.UserAgent qualified as UserAgent
import Lens.Micro.Platform ()
import Log (logAttention_, logTrace, logAttention)
import Prelude hiding (read, readFile, writeFile)
import Storage.File qualified as File
import Storage.S3 qualified as S3
import Target.File (Target(..), FileSys)
import Target.S3 (S3)
import Target.Types (handleTarget, targetHandler, AnyTarget (..), HasTargetId (..))
import UnliftIO (throwIO)
import UnliftIO.Directory (doesDirectoryExist)
import UnliftIO.STM (readTVarIO)
import Filehub.Types (Display (..), Env(..))
import Filehub.Session.Copy qualified as Copy


get :: SessionId -> (SessionGet Filehub -> Filehub a) -> Filehub a
get sessionId field = do
  let viewRecord = newSessionGet sessionId
  field viewRecord


set :: SessionId -> (SessionSet Filehub -> val -> Filehub ()) -> val -> Filehub ()
set sessionId field val = do
  let setRecord = newSessionSet sessionId
  field setRecord val


newSessionGet :: SessionId -> SessionGet Filehub
newSessionGet sessionId =
  let display = do
        s <- Session.Pool.get sessionId
        case s.resolution of
          Just resolution ->
            case s.deviceType of
              UserAgent.Desktop -> pure $ Desktop
              UserAgent.Mobile  -> pure $ Display.classify resolution
              UserAgent.Tablet  -> pure $ Display.classify resolution
              UserAgent.Bot     -> pure $ Display.classify resolution
              UserAgent.Unknown -> pure $ Display.classify resolution
          Nothing -> pure NoDisplay


      currentDir = do
        (TargetView _ td) <- currentTarget
        pure td.currentDir


      sortedFileBy = do
        (TargetView _ td) <- currentTarget
        pure td.sortedFileBy


      selected = do
        (TargetView _ td) <- currentTarget
        pure td.selected


      root = do
        (TargetView (AnyTarget tgt) _) <- currentTarget
        fromMaybe (pure $ Root (AbsPath "")) . asum $
          [ cast tgt <&> \(x :: Target FileSys) -> pure x.root
          , cast tgt <&> \(_ :: Target S3) -> pure (Root (AbsPath ""))
          ]


      targetViews = do
        s <- Session.Pool.get sessionId
        let targetIds =  Map.keys s.targets
        targets       <- filter ((`elem` targetIds) . fst) <$> (asks (.targets) >>= readTVarIO)
        pure $
          flip mapMaybe targets \(targetId, target) ->
            case Map.lookup targetId s.targets of
              Just targetData -> pure (TargetView target targetData)
              Nothing         -> Nothing


      controlPanelState = do
        isAnySelected <- Selected.anySelected sessionId
        copyState     <- Copy.getCopyState sessionId
        case (isAnySelected, copyState) of
          (_, Paste {})           -> pure ControlPanelCopied
          (True, CopySelected {}) -> pure ControlPanelSelecting
          (True, NoCopyPaste)     -> pure ControlPanelSelecting
          _                       -> pure ControlPanelDefault


      storage = do
        (TargetView t _) <- currentTarget
        let s3Storage   = makeStorageS3 sessionId
            fileStorage = makeStorageFileSys sessionId
            onError     = do
              logAttention_ "[ssshuu] Target error"
              throwIO (FilehubError TargetError "Invalid target")


        fromMaybe onError $ handleTarget t
          [ targetHandler @FileSys \_ -> pure fileStorage
          , targetHandler @S3      \_ -> pure s3Storage
          ]


      currentTarget :: Filehub TargetView
      currentTarget = do
        s <- Session.Pool.get sessionId
        targets <- asks (.targets) >>= readTVarIO
        maybe (throwIO (FilehubError InvalidSession "Invalid session")) pure do
          let targetId      = s.currentTargetId
          targetSessionData <- Map.lookup targetId s.targets
          target            <- lookup targetId targets
          pure $ TargetView target targetSessionData

      g = Session.Pool.get sessionId

    in SessionGet
      { currentDir        = currentDir
      , root              = root
      , display           = display
      , sortedFileBy      = sortedFileBy
      , selected          = selected
      , authId            = g <&> (.authId)
      , sidebarCollapsed  = g <&> (.sidebarCollapsed)
      , layout            = g <&> (.layout)
      , theme             = g <&> (.theme)
      , locale            = g <&> (.locale)
      , targetViews       = targetViews
      , controlPanelState = controlPanelState
      , sharedLinkPermit  = g <&> (.sharedLinkPermit)
      , currentTarget     = currentTarget
      , oidcFlow          = g <&> (.oidcFlow)
      , notifications     = g <&> (.notifications)
      , pendingTasks      = g <&> (.pendingTasks)
      , storage           = storage
      }


newSessionSet :: SessionId -> SessionSet Filehub
newSessionSet sessionId =
  let upS :: (Session -> Session) -> Filehub ()
      upS f = Session.Pool.update sessionId f

      upT :: (TargetSessionData -> TargetSessionData) -> Filehub ()
      upT f = upS $ \s -> s { targets = Map.adjust f s.currentTargetId s.targets }

      currentDir a = upT (\td -> td { currentDir = a })

      sortedFileBy a = upT (\td -> td { sortedFileBy = a })

      selected a = upT (\td -> td { selected = a })

      authId a = upS (\s -> s { authId = a })

      sidebarCollapsed a = upS (\s -> s { sidebarCollapsed = a })

      layout a = upS (\s -> s { layout = a })

      theme a = upS (\s -> s { theme = a })

      locale a = upS (\s -> s { locale = a })

      sharedLinkPermit a = upS (\s -> s { sharedLinkPermit = a })

      notifications a = upS (\s -> s { notifications = a })

      oidcFlow a = upS (\s -> s { oidcFlow = a })

      pendingTasks a = upS (\s -> s { pendingTasks = a })

      currentTarget tid = do
          TargetView target _ <- get sessionId (.currentTarget)
          targets <- asks (.targets) >>= readTVarIO
          if getTargetId target == tid
             then pure ()
             else do
               case lookup tid targets of
                 Just _ -> do
                   logTrace "[vccxxa] Changing target" (show tid)
                   upS (\s -> s { currentTargetId = tid })
                 Nothing -> do
                   logAttention "[vccxxa] Can't change to target" (show tid)
                   throwIO (FilehubError InvalidSession "Invalid session")

   in
    SessionSet
      { currentDir        = currentDir
      , sortedFileBy      = sortedFileBy
      , selected          = selected
      , authId            = authId
      , sidebarCollapsed  = sidebarCollapsed
      , layout            = layout
      , theme             = theme
      , locale            = locale
      , sharedLinkPermit  = sharedLinkPermit
      , notifications     = notifications
      , oidcFlow          = oidcFlow
      , pendingTasks      = pendingTasks
      , currentTarget     = currentTarget
      }



makeStorageDummy :: [(AbsPath, FileWithContent)] -> Storage Filehub
makeStorageDummy mockFS =
  Storage
    { get = \path -> let mRes = lookup path mockFS
                      in case mRes of
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


makeStorageFileSys :: SessionId -> Storage Filehub
makeStorageFileSys sessionId =
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
                      set sessionId (.currentDir) dir
    , isDirectory = File.isDirectory
    , write       = File.write
    , mv          = File.mv
    , rename      = File.rename
    , delete      = File.delete
    , new         = File.new
    , newFolder   = File.newFolder
    , lsCwd       = do
                      currentDir <- get sessionId (.currentDir)
                      File.lsCwd currentDir
    , upload      = \filedata -> do
                      currentDir <- get sessionId (.currentDir)
                      File.upload currentDir filedata
    , download    = \clientPath -> do
                      fileSys <- getFileSys
                      File.download fileSys clientPath
    }
  where
    getFileSys :: Filehub (Target FileSys)
    getFileSys = do
      TargetView target _ <- get sessionId (.currentTarget)
      maybe (throwIO (FilehubError TargetError "Target is not valid file system direcotry")) pure $ handleTarget target
        [ targetHandler @FileSys id
        ]



makeStorageS3 :: SessionId -> Storage Filehub
makeStorageS3 sessionId =
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
        root     <- get sessionId (.root)
        s3       <- getS3
        let path =  fromClientPath root clientPath
        S3.download s3 path
    , isDirectory = \filePath -> do
        s3 <- getS3
        S3.isDirectory s3 filePath
    }
  where
    getS3 :: Filehub (Target S3)
    getS3 = do
      TargetView target _ <- get sessionId (.currentTarget)
      maybe (throwIO (FilehubError TargetError "Target is not valid S3 bucket")) pure $ handleTarget target
        [ targetHandler @S3 id
        ]
