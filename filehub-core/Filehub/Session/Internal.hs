module Filehub.Session.Internal
  ( createSession
  , extendSession
  )
  where

import Filehub.Prelude
import Control.Monad.Reader (asks)
import Data.ClientPath (AbsPath(..), Root(..))
import Data.UUID.V4 qualified as UUID
import Filehub.Monad (Filehub)
import Filehub.Session.Types (Session(..), TargetSessionData(..), Selected (..), CopyState (..), Layout (..))
import Filehub.Session.Types.SessionId (SessionId(..))
import Filehub.Types (SortFileBy (..), Env(..))
import Filehub.UserAgent qualified as UserAgent
import Target.File (FileSys)
import Target.File (Target(..))
import Target.Types (AnyTarget (..), TargetId, execTarget, targetHandler, IsTarget (..))
import UnliftIO (atomically, writeTVar)
import UnliftIO.STM (newTBQueueIO)


createSessionId :: Filehub SessionId
createSessionId = SessionId <$> liftIO UUID.nextRandom


createExpireDate :: Filehub UTCTime
createExpireDate = do
  duration <- asks (.sessionDuration)
  current  <- liftIO time_now
  pure $ duration `time_add` current


createSession :: Filehub Session
createSession = do
  ts              <- asks (.targets) >>= readTVarIO
  theme           <- readTVarIO =<< asks (.theme)
  locale          <- readTVarIO =<< asks (.locale)
  sessionId       <- createSessionId
  expireDate      <- createExpireDate
  notifications   <- liftIO (newTBQueueIO 16)
  pendingTasks    <- newTVarIO set_empty
  currentTargetId <- newTVarIO . fst . head $ ts
  targets         <- createTargets ts
  pure Session
    { sessionId         = sessionId
    , authId            = Nothing
    , sharedLinkPermit  = Nothing
    , resolution        = Nothing
    , deviceType        = UserAgent.Unknown
    , expireDate        = expireDate
    , targets           = targets -- targetToSessionData <$> map_fromList targets
    , copyState         = NoCopyPaste
    , currentTargetId   = currentTargetId
    , sidebarCollapsed  = False
    , layout            = ThumbnailLayout
    , theme             = theme
    , locale            = locale
    , oidcFlow          = Nothing
    , notifications     = notifications
    , pendingTasks      = pendingTasks
    }


createTargets :: [(TargetId, AnyTarget)] -> Filehub (Map TargetId TargetSessionData)
createTargets ts = do
  tds <- for ts \(tid, t) -> do
    td <- mkDef
    _ <- execTarget t
      [ targetHandler @FileSys \(x :: Target FileSys) -> do
          atomically do
            writeTVar td.currentDir (coerce x.root)
      ]
    pure (tid, td)
  pure $ map_fromList tds

  where
    mkDef = do
      currentDir   <- newTVarIO (AbsPath "")
      sortedFileBy <- newTVarIO ByModifiedDown
      selected     <- newTVarIO NoSelection
      pure TargetSessionData { currentDir = currentDir, sortedFileBy = sortedFileBy, selected = selected }


extendSession :: NominalDiffTime -> Session -> Session
extendSession extension session = session { expireDate = extension `time_add` session.expireDate  }
