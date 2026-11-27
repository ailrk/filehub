module Filehub.Notification.Types (Notification(..), TaskId, newTaskId) where

import Filehub.Prelude
import Servant.API.EventStream (ToServerEvent(..), ServerEvent (..))
import Data.Aeson ((.=), ToJSON(..))
import Data.Aeson qualified as Aeson
import Data.Unique (Unique, hashUnique, newUnique)
import Data.Hashable (Hashable)
import Lucid (Html, renderText)
import Text.Debug (Debug(..))


newtype TaskId = TaskId Unique
  deriving (Eq, Ord)
  deriving newtype (Hashable)

instance Show TaskId where
  show (TaskId u) = "TaskId " <> show (hashUnique u)

instance Debug TaskId where debug = show

instance ToJSON TaskId where
  toJSON (TaskId u) = toJSON (hashUnique u)

newTaskId :: MonadIO m => m TaskId
newTaskId = TaskId <$> liftIO newUnique


data Notification
  = Pong
  | SimpleMessage
      { content     :: Text
      }
  | TaskCompleted
      { taskId :: TaskId
      , htmxResponse :: Maybe (Html ())
      }
  | TaskFailed
      { taskId :: TaskId
      , htmxResponse :: Maybe (Html ())
      }
  | DeleteProgressed
      { taskId       :: TaskId
      , progress     :: Rational
      , htmxResponse :: Maybe (Html ())
      }
  | PasteProgressed
      { taskId       :: TaskId
      , progress     :: Rational
      , htmxResponse :: Maybe (Html ())
      }
  | MoveProgressed
      { taskId       :: TaskId
      , progress     :: Rational
      , htmxResponse :: Maybe (Html ())
      }
  | UploadProgressed
      { taskId       :: TaskId
      , progress     :: Rational
      , htmxResponse :: Maybe (Html ())
      }
  deriving (Show)


-- https://html.spec.whatwg.org/multipage/server-sent-events.html
instance ToServerEvent Notification where
  toServerEvent Pong = ServerEvent
    { eventType = Just "Pong"
    , eventId   = Nothing
    , eventData = ""
    }
  toServerEvent (SimpleMessage content) = ServerEvent
    { eventType = Nothing
    , eventId   = Nothing
    , eventData = Aeson.encode content
    }
  toServerEvent (TaskCompleted taskId htmxResponse) = ServerEvent
    { eventType = Just "TaskCompleted"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"       .= toJSON taskId
        , "htmxResponse" .= toJSON (renderText <$> htmxResponse)
        ]
    }
  toServerEvent (TaskFailed taskId htmxResponse) = ServerEvent
    { eventType = Just "TaskFailed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"       .= toJSON taskId
        , "htmxResponse" .= toJSON (renderText <$> htmxResponse)
        ]
    }
  toServerEvent (DeleteProgressed taskId progress htmxResponse) = ServerEvent
    { eventType = Just "DeleteProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"       .= toJSON taskId
        , "progress"     .= toJSON progress
        , "htmxResponse" .= toJSON (renderText <$> htmxResponse)
        ]
    }
  toServerEvent (PasteProgressed taskId progress htmxResponse) = ServerEvent
    { eventType = Just "PasteProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"       .= toJSON taskId
        , "progress"     .= toJSON progress
        , "htmxResponse" .= toJSON (renderText <$> htmxResponse)
        ]
    }
  toServerEvent (MoveProgressed taskId progress htmxResponse) = ServerEvent
    { eventType = Just "MoveProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"       .= toJSON taskId
        , "progress"     .= toJSON progress
        , "htmxResponse" .= toJSON (renderText <$> htmxResponse)
        ]
    }
  toServerEvent (UploadProgressed taskId progress htmxResponse) = ServerEvent
    { eventType = Just "UploadProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"       .= toJSON taskId
        , "progress"     .= toJSON progress
        , "htmxResponse" .= toJSON (renderText <$> htmxResponse)
        ]
    }
