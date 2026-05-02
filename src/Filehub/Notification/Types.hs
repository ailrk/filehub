module Filehub.Notification.Types (Notification(..)) where

import Servant.API.EventStream (ToServerEvent(..), ServerEvent (..))
import Worker.Task (TaskId)
import Data.Aeson ((.=), ToJSON(..))
import Data.Aeson qualified as Aeson
import Lucid (Html, renderText)
import Data.Text (Text)


data Notification
  = Pong
  | SimpleMessage
      { content     :: Text
      }
  | TaskCompleted
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
    { eventType = Nothing
    , eventId   = Nothing
    , eventData = "Pong"
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
