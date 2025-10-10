module Filehub.Notification.Types (Notification(..)) where

import Servant.API.EventStream (ToServerEvent(..), ServerEvent (..))
import Worker.Task (TaskId)
import Data.Aeson ((.=), ToJSON(..))
import Data.Aeson qualified as Aeson


data Notification
  = Pong
  | TaskCompleted TaskId
  | DeleteProgressed TaskId Rational
  | PasteProgressed TaskId Rational
  | MoveProgressed TaskId Rational
  | UploadProgressed TaskId Rational
  deriving (Show, Eq)


-- https://html.spec.whatwg.org/multipage/server-sent-events.html
instance ToServerEvent Notification where
  toServerEvent Pong = ServerEvent
    { eventType = Nothing
    , eventId   = Nothing
    , eventData = "Pong"
    }
  toServerEvent (TaskCompleted taskId) = ServerEvent
    { eventType = Just "TaskCompleted"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object [ "taskId" .= toJSON taskId ]
    }
  toServerEvent (DeleteProgressed taskId progress) = ServerEvent
    { eventType = Just "DeleteProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"   .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    }
  toServerEvent (PasteProgressed taskId progress) = ServerEvent
    { eventType = Just "PasteProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"   .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    }
  toServerEvent (MoveProgressed taskId progress) = ServerEvent
    { eventType = Just "MoveProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"   .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    }
  toServerEvent (UploadProgressed taskId progress) = ServerEvent
    { eventType = Just "UploadProgressed"
    , eventId   = Nothing
    , eventData = Aeson.encode $ Aeson.object
        [ "taskId"   .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    }
