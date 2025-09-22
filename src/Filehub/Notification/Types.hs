module Filehub.Notification.Types where

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


instance ToJSON Notification where
  toJSON Pong = toJSON (show Pong)
  toJSON (TaskCompleted taskId) = toJSON
    [ "TaskCompleted"
    , Aeson.object [ "taskId" .= toJSON taskId ]
    ]
  toJSON (DeleteProgressed taskId progress) = toJSON
    [ "DeleteProgressed"
    , Aeson.object
        [ "taskId"   .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    ]
  toJSON (PasteProgressed taskId progress) = toJSON
    [ "DeleteProgressed"
    , Aeson.object
        [ "taskId" .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    ]
  toJSON (MoveProgressed taskId progress) = toJSON
    [ "DeleteProgressed"
    , Aeson.object
        [ "taskId" .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    ]
  toJSON (UploadProgressed taskId progress) = toJSON
    [ "DeleteProgressed"
    , Aeson.object
        [ "taskId" .= toJSON taskId
        , "progress" .= toJSON progress
        ]
    ]


instance ToServerEvent Notification where
  toServerEvent notification = ServerEvent
    { eventType = Nothing
    , eventId   = Nothing
    , eventData = Aeson.encode notification
    }
