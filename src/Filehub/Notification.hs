module Filehub.Notification where

import Servant.API.EventStream (ToServerEvent(..), ServerEvent (..))
import Data.ByteString.Lazy.Char8 qualified as Lazy.Char8
import Conduit (ConduitT, yield)
import Filehub.Monad (Filehub)
import Filehub.Types (SessionId)


data Notification
  = Pong
  deriving (Show, Eq)


instance ToServerEvent Notification where
  toServerEvent Pong = ServerEvent
    { eventType = Just (Lazy.Char8.pack (show Pong))
    , eventId   = Nothing
    , eventData = Lazy.Char8.pack (show Pong)
    }


listen :: SessionId -> Filehub (ConduitT () Notification IO ())
listen sessionId = do
  pure do
    yield Pong
