{-# LANGUAGE NamedFieldPuns #-}
module Filehub.EvtLog where

import EvtLog (LogEvt(..), FromLogEvt(..), ToLogEvt(..))
import Target.Types (TargetId)
import Data.Time (UTCTime)
import Data.Aeson qualified as Aeson


data FilehubEvtLog
  = VirtualTargetCreated UTCTime TargetId
  deriving (Show)


instance FromLogEvt FilehubEvtLog where
  fromLogEvt LogEvt {eventTime, eventType, eventData } =
    case eventType of
      "VirtualTargetCreated" ->
        case Aeson.decode eventData of
          Just o -> Just (VirtualTargetCreated eventTime o)
          Nothing -> Nothing
      _ -> error "invalid event format"


instance ToLogEvt FilehubEvtLog where
  toLogEvt (VirtualTargetCreated time targetId) = LogEvt
    { eventTime = time
    , eventType = "VirtualTargetCreated"
    , eventData = Aeson.encode targetId
    }
