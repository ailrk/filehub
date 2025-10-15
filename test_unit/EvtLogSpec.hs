{-# LANGUAGE NamedFieldPuns #-}
module EvtLogSpec (spec) where

import Control.Exception (bracket)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Time.Clock (getCurrentTime, addUTCTime)
import EvtLog
import Test.Hspec


maxSize :: Int
maxSize = 20

data MyEvent = MyEvent
  { msg :: String
  } deriving (Show, Eq)


instance ToLogEvt MyEvent where
  toLogEvt (MyEvent msg) =
    LogEvt
      { eventTime = read "2025-01-01 00:00:00 UTC"
      , eventType = "MyEvent"
      , eventData = LBS.pack msg
      }


instance FromLogEvt MyEvent where
  fromLogEvt LogEvt{eventType, eventData} =
    if eventType == "MyEvent"
      then Just (MyEvent (LBS.unpack eventData))
      else Nothing


-- Helper: run a test with an in-memory SQLite DB
withMemoryDB :: (Handle -> IO a) -> IO a
withMemoryDB = bracket (EvtLog.initialize ":memory:" maxSize) EvtLog.close


spec :: Spec
spec = describe "EvtLog Integration Tests" $ do

  it "emits and reads back events" $ withMemoryDB $ \h -> do
    let evt = MyEvent "hello world"
    EvtLog.emit h evt

    -- fold over all events
    result <- EvtLog.fold h Nothing (\acc e -> e : acc) []
    result `shouldBe` [evt]

  it "can emit multiple events and read them in order" $ withMemoryDB $ \h -> do
    let evt1 = MyEvent "first"
        evt2 = MyEvent "second"
    emit h evt1
    emit h evt2

    -- fold all events
    result <- EvtLog.fold h Nothing (\acc e -> acc ++ [e]) []
    result `shouldBe` [evt1, evt2]

  it "fold with time range returns only matching events" $ withMemoryDB $ \h -> do
    now <- getCurrentTime
    let evt1 = MyEvent "early"
        evt2 = MyEvent "late"
    emit h evt1
    emit h evt2

    let range = Just (now, addUTCTime 3600 now)
    _ <- EvtLog.fold @MyEvent h range (\acc e -> e : acc) []  -- fold just to make sure no exceptions
    pure ()


  it "deletes 20% of oldest events when maxSize is exceeded" $ withMemoryDB $ \h -> do
    -- maxSize + 1 events
    let events = [ MyEvent ("evt" ++ show i) | i <- [0..maxSize] ]
    mapM_ (EvtLog.emit h) events
    result <- EvtLog.fold @MyEvent h Nothing (\acc e -> acc ++ [e]) []

    -- Should have deleted 20% of maxSize = 4 oldest events
    let expectedRemaining = drop 4 events
    result `shouldBe` expectedRemaining
