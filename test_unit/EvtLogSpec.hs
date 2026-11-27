{-# LANGUAGE NamedFieldPuns #-}
module EvtLogSpec (spec) where

import Filehub.Prelude
import Control.Handle.EvtLog
import Control.Handle.EvtLog qualified as EvtLog
import Data.ByteString.Lazy.Char8 qualified as LBS

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
withMemoryDB :: (EvtLog -> IO a) -> IO a
withMemoryDB = bracket (EvtLog.initialize ":memory:" maxSize) EvtLog.close


spec :: Spec
spec = describe "EvtLog" do

  it "emits and reads back events" $ withMemoryDB \h -> do
    let evt = MyEvent "hello world"
    EvtLog.emit h evt

    -- fold over all events
    result <- EvtLog.fold h Nothing (\acc e -> e : acc) []
    result `shouldBe` [evt]

  it "can emit multiple events and read them in order" $ withMemoryDB \h -> do
    let evt1 = MyEvent "first"
        evt2 = MyEvent "second"
    emit h evt1
    emit h evt2

    -- fold all events
    result <- EvtLog.fold h Nothing (\acc e -> acc ++ [e]) []
    result `shouldBe` [evt1, evt2]

  it "fold over empty log returns []" $ withMemoryDB \h -> do
    result <- EvtLog.fold @MyEvent h Nothing (\acc e -> e : acc) []
    result `shouldBe` []

  it "time range after all events excludes all results" $ withMemoryDB \h -> do
    emit h (MyEvent "event")
    -- MyEvent timestamps are fixed at "2025-01-01 00:00:00 UTC"
    let futureStart = read "2026-01-01 00:00:00 UTC"
        futureEnd   = read "2027-01-01 00:00:00 UTC"
    result <- EvtLog.fold @MyEvent h (Just (futureStart, futureEnd)) (\acc e -> e : acc) []
    result `shouldBe` []

  it "time range spanning event timestamp includes all matching events" $ withMemoryDB \h -> do
    let evt = MyEvent "in range"
    emit h evt
    -- MyEvent timestamps are fixed at "2025-01-01 00:00:00 UTC"
    let rangeStart = read "2024-06-01 00:00:00 UTC"
        rangeEnd   = read "2025-06-01 00:00:00 UTC"
    result <- EvtLog.fold @MyEvent h (Just (rangeStart, rangeEnd)) (\acc e -> acc ++ [e]) []
    result `shouldBe` [evt]

  it "N < maxSize events are all preserved in insertion order" $ withMemoryDB \h -> do
    let n      = maxSize - 1
        events = [ MyEvent ("msg" ++ show i) | i <- [(1 :: Int)..n] ]
    mapM_ (emit h) events
    result <- EvtLog.fold @MyEvent h Nothing (\acc e -> acc ++ [e]) []
    result `shouldBe` events

  it "deletes 20% of oldest events when maxSize is exceeded" $ withMemoryDB \h -> do
    -- maxSize + 1 events
    let events = [ MyEvent ("evt" ++ show i) | i <- [0..maxSize] ]
    mapM_ (EvtLog.emit h) events
    result <- EvtLog.fold @MyEvent h Nothing (\acc e -> acc ++ [e]) []

    -- Should have deleted 20% of maxSize = 4 oldest events
    let expectedRemaining = drop 4 events
    result `shouldBe` expectedRemaining
