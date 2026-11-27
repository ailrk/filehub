{-# LANGUAGE LambdaCase #-}
module Filehub.History.Types
  ( History(..)
  , HistoryEntry(..)
  , Operation(..)
  , newHistory
  , addEntry
  , getHistory
  , lookupEntry
  , nextId
  , clientName
  , clientFullPath
  , describeOperation
  ) where

import Filehub.Prelude
import Data.ClientPath (ClientPath(..))
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Filehub.SharedLink (SharedLinkHash)
import Filehub.Session.Types (SessionId)
import Network.URI.Encode qualified as URI.Encode
import System.FilePath (takeFileName)
import Target.Types (TargetId)


maxHistory :: Int
maxHistory = 500


data Operation
  = OpRename    TargetId ClientPath ClientPath
  | OpNewFile   TargetId ClientPath
  | OpNewFolder TargetId ClientPath
  | OpMove      TargetId [ClientPath] ClientPath
  | OpDelete    TargetId [ClientPath]
  | OpUpload    TargetId ClientPath
  | OpCopy      TargetId Int
  | OpPaste     TargetId ClientPath [Text]
  | OpShare     TargetId ClientPath SharedLinkHash
  | OpRevokeLink TargetId SharedLinkHash
  deriving (Show, Eq)


data HistoryEntry = HistoryEntry
  { entryId   :: Int
  , timestamp :: UTCTime
  , sessionId :: SessionId
  , operation :: Operation
  , succeeded :: Bool
  } deriving (Show)


data History = History
  { entries :: TVar (Seq HistoryEntry)
  , counter :: TVar Int
  }


newHistory :: IO History
newHistory = History <$> newTVarIO Seq.empty <*> newTVarIO 0


addEntry :: History -> HistoryEntry -> IO ()
addEntry h entry = atomically $ modifyTVar' h.entries \s ->
  let s' = if Seq.length s >= maxHistory then Seq.drop 1 s else s
   in s' Seq.|> entry


getHistory :: History -> IO [HistoryEntry]
getHistory h = reverse . toList <$> atomically (readTVar h.entries)


lookupEntry :: History -> Int -> IO (Maybe HistoryEntry)
lookupEntry h eid = do
  es <- atomically (readTVar h.entries)
  pure $ find (\e -> e.entryId == eid) (toList es)


nextId :: History -> IO Int
nextId h = atomically do
  n <- readTVar h.counter
  modifyTVar' h.counter (+1)
  pure n


describeOperation :: Operation -> Text
describeOperation = \case
  OpRename    _ from to -> txt_pack $ clientName from <> " → " <> clientName to
  OpNewFile   _ p       -> txt_pack $ clientName p
  OpNewFolder _ p       -> txt_pack $ clientName p
  OpMove      _ srcs _  -> txt_pack $ show (length srcs) <> " item(s)"
  OpDelete    _ ps      -> txt_pack $ show (length ps) <> " item(s)"
  OpUpload    _ p       -> txt_pack $ clientName p
  OpCopy      _ n       -> txt_pack $ show n <> " item(s)"
  OpPaste     _ _ fs    -> txt_pack $ show (length fs) <> " item(s)"
  OpShare     _ p _     -> txt_pack $ clientName p
  OpRevokeLink _ _      -> ""


decodeCP :: ClientPath -> String
decodeCP (ClientPath p) = URI.Encode.decode p

encodeCP :: String -> ClientPath
encodeCP = ClientPath . URI.Encode.encode

clientName :: ClientPath -> String
clientName = takeFileName . decodeCP

clientFullPath :: ClientPath -> String
clientFullPath = decodeCP

