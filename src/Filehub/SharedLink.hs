{- HLINT ignore "Avoid restricted function" -}
{-# LANGUAGE NamedFieldPuns #-}

module Filehub.SharedLink
  ( SharedLink(..)
  , SharedLinkType(..)
  , SharedLinkHash
  , SharedLinkPool(..)
  , SharedLinkPermit(..)
  , SharedLinkPermitSet(..)
  , createSharedLink
  , revokeSharedLink
  , newShareLinkPool
  , lookupSharedLink
  , newSharedLinkPermit
  )
  where

import Data.File (File(..), FileInfo)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Effectful (Eff, MonadIO (..), (:>), IOE)
import Effectful.Concurrent.STM (TVar, newTVarIO, Concurrent, atomically, modifyTVar', readTVar, writeTVar)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Vector qualified as Vector
import Data.Vector (Vector)
import Data.Text qualified as Text
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Data.Set (Set)
import Data.Coerce (coerce)
import Data.ClientPath (AbsPath(..))


data SharedLinkType
  = SingleUse
  | Expiry UTCTime
  | Indefinite
  deriving (Show, Eq)


newtype SharedLinkHash = SharedLinkHash Text
  deriving (Show, Eq, Ord)


instance FromHttpApiData SharedLinkHash where
  parseUrlPiece = (SharedLinkHash <$>) . parseUrlPiece @Text


instance ToHttpApiData SharedLinkHash where
  toUrlPiece (SharedLinkHash hash) = toUrlPiece hash


newtype SharedLinkPasscode = SharedLinkPasscode Text
  deriving (Show, Eq, Ord)


-- | An uuid that proofs the client has the permission to access the shared link.
-- Once the client authenticated through the passcode, the SharedLinkPermit will be sent to the client as cookie,
-- hence subsequent requests can skip the authentication process.
newtype SharedLinkPermit = SharedLinkPermit UUID
  deriving (Show, Eq, Ord)


data SharedLinkPermitSet = SharedLinkPermitSet SharedLinkPermit (Set SharedLinkHash)
  deriving (Show, Eq, Ord)


data SharedLink = SharedLink
  { file        :: FileInfo
  , hash        :: SharedLinkHash
  , createdAt   :: UTCTime
  , linkType    :: SharedLinkType
  , accessCount :: Int
  , passcode    :: Maybe SharedLinkPasscode
  , readonly    :: Bool
  , revoked     :: Bool
  }


data SharedLinkPool = SharedLinkPool
  { pool :: TVar (Map SharedLinkHash SharedLink)
  }


base62Chars :: Vector Char
base62Chars = Vector.fromList $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']


toBase62 :: Integer -> String
toBase62 0 = "0"
toBase62 n = reverse (go n)
  where
    go 0 = []
    go x =
      let (q, r) = x `divMod` 62
       in base62Chars Vector.! fromIntegral r : go q


shortHash :: ByteString -> Text
shortHash input =
  let digest = SHA256.hash input
      num    = bsToInteger digest
   in Text.pack $ take 7 (toBase62 num)
  where
    bsToInteger = ByteString.foldl' (\acc b -> acc * 256 + fromIntegral b) 0


mkSharedLinkHash :: ByteString -> SharedLinkHash
mkSharedLinkHash input = SharedLinkHash (shortHash input)


createSharedLink :: IOE :> es => FileInfo -> SharedLinkType -> Bool -> Maybe SharedLinkPasscode -> Eff es SharedLink
createSharedLink file linkType readonly mPasscode = do
  now <- liftIO getCurrentTime
  let hash = mkSharedLinkHash (coerce Char8.pack file.path)
  pure SharedLink
    { file        = file
    , hash        = hash
    , createdAt   = now
    , linkType    = linkType
    , accessCount = 0
    , passcode    = mPasscode
    , readonly    = readonly
    , revoked     = False
    }


revokeSharedLink :: Concurrent :> es => SharedLinkHash -> SharedLinkPool -> Eff es ()
revokeSharedLink hash (SharedLinkPool pool) = atomically do
  modifyTVar' pool \m -> do
    Map.update (\link -> Just (link { revoked = True })) hash m


lookupSharedLink :: (IOE :> es, Concurrent :> es) => SharedLinkHash -> SharedLinkPool -> Eff es (Maybe SharedLink)
lookupSharedLink hash (SharedLinkPool pool) = do
  now <- liftIO getCurrentTime
  atomically do
    m <- readTVar pool
    case Map.lookup hash m of
      Just link@SharedLink { linkType, accessCount } -> do
        case linkType of
          SingleUse -> do
            writeTVar pool (Map.delete hash m)
            pure do if accessCount == 0 then (Just link) else Nothing
          Expiry date -> do
            if date < now
               then do
                 writeTVar pool (Map.delete hash m)
                 pure Nothing
               else do
                 let newLink = link { accessCount = accessCount + 1}
                 writeTVar pool (Map.insert hash newLink m)
                 pure (Just newLink)
          Indefinite -> do
            let newLink = link { accessCount = accessCount + 1}
            writeTVar pool (Map.insert hash newLink m)
            pure (Just newLink)
      Nothing -> pure Nothing


newShareLinkPool :: Concurrent :> es => Eff es SharedLinkPool
newShareLinkPool = do
  tvar <- newTVarIO mempty
  pure $ SharedLinkPool tvar


newSharedLinkPermit :: IOE :> es => Eff es SharedLinkPermit
newSharedLinkPermit = do
  uuid <- liftIO UUID.nextRandom
  pure (SharedLinkPermit uuid)
