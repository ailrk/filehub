{-# LANGUAGE NamedFieldPuns #-}
module Filehub.SharedLink
  ( SharedLinkType (..)
  , SharedLinkHash (..)
  , SharedLinkPasscode (..)
  , SharedLinkPermit (..)
  , SharedLinkPermitSet (..)
  , SharedLink (..)
  , SharedLinkPool (..)
  , mkSharedLinkHash
  , createSharedLink
  , revokeSharedLink
  , lookupSharedLink
  , getSharedLink
  , getAllSharedLinks
  , consumeSharedLink
  , updateSharedLink
  , addSharedLink
  , newShareLinkPool
  , newSharedLinkPermit
  , issuePermit
  ) where

import Filehub.Prelude
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ClientPath (AbsPath(..))
import Data.File (File(..), FileInfo)
import Target.Types (TargetId)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Servant (FromHttpApiData(..), ToHttpApiData(..))


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
  , targetId    :: TargetId
  }


data SharedLinkPool = SharedLinkPool
  { pool :: TVar (Map SharedLinkHash SharedLink)
  }


base62Chars :: Vector Char
base62Chars = V.fromList $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']


toBase62 :: Integer -> String
toBase62 0 = "0"
toBase62 n = reverse (go n)
  where
    go 0 = []
    go x =
      let (q, r) = x `divMod` 62
       in base62Chars V.! fromIntegral r : go q


shortHash :: ByteString -> Text
shortHash input =
  let digest = SHA256.hash input
      num    = bsToInteger digest
   in txt_pack $ take 7 (toBase62 num)
  where
    bsToInteger = bs_foldl' (\acc b -> acc * 256 + fromIntegral b) 0


mkSharedLinkHash :: ByteString -> SharedLinkHash
mkSharedLinkHash input = SharedLinkHash (shortHash input)


createSharedLink :: MonadIO m => FileInfo -> TargetId -> SharedLinkType -> Bool -> Maybe SharedLinkPasscode -> m SharedLink
createSharedLink file tgtId linkType readonly mPasscode = do
  now <- liftIO time_now
  salt <- liftIO UUID.nextRandom
  let hash = mkSharedLinkHash (coerce bs_packChars file.path <> bs_packChars (show salt))
  pure SharedLink
    { file        = file
    , hash        = hash
    , createdAt   = now
    , linkType    = linkType
    , accessCount = 0
    , passcode    = mPasscode
    , readonly    = readonly
    , revoked     = False
    , targetId    = tgtId
    }


revokeSharedLink :: MonadIO m => SharedLinkHash -> SharedLinkPool -> m ()
revokeSharedLink hash (SharedLinkPool pool) = atomically do
  modifyTVar' pool \m -> do
    map_update (\link -> Just (link { revoked = True })) hash m


lookupSharedLink :: MonadIO m => SharedLinkHash -> SharedLinkPool -> m (Maybe SharedLink)
lookupSharedLink hash (SharedLinkPool pool) = do
  now <- liftIO time_now
  atomically do
    m <- readTVar pool
    case map_lookup hash m of
      Just link@SharedLink { linkType, accessCount } -> do
        case linkType of
          SingleUse -> pure (Just link)
          Expiry date -> do
            if date < now
               then do
                 writeTVar pool (map_delete hash m)
                 pure Nothing
               else do
                 let newLink = link { accessCount = accessCount + 1}
                 writeTVar pool (map_insert hash newLink m)
                 pure (Just newLink)
          Indefinite -> do
            let newLink = link { accessCount = accessCount + 1}
            writeTVar pool (map_insert hash newLink m)
            pure (Just newLink)
      Nothing -> pure Nothing


getSharedLink :: MonadIO m => SharedLinkHash -> SharedLinkPool -> m (Maybe SharedLink)
getSharedLink hash (SharedLinkPool pool) =
  map_lookup hash <$> atomically (readTVar pool)


getAllSharedLinks :: MonadIO m => SharedLinkPool -> m [SharedLink]
getAllSharedLinks (SharedLinkPool pool) =
  map_elems <$> atomically (readTVar pool)


consumeSharedLink :: MonadIO m => SharedLinkHash -> SharedLinkPool -> m ()
consumeSharedLink hash (SharedLinkPool pool) = atomically do
  modifyTVar' pool $ map_adjust (\l -> l { accessCount = l.accessCount + 1 }) hash


updateSharedLink :: MonadIO m => SharedLinkHash -> (SharedLink -> SharedLink) -> SharedLinkPool -> m ()
updateSharedLink hash f (SharedLinkPool pool) = atomically do
  modifyTVar' pool (map_adjust f hash)


addSharedLink :: MonadIO m => SharedLink -> SharedLinkPool -> m ()
addSharedLink link (SharedLinkPool pool) = atomically do
  modifyTVar' pool (map_insert link.hash link)


newShareLinkPool :: MonadIO m => m SharedLinkPool
newShareLinkPool = do
  tvar <- newTVarIO mempty
  pure $ SharedLinkPool tvar


newSharedLinkPermit :: MonadIO m => m SharedLinkPermit
newSharedLinkPermit = do
  uuid <- liftIO UUID.nextRandom
  pure (SharedLinkPermit uuid)


-- | Returns a SetCookie value for `sharedlink_permit`.
-- If the session already has a permit, we reuse the same UUID and add the hash.
-- Otherwise we create a fresh permit UUID.
issuePermit :: MonadIO m => Maybe SharedLinkPermitSet -> SharedLinkHash -> m (SharedLinkPermit, SharedLinkPermitSet)
issuePermit mExisting hash = do
  permit <- case mExisting of
    Just (SharedLinkPermitSet p _) -> pure p
    Nothing                        -> newSharedLinkPermit
  let hashes = case mExisting of
        Just (SharedLinkPermitSet _ hs) -> hs
        Nothing                         -> mempty
      newSet = SharedLinkPermitSet permit (set_insert hash hashes)
  pure (permit, newSet)
