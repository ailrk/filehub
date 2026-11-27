{-# LANGUAGE ForeignFunctionInterface #-}
module Data.DiskUsage (DiskUsage(..), getDiskUsage) where

import Filehub.Prelude
import Data.Word (Word8, Word64)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)


data DiskUsage = DiskUsage
  { total :: Integer
  , used  :: Integer
  } deriving (Show, Eq)


foreign import ccall "hs_get_disk_usage"
  c_getDiskUsage :: Ptr Word8 -> Ptr Word64 -> Ptr Word64 -> IO CInt


getDiskUsage :: MonadIO m => FilePath -> m (Maybe DiskUsage)
getDiskUsage path = liftIO do
  result <- tryIO $
    withCString path \cPath ->
    alloca \pTotal ->
    alloca \pUsed -> do
      ret <- c_getDiskUsage (castPtr cPath) pTotal pUsed
      if ret /= 0
        then pure Nothing
        else do
          t <- peek pTotal
          u <- peek pUsed
          pure $ Just DiskUsage { total = fromIntegral t, used = fromIntegral u }
  pure $ case result of
    Left _  -> Nothing
    Right v -> v
