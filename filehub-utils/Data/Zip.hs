module Data.Zip
  ( Entry(..)
  , writeZip
  , packDir
  ) where

import Codec.Compression.Zlib.Raw qualified as Deflate
import Control.Monad (forM, forM_)
import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Build
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Word (Word16, Word32)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, (</>))
import System.IO (Handle, IOMode (..), hTell, withFile)


-- CRC-32 with polynomial 0xEDB88320 (reflected).
crc32Table :: Vector Word32
crc32Table = V.generate 256 poly
  where
    poly n = foldl' step (fromIntegral n :: Word32) [0 .. 7 :: Int]
    step c _ | c .&. 1 /= 0 = 0xEDB88320 `xor` (c `shiftR` 1)
             | otherwise    = c `shiftR` 1

crc32 :: ByteString -> Word32
crc32 bs = BS.foldl' step 0xFFFFFFFF bs `xor` 0xFFFFFFFF
  where
    step acc b =
      let idx = fromIntegral ((acc .&. 0xFF) `xor` fromIntegral b) :: Int
      in (acc `shiftR` 8) `xor` V.unsafeIndex crc32Table idx

-- MS-DOS timestamp: 1980-01-01 00:00:00
dosDate :: Word16
dosDate = 0x0021   -- year=0 (from 1980), month=1, day=1

dosTime :: Word16
dosTime = 0x0000


data Entry = Entry
  { entryName  :: FilePath
  , entryBytes :: ByteString
  }


data EntryInfo = EntryInfo
  { eiOffset     :: Word32
  , eiName       :: ByteString
  , eiCRC32      :: Word32
  , eiMethod     :: Word16
  , eiCompSize   :: Word32
  , eiUncompSize :: Word32
  }


writeLocalEntry :: Handle -> Entry -> IO EntryInfo
writeLocalEntry h entry = do
  offset <- fromIntegral <$> hTell h
  let nameBS = T.encodeUtf8 (T.pack (toZipPath entry.entryName))
      crc    = crc32 entry.entryBytes
      uncomp = fromIntegral (BS.length entry.entryBytes) :: Word32
      comp   = LBS.toStrict (Deflate.compress (LBS.fromStrict entry.entryBytes))
      (method, compData)
        | BS.length comp < BS.length entry.entryBytes = (8, comp)
        | otherwise                                   = (0, entry.entryBytes)
      compSz = fromIntegral (BS.length compData) :: Word32
  LBS.hPut h . Build.toLazyByteString $
    Build.word32LE 0x04034B50              -- local file header signature
    <> Build.word16LE 20                   -- version needed: 2.0
    <> Build.word16LE 0x0800               -- general purpose: UTF-8
    <> Build.word16LE method               -- compression method
    <> Build.word16LE dosTime
    <> Build.word16LE dosDate
    <> Build.word32LE crc
    <> Build.word32LE compSz
    <> Build.word32LE uncomp
    <> Build.word16LE (fromIntegral (BS.length nameBS))
    <> Build.word16LE 0                    -- extra field length
    <> Build.byteString nameBS
  BS.hPut h compData
  pure EntryInfo
    { eiOffset     = offset
    , eiName       = nameBS
    , eiCRC32      = crc
    , eiMethod     = method
    , eiCompSize   = compSz
    , eiUncompSize = uncomp
    }


writeCentralDirectory :: Handle -> [EntryInfo] -> IO ()
writeCentralDirectory h entries = do
  cdOffset <- fromIntegral <$> hTell h
  forM_ entries $ \EntryInfo{..} ->
    LBS.hPut h . Build.toLazyByteString $
      Build.word32LE 0x02014B50             -- central dir signature
      <> Build.word16LE 20                  -- version made by
      <> Build.word16LE 20                  -- version needed
      <> Build.word16LE 0x0800             -- general purpose: UTF-8
      <> Build.word16LE eiMethod
      <> Build.word16LE dosTime
      <> Build.word16LE dosDate
      <> Build.word32LE eiCRC32
      <> Build.word32LE eiCompSize
      <> Build.word32LE eiUncompSize
      <> Build.word16LE (fromIntegral (BS.length eiName))
      <> Build.word16LE 0                   -- extra field length
      <> Build.word16LE 0                   -- file comment length
      <> Build.word16LE 0                   -- disk number start
      <> Build.word16LE 0                   -- internal file attributes
      <> Build.word32LE 0                   -- external file attributes
      <> Build.word32LE eiOffset            -- relative offset of local header
      <> Build.byteString eiName
  cdSize <- subtract cdOffset . fromIntegral <$> hTell h
  let n = fromIntegral (length entries) :: Word16
  LBS.hPut h . Build.toLazyByteString $
    Build.word32LE 0x06054B50               -- end of central dir signature
    <> Build.word16LE 0                     -- disk number
    <> Build.word16LE 0                     -- disk with start of CD
    <> Build.word16LE n                     -- entries on this disk
    <> Build.word16LE n                     -- total entries
    <> Build.word32LE cdSize
    <> Build.word32LE cdOffset
    <> Build.word16LE 0                     -- comment length


toZipPath :: FilePath -> String
toZipPath = map (\c -> if c == '\\' then '/' else c)


-- | Write a ZIP archive to @zipPath@ containing the given entries.
-- Uses DEFLATE when it reduces size, otherwise stores uncompressed.
writeZip :: FilePath -> [Entry] -> IO ()
writeZip zipPath entries =
  withFile zipPath WriteMode $ \h -> do
    infos <- mapM (writeLocalEntry h) entries
    writeCentralDirectory h infos


collectEntries :: FilePath -> FilePath -> IO [Entry]
collectEntries root dir = do
  names <- sort <$> listDirectory dir
  fmap concat . forM names $ \name -> do
    let full = dir </> name
    isDir <- doesDirectoryExist full
    if isDir
      then collectEntries root full
      else do
        bs <- BS.readFile full
        pure [Entry { entryName = makeRelative root full, entryBytes = bs }]


-- | Recursively pack @rootDir@ into a ZIP archive at @zipPath@.
packDir :: FilePath -> FilePath -> IO ()
packDir zipPath rootDir = collectEntries rootDir rootDir >>= writeZip zipPath
