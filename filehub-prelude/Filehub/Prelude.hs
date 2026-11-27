-- | Module system is a trap, I monomorphize Haskell.
module Filehub.Prelude
  ( -- * Text  (strict)
    Text
  , txt_pack
  , txt_unpack
  , txt_show
  , txt_null
  , txt_empty
  , txt_length
  , txt_singleton
  , txt_append
  , txt_concat
  , txt_intercalate
  , txt_intersperse
  , txt_isPrefixOf
  , txt_isSuffixOf
  , txt_isInfixOf
  , txt_strip
  , txt_stripStart
  , txt_stripEnd
  , txt_stripPrefix
  , txt_stripSuffix
  , txt_toLower
  , txt_toUpper
  , txt_toCaseFold
  , txt_lines
  , txt_words
  , txt_unlines
  , txt_unwords
  , txt_splitOn
  , txt_breakOn
  , txt_take
  , txt_drop
  , txt_takeEnd
  , txt_dropEnd
  , txt_takeWhile
  , txt_dropWhile
  , txt_head
  , txt_last
  , txt_tail
  , txt_init
  , txt_reverse
  , txt_map
  , txt_filter
  , txt_foldl'
  , txt_foldr
  , txt_any
  , txt_all
  , txt_replicate
  , txt_replace
  , txt_encode
  , txt_decode

    ------------------------------------------------------------
    -- * Text  (lazy)
    ------------------------------------------------------------
  , LText
  , lTxt_pack
  , lTxt_unpack
  , lTxt_fromStrict
  , lTxt_toStrict

    ------------------------------------------------------------
    -- * ByteString  (strict)
    ------------------------------------------------------------
  , ByteString
  , bs_pack
  , bs_unpack
  , bs_packChars
  , bs_unpackChars
  , bs_empty
  , bs_null
  , bs_length
  , bs_singleton
  , bs_append
  , bs_concat
  , bs_intercalate
  , bs_isPrefixOf
  , bs_isSuffixOf
  , bs_isInfixOf
  , bs_strip
  , bs_take
  , bs_drop
  , bs_takeWhile
  , bs_dropWhile
  , bs_splitAt
  , bs_breakSubstring
  , bs_map
  , bs_filter
  , bs_foldl'
  , bs_foldr
  , bs_any
  , bs_all

    ------------------------------------------------------------
    -- * ByteString  (lazy)
    ------------------------------------------------------------
  , LByteString
  , lBS_pack
  , lBS_unpack
  , lBS_empty
  , lBS_null
  , lBS_length
  , lBS_append
  , lBS_concat
  , lBS_toStrict
  , lBS_fromStrict
  , lBS_fromChunks
  , lBS_toChunks

    ------------------------------------------------------------
    -- * Map  (strict)
    ------------------------------------------------------------
  , Map
  , map_empty
  , map_singleton
  , map_fromList
  , map_fromListWith
  , map_toList
  , map_toAscList
  , map_keys
  , map_elems
  , map_size
  , map_null
  , map_member
  , map_notMember
  , map_lookup
  , map_findWithDefault
  , map_insert
  , map_insertWith
  , map_delete
  , map_adjust
  , map_update
  , map_alter
  , map_map
  , map_mapWithKey
  , map_mapKeys
  , map_filter
  , map_filterWithKey
  , map_partition
  , map_foldl'
  , map_foldr
  , map_foldlWithKey'
  , map_foldrWithKey
  , map_traverseWithKey
  , map_union
  , map_unionWith
  , map_unionWithKey
  , map_unions
  , map_unionsWith
  , map_intersection
  , map_intersectionWith
  , map_difference
  , map_differenceWith

    ------------------------------------------------------------
    -- * Set  (strict)
    ------------------------------------------------------------
  , Set
  , set_empty
  , set_singleton
  , set_fromList
  , set_toList
  , set_toAscList
  , set_size
  , set_null
  , set_member
  , set_notMember
  , set_insert
  , set_delete
  , set_map
  , set_filter
  , set_partition
  , set_foldl'
  , set_foldr
  , set_union
  , set_unions
  , set_intersection
  , set_difference
  , set_isSubsetOf
  , set_disjoint

    ------------------------------------------------------------
    -- * Seq
    ------------------------------------------------------------
  , Seq
  , seq_empty
  , seq_singleton
  , seq_fromList
  , seq_toList
  , seq_length
  , seq_null
  , seq_index
  , seq_append

    ------------------------------------------------------------
    -- * Maybe
    ------------------------------------------------------------
  , fromMaybe
  , catMaybes
  , mapMaybe
  , listToMaybe
  , maybeToList
  , isJust
  , isNothing

    ------------------------------------------------------------
    -- * Either
    ------------------------------------------------------------
  , partitionEithers
  , lefts
  , rights

    ------------------------------------------------------------
    -- * List
    ------------------------------------------------------------
  , sortBy
  , sortOn
  , groupBy
  , nubBy
  , intercalate
  , intersperse
  , isPrefixOf
  , isSuffixOf
  , find
  , findIndex
  , partition
  , transpose
  , tails
  , inits

    ------------------------------------------------------------
    -- * Monad / Applicative
    ------------------------------------------------------------
  , when
  , unless
  , void
  , join
  , guard
  , (<|>)
  , (<&>)

    ------------------------------------------------------------
    -- * Traversal
    ------------------------------------------------------------
  , for
  , for_
  , forM
  , forM_
  , traverse_
  , foldM
  , asum

    ------------------------------------------------------------
    -- * Function
    ------------------------------------------------------------
  , (&)
  , on
  , fix

    ------------------------------------------------------------
    -- * Coerce / Identity
    ------------------------------------------------------------
  , coerce
  , Identity (..)

    ------------------------------------------------------------
    -- * Generic
    ------------------------------------------------------------
  , Generic

    ------------------------------------------------------------
    -- * IORef
    ------------------------------------------------------------
  , IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef'

    ------------------------------------------------------------
    -- * STM / TVar
    ------------------------------------------------------------
  , STM
  , TVar
  , newTVar
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , modifyTVar
  , modifyTVar'
  , atomically

    ------------------------------------------------------------
    -- * IO / Exceptions
    ------------------------------------------------------------
  , throwIO
  , try
  , catch
  , handle
  , bracket
  , finally
  , MonadIO (..)
  , MonadUnliftIO (..)

    ------------------------------------------------------------
    -- * String interpolation
    ------------------------------------------------------------
  , i

    ------------------------------------------------------------
    -- * Concurrency
    ------------------------------------------------------------
  , forConcurrently
  , forConcurrently_
  , Async
  , async
  , wait
  , cancel
  ) where


import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word8)
import Data.Int (Int64)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as SMap

import Data.Set (Set)
import Data.Set qualified as SSet

import Data.Sequence (Seq, (><))
import Data.Sequence qualified as SSeq

import Data.Maybe
  ( fromMaybe, catMaybes, mapMaybe, listToMaybe, maybeToList, isJust, isNothing )
import Data.Either (partitionEithers, lefts, rights)
import Data.List
  ( sortBy, sortOn, groupBy, nubBy, intercalate, intersperse
  , isPrefixOf, isSuffixOf, find, findIndex, partition
  , transpose, tails, inits )

import Control.Monad (when, unless, void, join, guard, forM, forM_, foldM)
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Foldable (for_, traverse_, asum)
import Data.Function ((&), on, fix)

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, modifyTVar, modifyTVar')
import UnliftIO
  ( newTVarIO, readTVarIO, atomically
  , throwIO, try, catch, handle, bracket, finally
  , MonadIO (..), MonadUnliftIO (..) )

import Data.String.Interpolate (i)

import UnliftIO.Async (forConcurrently, forConcurrently_, Async, async, wait, cancel)


-- Text (strict)

type LText = LT.Text

txt_pack :: String -> Text
txt_pack = T.pack

txt_unpack :: Text -> String
txt_unpack = T.unpack

txt_show :: Show a => a -> Text
txt_show = T.pack . show

txt_null :: Text -> Bool
txt_null = T.null

txt_empty :: Text
txt_empty = T.empty

txt_length :: Text -> Int
txt_length = T.length

txt_singleton :: Char -> Text
txt_singleton = T.singleton

txt_append :: Text -> Text -> Text
txt_append = T.append

txt_concat :: [Text] -> Text
txt_concat = T.concat

txt_intercalate :: Text -> [Text] -> Text
txt_intercalate = T.intercalate

txt_intersperse :: Char -> Text -> Text
txt_intersperse = T.intersperse

txt_isPrefixOf :: Text -> Text -> Bool
txt_isPrefixOf = T.isPrefixOf

txt_isSuffixOf :: Text -> Text -> Bool
txt_isSuffixOf = T.isSuffixOf

txt_isInfixOf :: Text -> Text -> Bool
txt_isInfixOf = T.isInfixOf

txt_strip :: Text -> Text
txt_strip = T.strip

txt_stripStart :: Text -> Text
txt_stripStart = T.stripStart

txt_stripEnd :: Text -> Text
txt_stripEnd = T.stripEnd

txt_stripPrefix :: Text -> Text -> Maybe Text
txt_stripPrefix = T.stripPrefix

txt_stripSuffix :: Text -> Text -> Maybe Text
txt_stripSuffix = T.stripSuffix

txt_toLower :: Text -> Text
txt_toLower = T.toLower

txt_toUpper :: Text -> Text
txt_toUpper = T.toUpper

txt_toCaseFold :: Text -> Text
txt_toCaseFold = T.toCaseFold

txt_lines :: Text -> [Text]
txt_lines = T.lines

txt_words :: Text -> [Text]
txt_words = T.words

txt_unlines :: [Text] -> Text
txt_unlines = T.unlines

txt_unwords :: [Text] -> Text
txt_unwords = T.unwords

txt_splitOn :: Text -> Text -> [Text]
txt_splitOn = T.splitOn

txt_breakOn :: Text -> Text -> (Text, Text)
txt_breakOn = T.breakOn

txt_take :: Int -> Text -> Text
txt_take = T.take

txt_drop :: Int -> Text -> Text
txt_drop = T.drop

txt_takeEnd :: Int -> Text -> Text
txt_takeEnd = T.takeEnd

txt_dropEnd :: Int -> Text -> Text
txt_dropEnd = T.dropEnd

txt_takeWhile :: (Char -> Bool) -> Text -> Text
txt_takeWhile = T.takeWhile

txt_dropWhile :: (Char -> Bool) -> Text -> Text
txt_dropWhile = T.dropWhile

txt_head :: Text -> Char
txt_head = T.head

txt_last :: Text -> Char
txt_last = T.last

txt_tail :: Text -> Text
txt_tail = T.tail

txt_init :: Text -> Text
txt_init = T.init

txt_reverse :: Text -> Text
txt_reverse = T.reverse

txt_map :: (Char -> Char) -> Text -> Text
txt_map = T.map

txt_filter :: (Char -> Bool) -> Text -> Text
txt_filter = T.filter

txt_foldl' :: (a -> Char -> a) -> a -> Text -> a
txt_foldl' = T.foldl'

txt_foldr :: (Char -> a -> a) -> a -> Text -> a
txt_foldr = T.foldr

txt_any :: (Char -> Bool) -> Text -> Bool
txt_any = T.any

txt_all :: (Char -> Bool) -> Text -> Bool
txt_all = T.all

txt_replicate :: Int -> Text -> Text
txt_replicate = T.replicate

txt_replace :: Text -> Text -> Text -> Text
txt_replace = T.replace

txt_encode :: Text -> ByteString
txt_encode = TE.encodeUtf8

txt_decode :: ByteString -> Text
txt_decode = TE.decodeUtf8With lenientDecode


-- Text (lazy)

lTxt_pack :: String -> LText
lTxt_pack = LT.pack

lTxt_unpack :: LText -> String
lTxt_unpack = LT.unpack

lTxt_fromStrict :: Text -> LText
lTxt_fromStrict = LT.fromStrict

lTxt_toStrict :: LText -> Text
lTxt_toStrict = LT.toStrict


-- ByteString (strict)

bs_pack :: [Word8] -> ByteString
bs_pack = BS.pack

bs_unpack :: ByteString -> [Word8]
bs_unpack = BS.unpack

bs_packChars :: String -> ByteString
bs_packChars = BC.pack

bs_unpackChars :: ByteString -> String
bs_unpackChars = BC.unpack

bs_empty :: ByteString
bs_empty = BS.empty

bs_null :: ByteString -> Bool
bs_null = BS.null

bs_length :: ByteString -> Int
bs_length = BS.length

bs_singleton :: Word8 -> ByteString
bs_singleton = BS.singleton

bs_append :: ByteString -> ByteString -> ByteString
bs_append = BS.append

bs_concat :: [ByteString] -> ByteString
bs_concat = BS.concat

bs_intercalate :: ByteString -> [ByteString] -> ByteString
bs_intercalate = BS.intercalate

bs_isPrefixOf :: ByteString -> ByteString -> Bool
bs_isPrefixOf = BS.isPrefixOf

bs_isSuffixOf :: ByteString -> ByteString -> Bool
bs_isSuffixOf = BS.isSuffixOf

bs_isInfixOf :: ByteString -> ByteString -> Bool
bs_isInfixOf = BS.isInfixOf

bs_strip :: ByteString -> ByteString
bs_strip = BC.strip

bs_take :: Int -> ByteString -> ByteString
bs_take = BS.take

bs_drop :: Int -> ByteString -> ByteString
bs_drop = BS.drop

bs_takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
bs_takeWhile = BS.takeWhile

bs_dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
bs_dropWhile = BS.dropWhile

bs_splitAt :: Int -> ByteString -> (ByteString, ByteString)
bs_splitAt = BS.splitAt

bs_breakSubstring :: ByteString -> ByteString -> (ByteString, ByteString)
bs_breakSubstring = BS.breakSubstring

bs_map :: (Word8 -> Word8) -> ByteString -> ByteString
bs_map = BS.map

bs_filter :: (Word8 -> Bool) -> ByteString -> ByteString
bs_filter = BS.filter

bs_foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
bs_foldl' = BS.foldl'

bs_foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
bs_foldr = BS.foldr

bs_any :: (Word8 -> Bool) -> ByteString -> Bool
bs_any = BS.any

bs_all :: (Word8 -> Bool) -> ByteString -> Bool
bs_all = BS.all


-- ByteString (lazy)

type LByteString = LBS.ByteString

lBS_pack :: [Word8] -> LByteString
lBS_pack = LBS.pack

lBS_unpack :: LByteString -> [Word8]
lBS_unpack = LBS.unpack

lBS_empty :: LByteString
lBS_empty = LBS.empty

lBS_null :: LByteString -> Bool
lBS_null = LBS.null

lBS_length :: LByteString -> Int64
lBS_length = fromIntegral . LBS.length

lBS_append :: LByteString -> LByteString -> LByteString
lBS_append = LBS.append

lBS_concat :: [LByteString] -> LByteString
lBS_concat = LBS.concat

lBS_toStrict :: LByteString -> ByteString
lBS_toStrict = LBS.toStrict

lBS_fromStrict :: ByteString -> LByteString
lBS_fromStrict = LBS.fromStrict

lBS_fromChunks :: [ByteString] -> LByteString
lBS_fromChunks = LBS.fromChunks

lBS_toChunks :: LByteString -> [ByteString]
lBS_toChunks = LBS.toChunks


-- Map (strict)

map_empty :: Map k v
map_empty = SMap.empty

map_singleton :: k -> v -> Map k v
map_singleton = SMap.singleton

map_fromList :: Ord k => [(k, v)] -> Map k v
map_fromList = SMap.fromList

map_fromListWith :: Ord k => (v -> v -> v) -> [(k, v)] -> Map k v
map_fromListWith = SMap.fromListWith

map_toList :: Map k v -> [(k, v)]
map_toList = SMap.toList

map_toAscList :: Map k v -> [(k, v)]
map_toAscList = SMap.toAscList

map_keys :: Map k v -> [k]
map_keys = SMap.keys

map_elems :: Map k v -> [v]
map_elems = SMap.elems

map_size :: Map k v -> Int
map_size = SMap.size

map_null :: Map k v -> Bool
map_null = SMap.null

map_member :: Ord k => k -> Map k v -> Bool
map_member = SMap.member

map_notMember :: Ord k => k -> Map k v -> Bool
map_notMember = SMap.notMember

map_lookup :: Ord k => k -> Map k v -> Maybe v
map_lookup = SMap.lookup

map_findWithDefault :: Ord k => v -> k -> Map k v -> v
map_findWithDefault = SMap.findWithDefault

map_insert :: Ord k => k -> v -> Map k v -> Map k v
map_insert = SMap.insert

map_insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
map_insertWith = SMap.insertWith

map_delete :: Ord k => k -> Map k v -> Map k v
map_delete = SMap.delete

map_adjust :: Ord k => (v -> v) -> k -> Map k v -> Map k v
map_adjust = SMap.adjust

map_update :: Ord k => (v -> Maybe v) -> k -> Map k v -> Map k v
map_update = SMap.update

map_alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
map_alter = SMap.alter

map_map :: (a -> b) -> Map k a -> Map k b
map_map = SMap.map

map_mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
map_mapWithKey = SMap.mapWithKey

map_mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
map_mapKeys = SMap.mapKeys

map_filter :: (v -> Bool) -> Map k v -> Map k v
map_filter = SMap.filter

map_filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v
map_filterWithKey = SMap.filterWithKey

map_partition :: (v -> Bool) -> Map k v -> (Map k v, Map k v)
map_partition = SMap.partition

map_foldl' :: (a -> b -> a) -> a -> Map k b -> a
map_foldl' = SMap.foldl'

map_foldr :: (a -> b -> b) -> b -> Map k a -> b
map_foldr = SMap.foldr

map_foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
map_foldlWithKey' = SMap.foldlWithKey'

map_foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
map_foldrWithKey = SMap.foldrWithKey

map_traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
map_traverseWithKey = SMap.traverseWithKey

map_union :: Ord k => Map k v -> Map k v -> Map k v
map_union = SMap.union

map_unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
map_unionWith = SMap.unionWith

map_unionWithKey :: Ord k => (k -> v -> v -> v) -> Map k v -> Map k v -> Map k v
map_unionWithKey = SMap.unionWithKey

map_unions :: Ord k => [Map k v] -> Map k v
map_unions = SMap.unions

map_unionsWith :: Ord k => (v -> v -> v) -> [Map k v] -> Map k v
map_unionsWith = SMap.unionsWith

map_intersection :: Ord k => Map k a -> Map k b -> Map k a
map_intersection = SMap.intersection

map_intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
map_intersectionWith = SMap.intersectionWith

map_difference :: Ord k => Map k a -> Map k b -> Map k a
map_difference = SMap.difference

map_differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
map_differenceWith = SMap.differenceWith


-- Set (strict)

set_empty :: Set a
set_empty = SSet.empty

set_singleton :: a -> Set a
set_singleton = SSet.singleton

set_fromList :: Ord a => [a] -> Set a
set_fromList = SSet.fromList

set_toList :: Set a -> [a]
set_toList = SSet.toList

set_toAscList :: Set a -> [a]
set_toAscList = SSet.toAscList

set_size :: Set a -> Int
set_size = SSet.size

set_null :: Set a -> Bool
set_null = SSet.null

set_member :: Ord a => a -> Set a -> Bool
set_member = SSet.member

set_notMember :: Ord a => a -> Set a -> Bool
set_notMember = SSet.notMember

set_insert :: Ord a => a -> Set a -> Set a
set_insert = SSet.insert

set_delete :: Ord a => a -> Set a -> Set a
set_delete = SSet.delete

set_map :: Ord b => (a -> b) -> Set a -> Set b
set_map = SSet.map

set_filter :: (a -> Bool) -> Set a -> Set a
set_filter = SSet.filter

set_partition :: (a -> Bool) -> Set a -> (Set a, Set a)
set_partition = SSet.partition

set_foldl' :: (a -> b -> a) -> a -> Set b -> a
set_foldl' = SSet.foldl'

set_foldr :: (a -> b -> b) -> b -> Set a -> b
set_foldr = SSet.foldr

set_union :: Ord a => Set a -> Set a -> Set a
set_union = SSet.union

set_unions :: Ord a => [Set a] -> Set a
set_unions = SSet.unions

set_intersection :: Ord a => Set a -> Set a -> Set a
set_intersection = SSet.intersection

set_difference :: Ord a => Set a -> Set a -> Set a
set_difference = SSet.difference

set_isSubsetOf :: Ord a => Set a -> Set a -> Bool
set_isSubsetOf = SSet.isSubsetOf

set_disjoint :: Ord a => Set a -> Set a -> Bool
set_disjoint = SSet.disjoint


-- Seq

seq_empty :: Seq a
seq_empty = SSeq.empty

seq_singleton :: a -> Seq a
seq_singleton = SSeq.singleton

seq_fromList :: [a] -> Seq a
seq_fromList = SSeq.fromList

seq_toList :: Seq a -> [a]
seq_toList = foldr (:) []

seq_length :: Seq a -> Int
seq_length = SSeq.length

seq_null :: Seq a -> Bool
seq_null = SSeq.null

seq_index :: Seq a -> Int -> a
seq_index = SSeq.index

seq_append :: Seq a -> Seq a -> Seq a
seq_append = (><)
