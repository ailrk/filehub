module Text.Toml
  ( TomlValue(..)
  , TomlTable
  , TomlError(..)
  , parse
  , parseFile
  , encode
  , getText
  , getString
  , getInt
  , getBool
  , getTable
  , getArrayOfTables
  , getArray
  , getTextArray
  , getStringArray
  , getOptText
  , getOptInt
  , getOptBool
  , getOptTable
  ) where

import Data.Char (isDigit, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO


type TomlTable = Map Text TomlValue

data TomlValue
  = TString Text
  | TInt    Int
  | TBool   Bool
  | TArray  [TomlValue]
  | TTable  TomlTable
  | TArrayOfTables [TomlTable]
  deriving (Show, Eq)

newtype TomlError = TomlError Text
  deriving (Show)


------------------------------------------------------------
-- Public API

parse :: Text -> Either TomlError TomlTable
parse = fmap buildDoc . parseSections

parseFile :: FilePath -> IO (Either TomlError TomlTable)
parseFile path = parse <$> TIO.readFile path

encode :: TomlTable -> Text
encode tbl =
  T.unlines $ scalarsOut ++ sectionsOut
  where
    entries     = Map.toList tbl
    scalarsOut  = map encodeKV (filter (isScalar . snd) entries)
    sectionsOut = concatMap (encodeSection "") (filter (not . isScalar . snd) entries)


------------------------------------------------------------
-- Accessors

getText :: Text -> TomlTable -> Either TomlError Text
getText k tbl = case Map.lookup k tbl of
  Just (TString t) -> Right t
  Just _           -> Left (TomlError (k <> ": expected string"))
  Nothing          -> Left (TomlError (k <> ": key not found"))

getString :: Text -> TomlTable -> Either TomlError String
getString k tbl = T.unpack <$> getText k tbl

getInt :: Text -> TomlTable -> Either TomlError Int
getInt k tbl = case Map.lookup k tbl of
  Just (TInt i) -> Right i
  Just _        -> Left (TomlError (k <> ": expected int"))
  Nothing       -> Left (TomlError (k <> ": key not found"))

getBool :: Text -> TomlTable -> Either TomlError Bool
getBool k tbl = case Map.lookup k tbl of
  Just (TBool b) -> Right b
  Just _         -> Left (TomlError (k <> ": expected bool"))
  Nothing        -> Left (TomlError (k <> ": key not found"))

getTable :: Text -> TomlTable -> Either TomlError TomlTable
getTable k tbl = case Map.lookup k tbl of
  Just (TTable t) -> Right t
  Just _          -> Left (TomlError (k <> ": expected table"))
  Nothing         -> Left (TomlError (k <> ": key not found"))

getArrayOfTables :: Text -> TomlTable -> Either TomlError [TomlTable]
getArrayOfTables k tbl = case Map.lookup k tbl of
  Just (TArrayOfTables ts) -> Right ts
  Nothing                  -> Right []
  Just _                   -> Left (TomlError (k <> ": expected array of tables"))

getArray :: Text -> TomlTable -> Either TomlError [TomlValue]
getArray k tbl = case Map.lookup k tbl of
  Just (TArray vs) -> Right vs
  Nothing          -> Right []
  Just _           -> Left (TomlError (k <> ": expected array"))

getTextArray :: Text -> TomlTable -> Either TomlError [Text]
getTextArray k tbl = do
  vs <- getArray k tbl
  traverse (\case TString t -> Right t; _ -> Left (TomlError (k <> ": expected string in array"))) vs

getStringArray :: Text -> TomlTable -> Either TomlError [String]
getStringArray k tbl = map T.unpack <$> getTextArray k tbl

getOptText :: Text -> TomlTable -> Either TomlError (Maybe Text)
getOptText k tbl = case Map.lookup k tbl of
  Nothing          -> Right Nothing
  Just (TString t) -> Right (Just t)
  Just _           -> Left (TomlError (k <> ": expected string"))

getOptInt :: Text -> TomlTable -> Either TomlError (Maybe Int)
getOptInt k tbl = case Map.lookup k tbl of
  Nothing       -> Right Nothing
  Just (TInt i) -> Right (Just i)
  Just _        -> Left (TomlError (k <> ": expected int"))

getOptBool :: Text -> TomlTable -> Either TomlError (Maybe Bool)
getOptBool k tbl = case Map.lookup k tbl of
  Nothing        -> Right Nothing
  Just (TBool b) -> Right (Just b)
  Just _         -> Left (TomlError (k <> ": expected bool"))

getOptTable :: Text -> TomlTable -> Either TomlError (Maybe TomlTable)
getOptTable k tbl = case Map.lookup k tbl of
  Nothing         -> Right Nothing
  Just (TTable t) -> Right (Just t)
  Just _          -> Left (TomlError (k <> ": expected table"))


------------------------------------------------------------
-- Section model

data SectionHeader = SHRoot | SHTable [Text] | SHAoT [Text]

data Section = Section SectionHeader [(Text, TomlValue)]


buildDoc :: [Section] -> TomlTable
buildDoc = foldl' applySection Map.empty


applySection :: TomlTable -> Section -> TomlTable
applySection root (Section hdr kvs) =
  let content = Map.fromList kvs
  in case hdr of
    SHRoot       -> Map.union content root
    SHTable path -> applyTableAt path content root
    SHAoT path   -> applyAoTAt  path content root


-- Navigate to a [table] path and merge content. Intermediate arrays are
-- entered via their last element.
applyTableAt :: [Text] -> TomlTable -> TomlTable -> TomlTable
applyTableAt []     content root = Map.union content root
applyTableAt [k]    content root =
  Map.alter (\case
    Just (TTable existing) -> Just (TTable (Map.union content existing))
    _                      -> Just (TTable content)
  ) k root
applyTableAt (k:ks) content root =
  Map.alter (\case
    Just (TArrayOfTables ts) | not (null ts) ->
      let lst = last ts
      in Just (TArrayOfTables (init ts ++ [applyTableAt ks content lst]))
    Just (TTable sub) -> Just (TTable (applyTableAt ks content sub))
    _                 -> Just (TTable (applyTableAt ks content Map.empty))
  ) k root


-- Append a new element to a [[aot]] path. Intermediate arrays are entered
-- via their last element.
applyAoTAt :: [Text] -> TomlTable -> TomlTable -> TomlTable
applyAoTAt []     _       root = root
applyAoTAt [k]    content root =
  Map.alter (\case
    Just (TArrayOfTables ts) -> Just (TArrayOfTables (ts ++ [content]))
    _                        -> Just (TArrayOfTables [content])
  ) k root
applyAoTAt (k:ks) content root =
  Map.alter (\case
    Just (TArrayOfTables ts) | not (null ts) ->
      let lst = last ts
      in Just (TArrayOfTables (init ts ++ [applyAoTAt ks content lst]))
    Just (TTable sub) -> Just (TTable (applyAoTAt ks content sub))
    _                 -> Nothing
  ) k root


------------------------------------------------------------
-- Parser

parseSections :: Text -> Either TomlError [Section]
parseSections text =
  go (T.unpack (normalizeNL text)) SHRoot [] []
  where
    go :: String -> SectionHeader -> [(Text, TomlValue)] -> [Section] -> Either TomlError [Section]
    go [] hdr kvs acc =
      Right (reverse (Section hdr (reverse kvs) : acc))

    go cs hdr kvs acc =
      case dropHSpace cs of
        []              -> go [] hdr kvs acc
        ('\n'    :rest) -> go rest hdr kvs acc
        ('#'     :rest) -> go (dropLine rest) hdr kvs acc
        ('[':'[':rest)  ->
          let (pathStr, rest') = break (== ']') rest
              rest''           = drop 2 rest'
          in case parsePath pathStr of
               Left e     -> Left e
               Right path ->
                 let newSec = Section hdr (reverse kvs)
                 in go rest'' (SHAoT path) [] (newSec : acc)
        ('['     :rest) ->
          let (pathStr, rest') = break (== ']') rest
              rest''           = drop 1 rest'
          in case parsePath pathStr of
               Left e     -> Left e
               Right path ->
                 let newSec = Section hdr (reverse kvs)
                 in go rest'' (SHTable path) [] (newSec : acc)
        cs' ->
          let (keyStr, rest0) = span isKeyChar cs'
              rest1            = dropHSpace rest0
          in case rest1 of
               ('=':rest2) ->
                 let key   = T.pack (trimStr keyStr)
                     rest3 = dropHSpace rest2
                 in if T.null key
                    then go (dropLine cs') hdr kvs acc
                    else case parseValue rest3 of
                      Left e          -> Left e
                      Right (v, rest4) ->
                        go (skipToNL rest4) hdr ((key, v) : kvs) acc
               _ -> go (dropLine cs') hdr kvs acc

    isKeyChar c = c /= '=' && c /= '\n' && c /= '#'
    dropHSpace  = dropWhile (\c -> c == ' ' || c == '\t')
    dropLine    = dropWhile (/= '\n')
    trimStr     = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    skipToNL ('#':cs) = dropLine cs
    skipToNL (' ':cs) = skipToNL cs
    skipToNL ('\t':cs) = skipToNL cs
    skipToNL cs       = cs


parsePath :: String -> Either TomlError [Text]
parsePath s =
  let segs = map (T.pack . trim) (splitDot s)
  in if any T.null segs
     then Left (TomlError ("invalid path: " <> T.pack s))
     else Right segs
  where
    trim     = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    splitDot []       = [""]
    splitDot ('.':cs) = "" : splitDot cs
    splitDot (c  :cs) = case splitDot cs of (h:t) -> (c:h) : t; [] -> [[c]]


parseValue :: String -> Either TomlError (TomlValue, String)
parseValue [] = Left (TomlError "empty value")
parseValue ('"':'"':'"':rest) = parseMLString rest
parseValue ('"':rest)         = parseStr rest
parseValue ('t':'r':'u':'e':rest)  = Right (TBool True,  rest)
parseValue ('f':'a':'l':'s':'e':rest) = Right (TBool False, rest)
parseValue ('[':rest)         = parseArray (dropWhile isSpace rest)
parseValue s@(c:_)
  | isDigit c || c == '-' = parseInt s
  | otherwise = Left (TomlError ("unexpected value: " <> T.pack (take 20 s)))


parseStr :: String -> Either TomlError (TomlValue, String)
parseStr = go []
  where
    go _   []              = Left (TomlError "unterminated string")
    go acc ('"'      :rest) = Right (TString (T.pack (reverse acc)), rest)
    go acc ('\\':'n' :rest) = go ('\n':acc) rest
    go acc ('\\':'t' :rest) = go ('\t':acc) rest
    go acc ('\\':'r' :rest) = go ('\r':acc) rest
    go acc ('\\'     :c:rest) = go (c:acc) rest
    go acc (c        :rest) = go (c:acc) rest


parseMLString :: String -> Either TomlError (TomlValue, String)
parseMLString s =
  let s' = case s of '\n':t -> t; _ -> s
  in go [] s'
  where
    go _   []                     = Left (TomlError "unterminated multiline string")
    go acc ('"':'"':'"':rest)     = Right (TString (T.pack (reverse acc)), rest)
    go acc (c  :rest)             = go (c:acc) rest


parseInt :: String -> Either TomlError (TomlValue, String)
parseInt s =
  let (numStr, rest) = span (\c -> isDigit c || c == '-') s
  in case reads numStr :: [(Int, String)] of
    [(n, "")] -> Right (TInt n, rest)
    _         -> Left (TomlError ("invalid integer: " <> T.pack numStr))


parseArray :: String -> Either TomlError (TomlValue, String)
parseArray = go []
  where
    go _   []        = Left (TomlError "unterminated array")
    go acc (']':rest) = Right (TArray (reverse acc), rest)
    go acc (',':rest) = go acc (dropWhile isSpace rest)
    go acc s =
      case parseValue s of
        Left e          -> Left e
        Right (v, rest) -> go (v:acc) (dropWhile isSpace rest)


normalizeNL :: Text -> Text
normalizeNL = T.replace "\r\n" "\n" . T.replace "\r" "\n"


------------------------------------------------------------
-- Encoder

isScalar :: TomlValue -> Bool
isScalar (TTable _)         = False
isScalar (TArrayOfTables _) = False
isScalar _                  = True


encodeKV :: (Text, TomlValue) -> Text
encodeKV (k, v) = k <> " = " <> encodeVal v


encodeVal :: TomlValue -> Text
encodeVal (TString t)
  | T.any (== '\n') t = "\"\"\"\n" <> t <> "\"\"\""
  | otherwise         = "\"" <> escapeStr t <> "\""
encodeVal (TInt i)    = T.pack (show i)
encodeVal (TBool b)   = if b then "true" else "false"
encodeVal (TArray vs) = "[" <> T.intercalate ", " (map encodeVal vs) <> "]"
encodeVal _           = ""


escapeStr :: Text -> Text
escapeStr = T.concatMap go
  where
    go '"'  = "\\\""
    go '\\' = "\\\\"
    go '\n' = "\\n"
    go '\t' = "\\t"
    go '\r' = "\\r"
    go c    = T.singleton c


encodeSection :: String -> (Text, TomlValue) -> [Text]
encodeSection parent (k, val) =
  let path = if null parent then T.unpack k else parent ++ "." ++ T.unpack k
  in case val of
    TTable sub ->
      "" : ("[" <> T.pack path <> "]") : encodeBody path sub
    TArrayOfTables [] -> []
    TArrayOfTables tbls ->
      concatMap (encodeAoTElem path) tbls
    _ -> []


encodeAoTElem :: String -> TomlTable -> [Text]
encodeAoTElem path tbl =
  "" : ("[[" <> T.pack path <> "]]") : encodeBody path tbl


encodeBody :: String -> TomlTable -> [Text]
encodeBody path tbl =
  let entries  = Map.toList tbl
      scalars  = filter (isScalar . snd) entries
      sections = filter (not . isScalar . snd) entries
  in map encodeKV scalars ++ concatMap (encodeSection path) sections
