{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Domain where

import Effectful.FileSystem
import Effectful.Reader.Dynamic (Reader)
import Effectful ((:>), Eff, IOE, MonadIO (liftIO))
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.FileSystem.IO (withFile, IOMode (..))
import Effectful.FileSystem.IO.ByteString.Lazy (hPut, readFile)
import Effectful.Concurrent.STM
import Control.Monad (unless, when, forM_)
import Text.Printf (printf)
import Filehub.Env (Env(..))
import Filehub.Env qualified as Env
import GHC.Generics
import Lens.Micro
import Lens.Micro.Platform ()
import Data.Time (UTCTime)
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as LText
import Data.List (sortOn, (\\))
import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import System.FilePath ((</>), takeFileName)
import Web.FormUrlEncoded (FromForm (..), parseUnique)
import Servant.Multipart (MultipartData(..), Mem, FileData (..))
import Prelude hiding (readFile, writeFile)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Codec.Archive.Zip qualified as Zip
import Codec.Archive.Zip (ZipOption(..))
import Network.Mime (defaultMimeLookup, MimeType)
import Lucid (Html, renderText)


------------------------------------
-- files related
------------------------------------


data FilehubError
  = FileExists
  | InvalidPath
  | InvalidDir
  deriving Show


instance ToHttpApiData FilehubError where
  toUrlPiece FileExists = Text.pack $ show FileExists
  toUrlPiece InvalidPath = Text.pack $ show InvalidPath
  toUrlPiece InvalidDir = Text.pack $ show InvalidDir


data FileContent
  = Content
  | Dir (Maybe [File])
  deriving (Show, Generic)


data File = File
  { path :: FilePath
  , atime :: UTCTime
  , mtime :: UTCTime
  , size :: Integer
  , mimetype :: MimeType
  , content :: FileContent
  }
  deriving (Show, Generic)


getFile :: (FileSystem :> es, Error FilehubError :> es) => FilePath -> Eff es File
getFile path = do
  exists <- doesPathExist path
  unless exists do
    throwError InvalidPath
  size <- getFileSize path
  mtime <- getModificationTime path
  atime <- getAccessTime path
  isDir <- isDirectory path
  let mimetype = defaultMimeLookup (Text.pack path)
  pure File
    { path = path
    , size = size
    , mtime = mtime
    , atime = atime
    , mimetype = mimetype
    , content = if isDir then Dir Nothing else Content
    }


isDirectory :: (FileSystem :> es) => FilePath -> Eff es Bool
isDirectory filePath = do
  pathExists <- doesPathExist filePath
  dirExists <- doesDirectoryExist filePath
  if not pathExists
     then pure False
     else pure dirExists


readFileContent :: (FileSystem :> es) => File -> Eff es LBS.ByteString
readFileContent file = readFile file.path


loadDirContents :: (FileSystem :> es, Error FilehubError :> es) => File -> Eff es File
loadDirContents file = do
  case file.content of
    Dir Nothing -> do
      files <- lsDir file.path
      pure $ file & #content .~ Dir (Just files)
    _ -> pure file


dirtree :: Traversal' File File
dirtree f = \case
  file@File { content = Content } -> f file
  file@File { content = Dir Nothing } -> f file
  file@File { content = Dir (Just files) } -> do
    let file' = f file
    let fs = traverse (dirtree f) files
    let modify f'@File { content = Dir Nothing } _ = f'
        modify f' xs = f' & #content . #_Dir ?~ xs
    modify <$> file' <*> fs


toFilePath :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => FilePath -> Eff es FilePath
toFilePath name = do
  currentDir <- Env.getCurrentDir
  makeAbsolute (currentDir </> name)


newFolder :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error FilehubError :> es) => String -> Eff es ()
newFolder name = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  createDirectoryIfMissing True filePath


newFile :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error FilehubError :> es) => String -> Eff es ()
newFile name = do
  filePath <- toFilePath name
  exists <- doesFileExist filePath
  when exists do
    throwError FileExists
  withFile filePath ReadWriteMode (\_ -> pure ())


writeFile :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => String -> LBS.ByteString -> Eff es ()
writeFile name content = do
  filePath <- toFilePath name
  withFile filePath ReadWriteMode (\h -> hPut h content)


deleteFile :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => String -> Eff es ()
deleteFile name = do
  filePath <- toFilePath name
  fileExists <- doesFileExist filePath
  dirExists <- doesDirectoryExist filePath
  if
     | fileExists -> removeFile filePath
     | dirExists -> removeDirectoryRecursive filePath
     | otherwise -> pure ()


lsDir :: (FileSystem :> es, Error FilehubError :> es) => FilePath -> Eff es [File]
lsDir path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  withCurrentDirectory path $
    listDirectory path
      >>= traverse makeAbsolute
      >>= traverse getFile


changeDir :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error FilehubError :> es) => FilePath -> Eff es ()
changeDir path = do
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  Env.setCurrentDir path


lsCurrentDir :: (Reader Env :> es, Concurrent :> es, FileSystem :> es, Error FilehubError :> es) => Eff es [File]
lsCurrentDir = do
  path <- Env.getCurrentDir
  exists <- doesDirectoryExist path
  unless exists do
    throwError InvalidDir
  lsDir path


upload :: (Reader Env :> es, Concurrent :> es, FileSystem :> es) => MultipartData Mem -> Eff es ()
upload multipart = do
  forM_ multipart.files $ \file -> do
    let name = Text.unpack file.fdFileName
    let content = file.fdPayload
    writeFile name content


download :: (Reader Env :> es, Error FilehubError :> es, FileSystem :> es, IOE :> es) => ClientPath -> Eff es LBS.ByteString
download clientPath = do
  root <- Env.getRoot
  let abspath = fromClientPath root clientPath
  file <- getFile abspath
  case file.content of
    Content -> readFileContent file
    Dir _ -> do
      archive <- liftIO $ Zip.addFilesToArchive [OptRecursive, OptPreserveSymbolicLinks] Zip.emptyArchive [file.path]
      pure $ Zip.fromArchive archive



------------------------------------
-- new types
------------------------------------


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)
instance FromForm SearchWord where fromForm f = SearchWord <$> parseUnique "search" f


newtype NewFile = NewFile Text deriving (Show, Eq, Generic)
instance FromForm NewFile where fromForm f = NewFile <$> parseUnique "new-file" f


newtype NewFolder = NewFolder Text deriving (Show, Eq, Generic)
instance FromForm NewFolder where fromForm f = NewFolder <$> parseUnique "new-folder" f


data UpdatedFile = UpdatedFile
  { clientPath :: ClientPath
  , content :: Text
  }
  deriving (Show, Eq, Generic)
instance FromForm UpdatedFile where
  fromForm f = do
    path <- parseUnique "path" f
    content <- parseUnique "content" f
    pure (UpdatedFile path content)


------------------------------------
-- table sort
------------------------------------


data SortFileBy
  = ByName
  | ByModified
  | BySize
  deriving (Show)


instance ToHttpApiData SortFileBy where
  toUrlPiece ByName = "name"
  toUrlPiece ByModified = "modified"
  toUrlPiece BySize = "size"


instance FromHttpApiData SortFileBy where
  parseUrlPiece "name" = pure ByName
  parseUrlPiece "modified" = pure ByModified
  parseUrlPiece "size" = pure BySize
  parseUrlPiece _ = Left "Unknown order"


sortFiles :: SortFileBy -> [File] -> [File]
sortFiles ByName = sortOn (takeFileName . (.path))
sortFiles ByModified = sortOn (.mtime)
sortFiles BySize = sortOn (.size)


------------------------------------
-- Client Path
------------------------------------


-- | Filepath without the root part. The path is safe to show in the frontend.
newtype ClientPath = ClientPath { unClientPath :: FilePath }
  deriving (Show, Eq, Semigroup, Monoid)


instance ToHttpApiData ClientPath where
  toUrlPiece (ClientPath p) = toUrlPiece p


instance FromHttpApiData ClientPath where
  parseUrlPiece p = ClientPath <$> parseUrlPiece p


toClientPath :: FilePath -> FilePath -> ClientPath
toClientPath root path =
  let p = path \\ root
   in ClientPath $
     case p of
       '/':p' -> p'
       _ -> '/' : p


fromClientPath :: FilePath -> ClientPath -> FilePath
fromClientPath root (ClientPath cp) =
  root </>
    case cp of
      '/': cp' -> cp'
      _ -> cp


------------------------------------
-- Readable size
------------------------------------


toReadableSize :: Integer -> String
toReadableSize nbytes =
  if | nB == 0 -> "0b"
     | nTb >= 1 -> printf "%.1fTB" (nTb :: Double)
     | nGb >= 1 -> printf "%.1fGB" (nGb :: Double)
     | nMb >= 1 -> printf "%.1fMB" (nMb :: Double)
     | nKb >= 1 -> printf "%.1fKB" (nKb :: Double)
     | nB >= 1 -> printf "%.0fB" (nB :: Double)
     | otherwise -> "unknown"

  where
    nB  = fromIntegral nbytes
    nKb = fromIntegral nbytes / (2 ^ 10)
    nMb = fromIntegral nbytes / (2 ^ 20)
    nGb = fromIntegral nbytes / (2 ^ 30)
    nTb = fromIntegral nbytes / (2 ^ 40)


------------------------------------
-- Image Viewer
------------------------------------

data ViewImage
  = ViewerJS
      { elementId :: Text
      , html :: Html ()
      }
  deriving Show


instance ToJSON ViewImage where
  toJSON ViewerJS { elementId, html } =
    Aeson.object
      [ "ViewerJS" .= Aeson.object
          [ "elementId" .= elementId
          , "html" .= renderText html
          ]
      ]


instance ToHttpApiData ViewImage where
  toUrlPiece v = (v & Aeson.encode & LText.decodeUtf8) ^. strict


------------------------------------
-- Theme
------------------------------------


data Theme = Dark1 | Dark2 | Dark3 | Light1 | Light2 | Light3


instance Show Theme where
  show = \case
    Dark1 -> "dark1"
    Dark2 -> "dark2"
    Dark3 -> "dark3"
    Light1 -> "light1"
    Light2 -> "light2"
    Light3 -> "light3"


instance Read Theme where
  readsPrec _ s = do
    let theme =
          case s of
          "dark1" -> Dark1
          "dark2" -> Dark2
          "dark3" -> Dark3
          "light1" -> Light1
          "light2" -> Light2
          "light3" -> Light3
          _ -> defaultTheme
    pure (theme, "")


defaultTheme :: Theme
defaultTheme = Dark1
