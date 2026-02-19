module Filehub.Server.Download (download) where

import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT)
import Conduit qualified
import Control.Monad (forM, replicateM)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath (..))
import Data.ClientPath qualified as ClientPath
import Data.File (FileType(..), File(..))
import Data.Foldable (forM_)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Effectful ( MonadIO (liftIO) )
import Effectful.Error.Dynamic (throwError)
import Filehub.Error (FilehubError(..), Error'(..))
import Filehub.Handler (ConfirmLogin)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Prelude hiding (init, readFile)
import Servant (Headers, Header, addHeader)
import System.Directory (removeFile)
import System.FilePath (takeFileName, makeRelative)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Text.Printf (printf)
import Data.Coerce (coerce)


download :: SessionId -> ConfirmLogin -> [ClientPath]
         -> Filehub (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))
download sessionId _ clientPaths = do
  Session.withStorage sessionId \storage -> do
    root    <- Session.getRoot sessionId
    case clientPaths of
      [clientPath@(ClientPath path)] -> do
        mFile   <- storage.get (ClientPath.fromClientPath root clientPath)
        conduit <- storage.download clientPath
        case mFile of
          Just file -> do
            let filename =
                  case file.content of
                    Regular -> printf "attachement; filename=%s" (takeFileName path)
                    Dir     -> printf "attachement; filename=%s.zip" (takeFileName path)
            pure $ addHeader filename conduit
          Nothing -> do
            throwError (FilehubError InvalidPath "can't download, invalid file path")
      _ -> do
        (zipPath, _) <- liftIO do
          tempDir <- Temp.getCanonicalTemporaryDirectory
          Temp.openTempFile tempDir "DXXXXXX.zip"

        files <- traverse (storage.get . ClientPath.fromClientPath root) clientPaths <&> catMaybes

        tasks <- forM files \file -> do
          conduit <- storage.readStream file
          pure (file.path, conduit)

        Zip.createArchive zipPath do
          forM_ tasks \(path, conduit) -> do
            m <- Zip.mkEntrySelector  (coerce makeRelative root path)
            Zip.sinkEntry Zip.Zstd conduit m
        tag <- Text.pack <$> replicateM 8 (randomRIO ('a', 'z'))
        let conduit =
              Conduit.bracketP
                (pure ())
                (\_ -> liftIO $ removeFile zipPath)
                (\_ -> Conduit.sourceFile zipPath)
        pure $ addHeader (printf "attachement; filename=%s.zip" tag) conduit
