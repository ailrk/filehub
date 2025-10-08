module Filehub.Server.Download where

import Codec.Archive.Zip qualified as Zip
import Conduit (ConduitT, ResourceT)
import Conduit qualified
import Control.Monad (forM, replicateM)
import Data.ByteString (ByteString)
import Data.ClientPath (ClientPath (..))
import Data.ClientPath qualified as ClientPath
import Data.File (FileType(..), File(..))
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Text qualified as Text
import Effectful ( MonadIO (liftIO) )
import Filehub.Error ( withServerError, withServerError )
import Filehub.Handler (ConfirmLogin)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Session (SessionId(..))
import Filehub.Session qualified as Session
import Prelude hiding (init, readFile)
import Servant (addHeader)
import Servant (Headers, Header)
import System.Directory (removeFile)
import System.FilePath (takeFileName, makeRelative)
import System.IO.Temp qualified as Temp
import System.Random (randomRIO)
import Text.Printf (printf)


download :: SessionId -> ConfirmLogin -> [ClientPath]
         -> Filehub (Headers '[ Header "Content-Disposition" String ] (ConduitT () ByteString (ResourceT IO) ()))
download sessionId _ clientPaths = do
  storage <- Session.getStorage sessionId & withServerError
  root    <- Session.getRoot sessionId & withServerError
  case clientPaths of
    [clientPath@(ClientPath path)] -> do
      file    <- storage.get (ClientPath.fromClientPath root clientPath) & withServerError
      conduit <- withServerError (storage.download clientPath)
      let filename =
            case file.content of
              Regular -> printf "attachement; filename=%s" (takeFileName path)
              Dir     -> printf "attachement; filename=%s.zip" (takeFileName path)
      pure $ addHeader filename conduit
    _ -> do
      (zipPath, _) <- liftIO do
        tempDir <- Temp.getCanonicalTemporaryDirectory
        Temp.openTempFile tempDir "DXXXXXX.zip"
      tasks <- do
        forM (fmap (ClientPath.fromClientPath root) clientPaths) \path -> withServerError do
          file    <- storage.get path
          conduit <- storage.readStream file
          pure (path, conduit)
      Zip.createArchive zipPath do
        forM_ tasks \(path, conduit) -> do
          m <- Zip.mkEntrySelector  (makeRelative root path)
          Zip.sinkEntry Zip.Zstd conduit m
      tag <- Text.pack <$> replicateM 8 (randomRIO ('a', 'z'))
      let conduit =
            Conduit.bracketP
              (pure ())
              (\_ -> liftIO $ removeFile zipPath)
              (\_ -> Conduit.sourceFile zipPath)
      pure $ addHeader (printf "attachement; filename=%s.zip" tag) conduit
