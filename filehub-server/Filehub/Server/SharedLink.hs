module Filehub.Server.SharedLink where

import Data.ClientPath (ClientPath (..))
import Data.Set qualified as S
import Filehub.Error ( FilehubError(..) )
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Session (SessionId(..))
import Filehub.SharedLink (SharedLinkHash, SharedLinkPermitSet (..), SharedLinkPermit)
import Lucid hiding (for_)
import Prelude hiding (init, readFile)
import Servant (err303    , errHeaders   )
import UnliftIO (throwIO)
import Filehub.Session.Pool (withSession)
import Filehub.Session (Session(..))


-- TODO
-- client has no permit -> auth
-- client as permit
--      session has no permit -> auth
--      session has permit ->
--        sessiono permt != clinet permit -> auth
--        sessiono permt == clinet permit ->
--          link hash is not in permit set -> auth
--          link hash is in permit set ->
--             has client path ->
--                client path is folder index -> render folder
--                client path is file         -> serve file
--             no client path  -> render shared index
--
-- Shared link also have a session?
-- So can we reuse the session?
-- Normally session rely on a target
--
-- but shared link doesn't map to a target
-- so should we create a synthesized target that forwards the link?
shared :: SessionId -> Maybe SharedLinkPermit -> SharedLinkHash -> Maybe ClientPath -> Filehub (Html ())
shared sessionId mClientPermit hash mClientPath = do
  case mClientPermit of
    Just clientPermit -> do
      sharedLinkPermit <- withSession sessionId \s -> pure s.sharedLinkPermit
      case sharedLinkPermit of
        Just (SharedLinkPermitSet permit hashes)
          | clientPermit /= permit         -> goAuth
          | hash `S.member` hashes       -> goAuth
          | Just clientPath <- mClientPath -> do
              undefined
              -- TODO
              -- convert shared client path to normal storage path from SharedLink
              -- check if file exist
              -- check if file is a directory
              -- render index/serve file accordingly
          | otherwise -> sharedIndex
        Nothing -> goAuth
    Nothing -> goAuth
  where
    goAuth = throwIO do
      HTTPError err303 { errHeaders = [( "Location" , "/s/auth")] }

    sharedIndex = do
      pure mempty


sharedAuth :: SessionId -> Filehub (Html ())
sharedAuth sessionId = undefined
