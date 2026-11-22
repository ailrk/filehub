{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.SharedLink
  ( shared
  , sharedFile
  , sharedPreview
  , sharedAuth
  , sharedAuthPost
  , share
  , shareForFile
  , shareAuto
  , shareLinks
  , revokeLink
  , editLinkForm
  , shareLinkRow
  , updateLink
  ) where

import Control.Concurrent.STM (throwSTM)
import Control.Monad.Reader (MonadReader(..))
import Data.ClientPath (ClientPath(..), AbsPath(..))
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (addUTCTime, getCurrentTime, UTCTime)
import Network.Mime.Extended (isMime)
import Data.UUID qualified as UUID
import Filehub.Cookie qualified as Cookies
import Control.Handle.Storage (Storage(..))
import Conduit (ConduitT, ResourceT)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.File (File(..), FileType(..))
import System.FilePath (takeFileName)
import Filehub.Env (Env(..))
import Filehub.Error (FilehubError(..))
import Filehub.Handler (ConfirmLogin, ConfirmRateLimitSharedLink(..))
import Filehub.History (Operation(..), recordHistory)
import Filehub.Monad
import Filehub.Session (SessionId(..), makeStorageAt, targetRoot, fromClientPathChecked)
import Filehub.Session.Pool (withSession, modifySession)
import Filehub.Session.Types (Session(..))
import Filehub.SharedLink
import Filehub.Sort (sortFiles, SortFileBy(..))
import Filehub.Template.SharedView qualified as Template.Shared
import Filehub.Types (ShareRequest(..), ShareLinkType(..), SharedLinkAuth(..), SharedLinkUpdate(..))
import Lucid hiding (for_)
import Network.HTTP.Types.Header (hLocation, hSetCookie)
import Prelude hiding (init, readFile)
import Servant (addHeader, Header, Headers, err303, err400, err404, err403, errBody, errHeaders)
import Control.Monad (unless, when)
import UnliftIO (MonadIO(..), atomically, readTVar, throwIO, tryIO, tryAny)
import Web.Cookie (SetCookie(..), defaultSetCookie)


-- | Serve a shared link: show the folder browser or redirect to auth.
shared :: SessionId -> Maybe SharedLinkPermit -> ConfirmRateLimitSharedLink -> SharedLinkHash -> Maybe ClientPath -> Filehub (Html ())
shared sessionId mClientPermit _ hash mClientPath = do
  env  <- ask
  let pool = env.sharedLinkPool
  mLink <- lookupSharedLink hash pool
  case mLink of
    Nothing -> throwIO (HTTPError err404 { errBody = "Link not found or expired" })
    Just link
      | link.revoked -> throwIO (HTTPError err403 { errBody = "Link revoked" })
      | otherwise -> do
          authorized <- checkPermit sessionId mClientPermit hash
          if authorized
            then renderShared link mClientPath
            else if singleUseConsumed link
              then throwIO (HTTPError err403 { errBody = "Single-use link already used" })
              else goAuth hash


checkPermit :: SessionId -> Maybe SharedLinkPermit -> SharedLinkHash -> Filehub Bool
checkPermit sessionId mClientPermit hash = do
  mPermitSet <- withSession sessionId (pure . (.sharedLinkPermit))
  case (mClientPermit, mPermitSet) of
    (Just clientPermit, Just (SharedLinkPermitSet sessionPermit hashes))
      | clientPermit == sessionPermit && hash `Set.member` hashes -> pure True
    _ -> pure False


renderShared :: SharedLink -> Maybe ClientPath -> Filehub (Html ())
renderShared link mClientPath = do
  env <- ask
  targets <- atomically (readTVar env.targets)
  case lookup link.targetId targets of
    Nothing -> throwIO (HTTPError err404 { errBody = "Target not found" })
    Just target -> do
      let root       = targetRoot target
          sharedRoot = link.file.path
      case link.file.content of
        Regular ->
          pure (Template.Shared.sharedPage link target [link.file] mClientPath)
        Dir -> do
          dir <- case mClientPath of
                   Nothing -> pure sharedRoot
                   Just cp -> do
                     p <- fromClientPathChecked root cp
                     pure (if isWithin sharedRoot p then p else sharedRoot)
          storage <- makeStorageAt target dir
          files   <- sortFiles ByNameUp <$> storage.ls dir
          pure (Template.Shared.sharedPage link target files mClientPath)


-- | Serve a single file from a shared link.
-- When `inline` is True, Content-Disposition is set to inline (useful for PDF iframes).
sharedFile :: SessionId -> Maybe SharedLinkPermit -> ConfirmRateLimitSharedLink -> SharedLinkHash -> Bool -> Maybe ClientPath
           -> Filehub (Headers '[Header "Content-Disposition" String] (ConduitT () ByteString (ResourceT IO) ()))
sharedFile sessionId mClientPermit _ hash inline mClientPath = do
  env <- ask
  let pool = env.sharedLinkPool
  mLink <- lookupSharedLink hash pool
  case mLink of
    Nothing -> throwIO (HTTPError err404 { errBody = "Link not found or expired" })
    Just link
      | link.revoked        -> throwIO (HTTPError err403 { errBody = "Link revoked" })
      | singleUseConsumed link -> throwIO (HTTPError err403 { errBody = "Single-use link already used" })
      | otherwise -> do
          authorized <- case link.passcode of
            Nothing -> pure True  -- no passcode = public link, skip permit check
            Just _  -> checkPermit sessionId mClientPermit hash
          unless authorized $ goAuth hash
          clientPath <- case mClientPath of
            Nothing -> throwIO (HTTPError err400 { errBody = "No file specified" })
            Just cp -> pure cp
          targets <- atomically (readTVar env.targets)
          case lookup link.targetId targets of
            Nothing -> throwIO (HTTPError err404 { errBody = "Target not found" })
            Just target -> do
              let root       = targetRoot target
                  sharedRoot = link.file.path
              absPath <- fromClientPathChecked root clientPath
              unless (isWithin sharedRoot absPath) $
                throwIO (HTTPError err403 { errBody = "Path not within shared root" })
              storage <- makeStorageAt target sharedRoot
              file    <- storage.get absPath
              case file.content of
                Dir     -> throwIO (HTTPError err400 { errBody = "Cannot download a directory" })
                Regular -> do
                  conduit <- storage.download clientPath
                  let ClientPath cp = clientPath
                      filename      = takeFileName (coerce cp)
                      disposition   = if inline
                                        then "inline; filename=" ++ filename
                                        else "attachment; filename=" ++ filename
                  pure $ addHeader disposition conduit


-- | Render a preview page for a single file in a shared link.
sharedPreview :: SessionId -> Maybe SharedLinkPermit -> ConfirmRateLimitSharedLink -> SharedLinkHash -> Maybe ClientPath -> Filehub (Html ())
sharedPreview sessionId mClientPermit _ hash mClientPath = do
  env <- ask
  let pool = env.sharedLinkPool
  mLink <- lookupSharedLink hash pool
  case mLink of
    Nothing -> throwIO (HTTPError err404 { errBody = "Link not found or expired" })
    Just link
      | link.revoked -> throwIO (HTTPError err403 { errBody = "Link revoked" })
      | otherwise -> do
          authorized <- checkPermit sessionId mClientPermit hash
          unless authorized $
            if singleUseConsumed link
              then throwIO (HTTPError err403 { errBody = "Single-use link already used" })
              else goAuth hash
          clientPath <- case mClientPath of
            Nothing -> throwIO (HTTPError err400 { errBody = "No file specified" })
            Just cp -> pure cp
          targets <- atomically (readTVar env.targets)
          case lookup link.targetId targets of
            Nothing -> throwIO (HTTPError err404 { errBody = "Target not found" })
            Just target -> do
              let root       = targetRoot target
                  sharedRoot = link.file.path
              absPath <- fromClientPathChecked root clientPath
              unless (isWithin sharedRoot absPath) $
                throwIO (HTTPError err403 { errBody = "Path not within shared root" })
              storage <- makeStorageAt target sharedRoot
              file    <- storage.get absPath
              case file.content of
                Dir -> throwIO (HTTPError err400 { errBody = "Cannot preview a directory" })
                Regular -> do
                  mText <- if file.mimetype `isMime` "text"
                    then do
                      bytes <- storage.read file
                      pure (Just (T.decodeUtf8With lenientDecode bytes))
                    else pure Nothing
                  pure (Template.Shared.sharedPreviewPage link target file clientPath mText)


singleUseConsumed :: SharedLink -> Bool
singleUseConsumed link = link.linkType == SingleUse && link.accessCount > 0


isWithin :: AbsPath -> AbsPath -> Bool
isWithin (AbsPath root) (AbsPath path) = (root ++ "/") `isPrefixOf` path || root == path


goAuth :: SharedLinkHash -> Filehub a
goAuth (SharedLinkHash hashText) =
  throwIO (HTTPError err303
    { errHeaders = [(hLocation, "/s/auth?hash=" <> T.encodeUtf8 hashText)] })


-- | Show the auth page (passcode form) or auto-issue a permit if no passcode is required.
sharedAuth :: SessionId -> ConfirmRateLimitSharedLink -> Maybe SharedLinkHash -> Filehub (Html ())
sharedAuth sessionId _ mHash = do
  case mHash of
    Nothing -> pure noLinkPage
    Just hash -> do
      env <- ask
      let pool = env.sharedLinkPool
      mLink <- lookupSharedLink hash pool
      case mLink of
        Nothing   -> pure expiredPage
        Just link
          | singleUseConsumed link -> pure usedPage
          | otherwise ->
              case link.passcode of
                Nothing -> autoIssue sessionId hash link
                Just _  -> pure (Template.Shared.sharedAuthPage hash False)
  where
    noLinkPage  = toHtml ("No link specified." :: Text)
    expiredPage = toHtml ("This link has expired or does not exist." :: Text)
    usedPage    = toHtml ("This single-use link has already been used." :: Text)


autoIssue :: SessionId -> SharedLinkHash -> SharedLink -> Filehub (Html ())
autoIssue sessionId hash link = do
  cookie <- performIssue sessionId hash
  when (link.linkType == SingleUse) do
    env <- ask
    consumeSharedLink hash env.sharedLinkPool
  let SharedLinkHash hashText = hash
  throwIO (HTTPError err303
    { errHeaders = [ (hLocation, "/s/" <> T.encodeUtf8 hashText)
                   , (hSetCookie, Cookies.renderSetCookie cookie)
                   ] })


-- | Handle passcode submission for a shared link.
sharedAuthPost :: SessionId -> ConfirmRateLimitSharedLink -> SharedLinkAuth -> Filehub (Html ())
sharedAuthPost sessionId _ SharedLinkAuth { hash = hashText, passcode = submittedPasscode } = do
  let hash = SharedLinkHash hashText
  env <- ask
  let pool = env.sharedLinkPool
  mLink <- lookupSharedLink hash pool
  case mLink of
    Nothing   -> pure (toHtml ("This link has expired or does not exist." :: Text))
    Just link ->
      case link.passcode of
        Nothing ->
          autoIssue sessionId hash link
        Just (SharedLinkPasscode expected) ->
          if submittedPasscode == expected
            then do
              cookie <- performIssue sessionId hash
              when (link.linkType == SingleUse) do
                consumeSharedLink hash pool
              throwIO (HTTPError err303
                { errHeaders = [ (hLocation, "/s/" <> T.encodeUtf8 hashText)
                               , (hSetCookie, Cookies.renderSetCookie cookie)
                               ] })
            else
              pure (Template.Shared.sharedAuthPage hash True)


performIssue :: SessionId -> SharedLinkHash -> Filehub SetCookie
performIssue sessionId hash = do
  mExisting <- withSession sessionId (pure . (.sharedLinkPermit))
  (SharedLinkPermit permitUuid, newSet) <- issuePermit mExisting hash
  modifySession sessionId \s ->
    pure $ s { sharedLinkPermit = Just newSet }
  pure defaultSetCookie
    { setCookieName     = "sharedlink_permit"
    , setCookieValue    = UUID.toASCIIBytes permitUuid
    , setCookieHttpOnly = True
    , setCookiePath     = Just "/"
    , setCookieSecure   = False
    }


-- | Create a new shared link for a file/directory.
share :: SessionId -> ConfirmLogin -> ShareRequest -> Filehub (Html ())
share sessionId _ req = do
  env  <- ask
  let pool = env.sharedLinkPool
  targets <- atomically (readTVar env.targets)

  currentTargetId <- withSession sessionId \s ->
    readTVar s.currentTargetId

  case lookup currentTargetId targets of
    Nothing -> throwIO (HTTPError err404 { errBody = "Target not found" })
    Just target -> do
      let root = targetRoot target
      absPath <- fromClientPathChecked root req.file
      storage <- makeStorageAt target absPath
      file    <- storage.get absPath
      linkType <- mkLinkType req
      let mPasscode = SharedLinkPasscode <$> req.passcode
      link <- createSharedLink file currentTargetId linkType req.readonly mPasscode
      addSharedLink link pool
      recordHistory sessionId (OpShare currentTargetId req.file link.hash) True
      pure (Template.Shared.shareCreatedModal link.hash)
  where
    mkLinkType ShareRequest { linkType, expiryHours } = case linkType of
      ShareSingleUse  -> pure SingleUse
      ShareIndefinite -> pure Indefinite
      ShareExpiry     -> do
        let hours = maybe 24 id expiryHours
        now <- liftIO getCurrentTime
        pure (Expiry (fromIntegral (hours * 3600) `addUTCTime` now))


-- | Return the hash of an active no-passcode share link for the given file, or 404.
shareForFile :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub Text
shareForFile sessionId _ mClientPath = do
  clientPath <- case mClientPath of
    Nothing -> throwIO (HTTPError err400 { errBody = "No file specified" })
    Just cp -> pure cp
  env <- ask
  currentTargetId <- withSession sessionId \s -> readTVar s.currentTargetId
  root <- atomically $ do
    targets <- readTVar env.targets
    case lookup currentTargetId targets of
      Nothing -> throwSTM (HTTPError err404 { errBody = "Target not found" })
      Just tgt -> pure (targetRoot tgt)
  absPath <- fromClientPathChecked root clientPath
  now <- liftIO getCurrentTime
  all_ <- getAllSharedLinks env.sharedLinkPool
  let active l = not l.revoked
              && l.targetId == currentTargetId
              && l.file.path == absPath
              && isNothing l.passcode
              && case l.linkType of { Expiry t -> t > now; _ -> True }
  case filter active all_ of
    SharedLink { hash = SharedLinkHash h } : _ -> pure h
    []                                         -> throwIO (HTTPError err404 { errBody = "No share link found" })


-- | Return existing or create new indefinite readonly no-passcode share link; returns hash.
shareAuto :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub Text
shareAuto sessionId confirmLogin mClientPath = do
  clientPath <- case mClientPath of
    Nothing -> throwIO (HTTPError err400 { errBody = "No file specified" })
    Just cp -> pure cp
  result <- tryAny (shareForFile sessionId confirmLogin mClientPath)
  case result of
    Right h -> pure h
    Left _  -> do
      env <- ask
      currentTargetId <- withSession sessionId \s -> readTVar s.currentTargetId
      targets <- atomically (readTVar env.targets)
      case lookup currentTargetId targets of
        Nothing -> throwIO (HTTPError err404 { errBody = "Target not found" })
        Just target -> do
          let root = targetRoot target
          absPath <- fromClientPathChecked root clientPath
          storage <- makeStorageAt target absPath
          file    <- storage.get absPath
          link    <- createSharedLink file currentTargetId Indefinite True Nothing
          addSharedLink link env.sharedLinkPool
          recordHistory sessionId (OpShare currentTargetId clientPath link.hash) True
          let SharedLinkHash h = link.hash
          pure h


-- | List all currently active shared links.
shareLinks :: SessionId -> ConfirmLogin -> Filehub (Html ())
shareLinks _ _ = do
  env <- ask
  activeLinks env.sharedLinkPool


activeLinks :: SharedLinkPool -> Filehub (Html ())
activeLinks pool = do
  now  <- liftIO getCurrentTime
  all_ <- getAllSharedLinks pool
  let links = filter (\l -> not l.revoked && case l.linkType of { Expiry t -> t > now; _ -> True }) all_
  pure (Template.Shared.shareLinksModal links)


-- | Revoke a link; return the updated full modal.
revokeLink :: SessionId -> ConfirmLogin -> Maybe SharedLinkHash -> Filehub (Html ())
revokeLink sessionId _ mHash = do
  env  <- ask
  hash <- case mHash of
    Nothing -> throwIO (HTTPError err400 { errBody = "Missing hash" })
    Just h  -> pure h
  mLink <- getSharedLink hash env.sharedLinkPool
  revokeSharedLink hash env.sharedLinkPool
  case mLink of
    Just link -> recordHistory sessionId (OpRevokeLink link.targetId link.hash) True
    Nothing   -> pure ()
  activeLinks env.sharedLinkPool

-- | Return the edit form as a full modal replacement.
editLinkForm :: SessionId -> ConfirmLogin -> Maybe SharedLinkHash -> Filehub (Html ())
editLinkForm _ _ mHash = do
  env <- ask
  hash <- case mHash of
    Nothing -> throwIO (HTTPError err400 { errBody = "Missing hash" })
    Just h  -> pure h
  mLink <- getSharedLink hash env.sharedLinkPool
  case mLink of
    Nothing   -> throwIO (HTTPError err404 { errBody = "Link not found" })
    Just link -> pure (Template.Shared.linkEditModal link)


-- | Return the normal row for a link (used by shareLinkRow route).
shareLinkRow :: SessionId -> ConfirmLogin -> Maybe SharedLinkHash -> Filehub (Html ())
shareLinkRow _ _ mHash = do
  env <- ask
  hash <- case mHash of
    Nothing -> throwIO (HTTPError err400 { errBody = "Missing hash" })
    Just h  -> pure h
  mLink <- getSharedLink hash env.sharedLinkPool
  case mLink of
    Nothing   -> throwIO (HTTPError err404 { errBody = "Link not found" })
    Just link -> pure (Template.Shared.linkRow link)


-- | Update a link's attributes; return the updated full modal.
updateLink :: SessionId -> ConfirmLogin -> SharedLinkUpdate -> Filehub (Html ())
updateLink _ _ upd = do
  env <- ask
  now <- liftIO getCurrentTime
  newLinkType <- case upd.linkType of
    ShareSingleUse  -> pure SingleUse
    ShareIndefinite -> pure Indefinite
    ShareExpiry     -> do
      let hours = maybe 24 id upd.expiryHours
      pure (Expiry (fromIntegral (hours * 3600) `addUTCTime` now))
  let modifier :: SharedLink -> SharedLink
      modifier link = link
        { linkType = newLinkType
        , passcode = SharedLinkPasscode <$> upd.passcode
        , readonly = upd.readonly
        }
  updateSharedLink upd.hash modifier env.sharedLinkPool
  activeLinks env.sharedLinkPool
