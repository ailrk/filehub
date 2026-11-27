{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Filehub.Server.Settings
  ( settingsPanel
  , settingsExport
  , settingsGetNewPlugin
  , settingsCancelNewPlugin
  , settingsAddPlugin
  , settingsUpdatePlugin
  , settingsDeletePlugin
  , settingsAddTarget
  , settingsDeleteTarget
  , settingsUpdateAppearance
  ) where

import Filehub.Prelude
import Control.Monad.Reader (ask)
import Data.ClientPath (AbsPath(..), Root(..))
import Filehub.Error (FilehubError(..))
import Filehub.Handler (ConfirmLogin)
import Filehub.Locale (Locale(..))
import Filehub.Monad (Filehub)
import Filehub.Plugin.Toml qualified as Plugin.Toml
import Filehub.Plugin.Types (StaticPlugin(..))
import Filehub.Server.UI.Render (runUIPartialUpdate, UIPartialUpdate(..))
import Filehub.Session (withSession_, Pool(..), Selected(..))
import Filehub.Types
import Lucid (Html)
import Servant (Headers, Header, addHeader, err400, errBody)
import System.Directory (doesFileExist)
import Target.File (FileSys, Target(..))
import Target.File qualified as FS
import Target.S3 qualified as S3
import Target.Types (AnyTarget(..), TargetId, getTargetId, handleTarget, targetHandler)
import Filehub.Template.Settings qualified as Template


------------------------------------
-- Panel
------------------------------------


settingsPanel :: SessionId -> ConfirmLogin -> Filehub (Html ())
settingsPanel _ _ = do
  env        <- ask
  allPlugins <- readTVarIO env.plugins
  dynPlugins <- readTVarIO env.dynamicPlugins
  allTargets <- readTVarIO env.targets
  themeVal   <- readTVarIO env.theme
  localeVal  <- readTVarIO env.locale
  pure $ Template.settingsModal
    allPlugins (dynNames dynPlugins) allTargets env.staticTargetIds themeVal localeVal


------------------------------------
-- Export
------------------------------------


settingsExport :: SessionId -> ConfirmLogin -> Filehub Text
settingsExport _ _ = do
  env <- ask
  -- Flush current state to file first so the exported content is fresh
  liftIO env.persistDynConfig
  case env.dynamicConfigPath of
    Nothing   -> pure "# No --dynamic-config flag supplied; nothing persisted.\n"
    Just path -> do
      exists <- liftIO (doesFileExist path)
      if exists
        then liftIO (txt_readFile path)
        else pure "# Dynamic config file not yet created.\n"


------------------------------------
-- Plugin editor
------------------------------------


settingsGetNewPlugin :: SessionId -> ConfirmLogin -> Filehub (Html ())
settingsGetNewPlugin _ _ = pure $ Template.newPluginForm Nothing Plugin.Toml.pluginHint


settingsCancelNewPlugin :: SessionId -> ConfirmLogin -> Filehub (Html ())
settingsCancelNewPlugin _ _ = do
  env <- ask
  allPlugins <- readTVarIO env.plugins
  dynPlugins <- readTVarIO env.dynamicPlugins
  pure $ Template.pluginsPanel allPlugins (dynNames dynPlugins)


settingsAddPlugin :: SessionId -> ConfirmLogin -> PluginsTextForm -> Filehub (Html ())
settingsAddPlugin sessionId _ form = do
  env        <- ask
  allPlugins <- readTVarIO env.plugins
  dynPlugins <- readTVarIO env.dynamicPlugins
  case parseExactlyOne form.pluginsToml of
    Left err -> pure $ Template.pluginsPanelWithNewForm allPlugins (dynNames dynPlugins) (Just err) form.pluginsToml
    Right p  -> do
      let StaticPlugin { name = pName } = p
      if any (\q -> let StaticPlugin { name = n } = q in n == pName) allPlugins
        then pure $ Template.pluginsPanelWithNewForm allPlugins (dynNames dynPlugins) (Just [i|Plugin "#{pName}" already exists.|]) form.pluginsToml
        else do
          atomically do
            modifyTVar env.dynamicPlugins (++ [p])
            modifyTVar env.plugins        (++ [p])
          liftIO env.persistDynConfig
          allPlugins' <- readTVarIO env.plugins
          dynPlugins' <- readTVarIO env.dynamicPlugins
          oob <- runUIPartialUpdate sessionId [UpdateView, UpdatePlugins]
          pure $ Template.pluginsPanel allPlugins' (dynNames dynPlugins') <> oob


settingsUpdatePlugin :: Text -> SessionId -> ConfirmLogin -> PluginsTextForm -> Filehub (Html ())
settingsUpdatePlugin oldName sessionId _ form = do
  env <- ask
  case parseExactlyOne form.pluginsToml of
    Left err -> pure $ Template.pluginItemError oldName form.pluginsToml err
    Right p  -> do
      let StaticPlugin { name = newName } = p
      allPlugins <- readTVarIO env.plugins
      let nameConflict = newName /= oldName &&
            any (\q -> let StaticPlugin { name = n } = q in n == newName) allPlugins
      if nameConflict
        then pure $ Template.pluginItemError oldName form.pluginsToml [i|Plugin "#{newName}" already exists.|]
        else do
          atomically do
            modifyTVar env.dynamicPlugins (pluginReplace oldName p)
            modifyTVar env.plugins        (pluginReplace oldName p)
          liftIO env.persistDynConfig
          oob <- runUIPartialUpdate sessionId [UpdateView, UpdatePlugins]
          pure $ Template.pluginItem p <> oob


settingsDeletePlugin :: Text -> SessionId -> ConfirmLogin -> Filehub (Html ())
settingsDeletePlugin name sessionId _ = do
  env <- ask
  atomically do
    modifyTVar env.dynamicPlugins (pluginRemove name)
    modifyTVar env.plugins        (pluginRemove name)
  liftIO env.persistDynConfig
  allPlugins <- readTVarIO env.plugins
  dynPlugins <- readTVarIO env.dynamicPlugins
  oob <- runUIPartialUpdate sessionId [UpdateView, UpdatePlugins]
  pure $ Template.pluginsPanel allPlugins (dynNames dynPlugins) <> oob


parseExactlyOne :: Text -> Either Text StaticPlugin
parseExactlyOne tomlText =
  case Plugin.Toml.parsePlugins tomlText of
    Left err -> Left err
    Right []  -> Left "Expected exactly one [[plugin]] block."
    Right [p] -> Right p
    Right _   -> Left "Expected exactly one [[plugin]] block; got multiple."


dynNames :: [StaticPlugin] -> Set Text
dynNames ps = set_fromList (map (\(StaticPlugin { name }) -> name) ps)


pluginRemove :: Text -> [StaticPlugin] -> [StaticPlugin]
pluginRemove n = filter \p ->
  let StaticPlugin { name } = p in name /= n


pluginReplace :: Text -> StaticPlugin -> [StaticPlugin] -> [StaticPlugin]
pluginReplace n newP = map \p ->
  let StaticPlugin { name } = p in if name == n then newP else p


------------------------------------
-- Target CRUD
------------------------------------


settingsAddTarget :: SessionId -> ConfirmLogin -> AddTargetForm -> Filehub (Html ())
settingsAddTarget sessionId _ form = do
  env <- ask
  tgt <- case form.targetType of
    "fs" -> do
      t <- FS.initialize FS.Config { root = txt_unpack form.targetPath }
      pure (AnyTarget t)
    "s3" -> do
      t <- S3.initialize S3.Config { bucket = txt_unpack form.targetPath }
      pure (AnyTarget t)
    other ->
      throwIO (HTTPError (err400 { errBody = [i|Unknown target type: #{other}|] }))

  let tid = getTargetId tgt
  atomically do
    modifyTVar env.targets        ((tid, tgt) :)
    modifyTVar env.dynamicTargets (tgt :)

  addTargetToAllSessions env tid tgt
  liftIO env.persistDynConfig

  allTargets <- readTVarIO env.targets
  sideBarOOB <- runUIPartialUpdate sessionId [UpdateSideBar]
  pure (Template.settingsTargetsSection allTargets env.staticTargetIds <> sideBarOOB)


settingsDeleteTarget :: TargetId -> SessionId -> ConfirmLogin -> Filehub (Html ())
settingsDeleteTarget tid sessionId _ = do
  env <- ask
  atomically do
    modifyTVar env.targets        (filter ((/= tid) . fst))
    modifyTVar env.dynamicTargets (filter ((/= tid) . getTargetId))

  removeTargetFromAllSessions env tid
  liftIO env.persistDynConfig

  allTargets <- readTVarIO env.targets
  sideBarOOB <- runUIPartialUpdate sessionId [UpdateSideBar]
  pure (Template.settingsTargetsSection allTargets env.staticTargetIds <> sideBarOOB)


------------------------------------
-- Appearance
------------------------------------


type Refresh = Headers '[Header "HX-Refresh" Text] (Html ())

settingsUpdateAppearance
  :: SessionId -> ConfirmLogin -> AppearanceForm -> Filehub Refresh
settingsUpdateAppearance sessionId _ form = do
  env <- ask

  newTheme <- case form.appTheme of
    Just "dark"  -> pure Dark
    Just "light" -> pure Light
    Just other   ->
      throwIO (HTTPError (err400 { errBody = [i|Unknown theme: #{other}|] }))
    Nothing      -> readTVarIO env.theme

  newLocale <- case form.appLocale >>= parseLocale of
    Just l  -> pure l
    Nothing ->
      case form.appLocale of
        Nothing -> readTVarIO env.locale
        Just t  -> throwIO (HTTPError (err400 { errBody = [i|Unknown locale: #{t}|] }))

  atomically do
    writeTVar env.theme  newTheme
    writeTVar env.locale newLocale

  -- Also update current session so the user sees the change immediately
  withSession_ sessionId \s ->
    pure (s { theme = newTheme, locale = newLocale }, ())

  liftIO env.persistDynConfig
  pure $ addHeader "true" (Template.settingsAppearanceSection newTheme newLocale)


------------------------------------
-- Target session helpers
------------------------------------


mkTargetSessionData :: AnyTarget -> IO TargetSessionData
mkTargetSessionData tgt = do
  let initDir = case handleTarget tgt [ targetHandler @FileSys \(FileBackend { root = Root (AbsPath p) }) -> AbsPath p ] of
                  Just d  -> d
                  Nothing -> AbsPath ""
  currentDir   <- newTVarIO initDir
  sortedFileBy <- newTVarIO ByModifiedDown
  selected     <- newTVarIO NoSelection
  pure TargetSessionData { currentDir = currentDir, sortedFileBy = sortedFileBy, selected = selected }


addTargetToAllSessions :: Env -> TargetId -> AnyTarget -> Filehub ()
addTargetToAllSessions env tid tgt = do
  let Pool { pool } = env.sessionPool
  sessions <- readTVarIO pool
  newTsds  <- liftIO $ traverse (\_ -> mkTargetSessionData tgt) sessions
  atomically $ modifyTVar pool \m ->
    map_intersectionWith (insertSessionTarget tid) m newTsds


removeTargetFromAllSessions :: Env -> TargetId -> Filehub ()
removeTargetFromAllSessions env tid = do
  let Pool { pool } = env.sessionPool
  atomically $ modifyTVar pool $ map_map (deleteSessionTarget tid)


insertSessionTarget :: TargetId -> Session -> TargetSessionData -> Session
insertSessionTarget tid s tsd = s { targets = map_insert tid tsd s.targets }

deleteSessionTarget :: TargetId -> Session -> Session
deleteSessionTarget tid s = s { targets = map_delete tid s.targets }


------------------------------------
-- Helpers
------------------------------------



parseLocale :: Text -> Maybe Locale
parseLocale = \case
  "en"    -> Just EN;   "zh_cn" -> Just ZH_CN; "zh_tw" -> Just ZH_TW
  "zh_hk" -> Just ZH_HK; "ja"  -> Just JA;    "es"    -> Just ES
  "fr"    -> Just FR;   "de"    -> Just DE;    "ko"    -> Just KO
  "ru"    -> Just RU;   "pt"    -> Just PT;    "it"    -> Just IT
  _       -> Nothing
