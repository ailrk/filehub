{-# LANGUAGE PartialTypeSignatures #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Utilities for Server.
module Filehub.Server.UI
  ( clear
  , index
  , view
  , sideBar
  , toolBar
  , controlPanel
  , newFileModal
  , newFolderModal
  , fileDetailModal
  , editorModal
  , selectLayout
  , sortTable
  , toggleTheme
  , changeLocale
  , toggleSidebar
  , renameModal
  , selectRows
  , contextMenu
  , entry
  , initViewer
  , open
  )
  where

import Data.ClientPath qualified as ClientPath
import Data.Maybe (fromMaybe)
import Filehub.Handler (ConfirmLogin, ConfirmReadOnly, ConfirmDesktopOnly)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Orphan ()
import Filehub.Server.UI.Desktop qualified as Server.Desktop
import Filehub.Server.UI.Mobile qualified as Server.Mobile
import Filehub.Server.Util (withQueryParam)
import Filehub.Session (SessionGet(..))
import Filehub.Session qualified as Session
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Selected qualified as Selected
import Filehub.Template (runTemplate, makeTemplateContext)
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Template.Shared qualified as Template
import Filehub.Types
import Lens.Micro.Platform ()
import Lucid hiding ()
import Prelude hiding (elem, readFile, init)
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader, NoContent (..)         )
import Filehub.Theme qualified as Theme
import Filehub.Locale (Locale)
import Filehub.Error (FilehubError(..), Error' (..))
import UnliftIO (throwIO)
import Network.Mime (MimeType)
import Data.File (FileInfo, File(..))
import Data.ClientPath (Root)
import Data.String.Interpolate (i)
import Network.Mime.Extended (isMime)
import Data.Coerce (coerce)
import System.FilePath (takeDirectory)
import Filehub.Sort qualified as Sort
import Data.List qualified as List
import Data.Text.Encoding qualified as Text


-- | Completely reset all state machines. This should be the only place to reset state.
clear :: SessionId -> Filehub ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId


entry :: SessionId -> FileInfo -> Filehub (Html ())
entry sessionId file = do
  display <- Session.get sessionId (.display)
  layout  <- Session.get sessionId (.layout)
  ctx     <- makeTemplateContext sessionId

  pure $ runTemplate ctx
    case display of
      NoDisplay -> Template.Mobile.entry file
      Mobile    -> Template.Mobile.entry file
      Desktop   -> case layout of
                     ListLayout      -> Template.Desktop.entry file
                     ThumbnailLayout -> Template.Desktop.thumbnail file


index :: SessionId -> Filehub (Html ())
index sessionId = do
  display <- Session.get sessionId (.display)
  case display of
    NoDisplay -> pure Template.bootstrap
    Desktop   -> Server.Desktop.index sessionId
    Mobile    -> Server.Mobile.index sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  display <- Session.get sessionId (.display)
  case display of
    Desktop   -> Server.Desktop.view sessionId
    Mobile    -> Server.Mobile.view sessionId
    NoDisplay -> Server.Mobile.view sessionId


controlPanel :: SessionId -> Filehub (Html ())
controlPanel sessionId = do
  display <- Session.get sessionId (.display)
  ctx <- makeTemplateContext sessionId
  pure $
    case display of
      Desktop -> runTemplate ctx Template.Desktop.controlPanel
      Mobile  -> runTemplate ctx Template.Mobile.controlPanel
      _       -> runTemplate ctx Template.Mobile.controlPanel


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = do
  display <- Session.get sessionId (.display)
  case display of
    Desktop -> Server.Desktop.sideBar sessionId
    _       -> Server.Mobile.sideBar sessionId


toolBar :: SessionId -> Filehub (Html ())
toolBar sessionId = do
  display <- Session.get sessionId (.display)
  case display of
    Desktop -> Server.Desktop.toolBar sessionId
    _       -> Server.Mobile.toolBar sessionId


renameModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Maybe ClientPath -> Filehub (Html ())
renameModal sessionId _ _ _ mClientPath = do
  root       <- Session.get sessionId (.root)
  clientPath <- withQueryParam mClientPath
  ctx        <- makeTemplateContext sessionId
  pure $ runTemplate ctx (Template.Desktop.renameModal (ClientPath.fromClientPath root clientPath))


newFileModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Filehub (Html ())
newFileModal sessionId _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFileModal


newFolderModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> ConfirmReadOnly -> Filehub (Html ())
newFolderModal sessionId  _ _ _ = do
  ctx <- makeTemplateContext sessionId
  pure $ runTemplate ctx Template.Desktop.newFolderModal


fileDetailModal :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> Maybe ClientPath -> Filehub (Html ())
fileDetailModal sessionId _ _ mPath = do
  Server.Desktop.fileDetailModal sessionId mPath


editorModal :: SessionId -> ConfirmLogin -> Maybe ClientPath -> Filehub (Html ())
editorModal sessionId _ mClientPath = do
  display <- Session.get sessionId (.display)
  case display of
    Mobile    -> Server.Mobile.editorModal sessionId mClientPath
    Desktop   -> Server.Desktop.editorModal sessionId mClientPath
    NoDisplay -> error "impossible"


selectLayout :: SessionId -> ConfirmLogin -> Maybe Layout -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
selectLayout sessionId _ layout = do
  Session.set sessionId (.layout) (fromMaybe ThumbnailLayout layout)
  addHeader LayoutChanged <$> index sessionId


sortTable :: SessionId -> ConfirmLogin -> Maybe SortFileBy -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
sortTable sessionId _ order = do
  display <- Session.get sessionId (.display)
  Session.set sessionId (.sortedFileBy) (fromMaybe ByNameUp order)
  html <- do
    view' <- view sessionId
    case display of
      Mobile -> do
        toolBar' <- Server.Mobile.toolBar sessionId
        pure do
          toolBar' `with` [ term "hx-swap-oob" "true" ]
          view'
      Desktop   -> pure view'
      NoDisplay -> pure view'
  pure $ addHeader TableSorted html



-- | Toggle the frontend theme by triggering the event handler of `ThemeChanged`
-- in the frontend. A fade-in animation is played when the theme toggled, and it needs
-- to be removed by the frontend.
toggleTheme :: SessionId -> ConfirmLogin -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
toggleTheme sessionId _ = do
  theme <- Session.get sessionId (.theme)
  case theme of
    Theme.Light -> Session.set sessionId (.theme) Dark
    Theme.Dark  -> Session.set sessionId (.theme) Light
  html <- index sessionId
  pure $ addHeader ThemeChanged (html `with` [ class_ "fade-in " ])


changeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
changeLocale _ Nothing = throwIO (FilehubError LocaleError "Invalid locale")
changeLocale sessionId (Just locale) = do
  Session.set sessionId (.locale) locale
  addHeader LocaleChanged <$> index sessionId


toggleSidebar :: SessionId -> ConfirmLogin -> Filehub (Html ())
toggleSidebar sessionId _ = do
  b <- Session.get sessionId (.sidebarCollapsed)
  Session.set sessionId (.sidebarCollapsed) (not b)
  index sessionId


selectRows :: SessionId -> ConfirmLogin -> Selected -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
selectRows sessionId _ selected = do
  case selected of
    NoSelection -> do
      Selected.setSelected sessionId NoSelection
      sideBar'      <- sideBar sessionId
      controlPanel' <- controlPanel sessionId
      pure $ addHeader 0 do
        sideBar' `with` [ term "hx-swap-oob" "true" ]
        controlPanel'
    _ -> do
      Selected.setSelected sessionId selected
      count         <- length <$> Selected.allSelecteds sessionId
      sideBar'      <- sideBar sessionId
      controlPanel' <- controlPanel sessionId
      pure $ addHeader count do
        sideBar' `with` [ term "hx-swap-oob" "true" ]
        controlPanel'


contextMenu :: SessionId -> ConfirmLogin -> ConfirmDesktopOnly -> [ClientPath] -> Filehub (Html ())
contextMenu sessionId _ _ paths = Server.Desktop.contextMenu sessionId paths


initViewer :: SessionId -> ConfirmLogin -> Maybe ClientPath
           -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
initViewer sessionId _ mClientPath = do
  root       <- Session.get sessionId (.root)
  order      <- Session.get sessionId (.sortedFileBy)
  storage    <- Session.get sessionId (.storage)
  clientPath <- withQueryParam mClientPath
  payload <- do
    let filePath  =  ClientPath.fromClientPath root clientPath
    let dir       =  coerce takeDirectory filePath
    files         <- takeResourceFiles . Sort.sortFiles order <$> (storage.ls dir)
    let idx       =  fromMaybe 0 $ List.elemIndex filePath (fmap (.path) files)
    let resources =  fmap (toResource root) files
    pure $ ViewerInited resources idx
  pure $ addHeader payload NoContent
  where
    isResource :: MimeType -> Bool
    isResource s = any (s `isMime`)  ["image", "video", "audio"]

    takeResourceFiles :: [FileInfo] -> [FileInfo]
    takeResourceFiles = filter (isResource . (.mimetype))

    toResource :: Root -> FileInfo -> Resource
    toResource root f =
      Resource
        { url = let ClientPath path = ClientPath.toClientPath root f.path -- encode path url
                 in ClientPath.RawClientPath [i|/serve?file=#{path}|]
                                                  , mimetype = Text.decodeUtf8 f.mimetype
        }


open :: SessionId -> ConfirmLogin -> Maybe OpenTarget -> Maybe ClientPath
     -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
open _ _ mTarget mClientPath = do
  clientPath <- withQueryParam mClientPath
  target     <- withQueryParam mTarget
  pure $ addHeader (Opened target clientPath) NoContent
