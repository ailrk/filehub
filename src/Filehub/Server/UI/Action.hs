{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Utilities for Server.
module Filehub.Server.UI.Action
  ( clear
  , selectLayout
  , sortTable
  , toggleTheme
  , changeLocale
  , toggleSidebar
  , selectRows
  , initViewer
  , open
  , cancel
  )
  where

import Control.Monad (join)
import Control.Monad.Reader (MonadReader(ask))
import Data.ClientPath (Root, toClientPath)
import Data.ClientPath qualified as ClientPath
import Data.Coerce (coerce)
import Data.File (FileInfo, File(..))
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text.Encoding qualified as T
import Filehub.Error (FilehubError(..), Error' (..))
import Filehub.Handler (ConfirmLogin)
import Filehub.Locale (Locale)
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Orphan ()
import Filehub.Server.UI.Render (index, UIPartialUpdate (..), runUIPartialUpdate)
import Filehub.Server.Util (withQueryParam)
import Filehub.Session (TargetView (..), getRoot, getTargetViews, getCurrentTarget, makeStorageCurrentTarget)
import Filehub.Session.Copy qualified as Copy
import Filehub.Session.Pool (withSession, withSession_, modifySession)
import Filehub.Session.Selected (AllSelected(..))
import Filehub.Session.Selected qualified as Selected
import Filehub.Session.Types (Selected (..), Layout (..))
import Filehub.Session.Types (Storage(..))
import Filehub.Sort qualified as Sort
import Filehub.Theme qualified as Theme
import Filehub.Types
import Lucid hiding ()
import Network.Mime (MimeType)
import Network.Mime.Extended (isMime)
import Prelude hiding (elem, readFile, init)
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader, NoContent (..))
import System.FilePath (takeDirectory)
import UnliftIO (throwIO, writeTVar, readTVar)


-- | Completely reset all state machines. This should be the only place to reset state.
clear :: SessionId -> Filehub ()
clear sessionId = do
  Selected.clearSelectedAllTargets sessionId
  Copy.clearCopyState sessionId



selectLayout :: SessionId -> ConfirmLogin -> Maybe Layout -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
selectLayout sessionId _ layout = do
  modifySession sessionId \s -> pure do
    s { layout = (fromMaybe ThumbnailLayout layout)
      }
  addHeader LayoutChanged <$> index sessionId


sortTable :: SessionId -> ConfirmLogin -> Maybe SortFileBy -> Filehub (Headers '[ Header "HX-Trigger" FilehubEvent ] (Html ()))
sortTable sessionId _ order = do
  env <- ask
  withSession sessionId \s -> do
    TargetView _ td <- getCurrentTarget env s
    writeTVar td.sortedFileBy (fromMaybe ByNameUp order)
  addHeader TableSorted <$> index sessionId


-- | Toggle the frontend theme by triggering the event handler of `ThemeChanged`
-- in the frontend. A fade-in animation is played when the theme toggled, and it needs
-- to be removed by the frontend.
toggleTheme :: SessionId -> ConfirmLogin -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
toggleTheme sessionId _ = do
  withSession_ sessionId \s -> pure
    case s.theme of
      Theme.Light -> ( s { theme = Dark }, ())
      Theme.Dark  -> ( s { theme = Light }, ())
  html <- index sessionId
  pure $ addHeader ThemeChanged (html `with` [ class_ "fade-in " ])


changeLocale :: SessionId -> Maybe Locale -> Filehub (Headers '[ Header "HX-Trigger-After-Settle" FilehubEvent ] (Html ()))
changeLocale _ Nothing = throwIO (FilehubError LocaleError "Invalid locale")
changeLocale sessionId (Just locale) = do
  withSession_ sessionId \s -> pure (s { locale = locale } , ())
  addHeader LocaleChanged <$> index sessionId


toggleSidebar :: SessionId -> ConfirmLogin -> Filehub (Html ())
toggleSidebar sessionId _ = do
  withSession_ sessionId \s -> pure
    ( s { sidebarCollapsed = not s.sidebarCollapsed
        }
    , ()
    )
  index sessionId


selectRows :: SessionId -> ConfirmLogin -> Selected -> Filehub (Headers '[ Header "X-Filehub-Selected-Count" Int ] (Html ()))
selectRows sessionId _ selected = do
  env <- ask
  case selected of
    NoSelection -> do
      withSession sessionId \s -> do
        TargetView _ td <- getCurrentTarget env s
        writeTVar td.selected NoSelection

    _ -> do
      withSession sessionId \s -> do
        TargetView _ td <- getCurrentTarget env s
        writeTVar td.selected selected

  AllSelected { count } <- withSession sessionId \s -> do
    Selected.getAllSelected =<< getTargetViews env s

  addHeader count <$> runUIPartialUpdate sessionId
    [ UpdateSideBar
    , UpdateControlPanel
    ]


initViewer :: SessionId -> ConfirmLogin -> Maybe ClientPath
           -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
initViewer sessionId _ mClientPath = do
  env <- ask
  storage <- makeStorageCurrentTarget sessionId
  (root, order) <- withSession sessionId \s -> do
    TargetView _ td <- getCurrentTarget env s
    root            <- getRoot env s
    order           <- readTVar td.sortedFileBy
    pure (root, order)

  clientPath <- withQueryParam mClientPath
  payload <- do
    let filePath  =  ClientPath.fromClientPath root clientPath
    let dir       =  coerce takeDirectory filePath
    files         <- takeResourceFiles . Sort.sortFiles order <$> (storage.ls dir)
    let idx       =  fromMaybe 0 $ L.elemIndex filePath (fmap (.path) files)
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
                                                  , mimetype = T.decodeUtf8 f.mimetype
        }


open :: SessionId -> ConfirmLogin -> Maybe OpenTarget -> Maybe ClientPath
     -> Filehub (Headers '[Header "HX-Trigger" FilehubEvent] NoContent)
open _ _ mTarget mClientPath = do
  clientPath <- withQueryParam mClientPath
  target     <- withQueryParam mTarget
  pure $ addHeader (Opened target clientPath) NoContent


cancel :: SessionId -> ConfirmLogin -> Filehub (Headers '[Header "X-Filehub-Selected-Count" Int] (Html ()))
cancel sessionId _ = do
  env     <- ask
  storage <- makeStorageCurrentTarget sessionId

  join $ withSession sessionId \s -> do
    AllSelected
      { count
      , allSelected
      }                   <- Selected.getAllSelected =<< getTargetViews env s
    TargetView { target } <- getCurrentTarget env s
    root                  <- getRoot env s

    pure do
      let
          selected = S.fromList case lookup target allSelected of
                                   Just sel  -> Selected.toList sel
                                   Nothing -> []

      selectedFiles <- do
        files <- storage.lsCwd
        let predicate f = (toClientPath root f.path) `S.member` selected
        pure $ filter predicate files


      clear sessionId
      addHeader count <$> runUIPartialUpdate sessionId
        [ UpdateControlPanel
        , UpdateSideBar
        , UpdateEntries selectedFiles
        ]
