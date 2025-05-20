{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Server.Desktop where

import Data.String.Interpolate (i)
import Effectful ( Eff, (:>), IOE, withRunInIO )
import Effectful.Error.Dynamic (throwError, Error)
import Effectful.Reader.Dynamic (Reader)
import Filehub.ClientPath qualified as ClientPath
import Filehub.Env (Env (..), TargetView (..))
import Filehub.Env qualified as Env
import Filehub.Target qualified as Target
import Filehub.Error ( withServerError, FilehubError(..), FilehubError(..), withServerError )
import Filehub.Monad ( Filehub )
import Filehub.Selected qualified as Selected
import Filehub.Sort (sortFiles)
import Filehub.Storage qualified as Storage
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Internal qualified as Template
import Filehub.Server.Internal (withQueryParam, runStorage, clear)
import Filehub.Types
    ( SessionId(..),
      SessionId(..),
      Selected (..), ClientPath)
import Filehub.ControlPanel qualified as ControlPanel
import Filehub.Copy qualified as Copy
import Lens.Micro
import Lens.Micro.Platform ()
import Lucid
import Prelude hiding (readFile)
import Prelude hiding (readFile)
import Servant ( ServerError(..), ServerError, err500 )
import System.FilePath (takeFileName)
import Log (logAttention_)
import UnliftIO (catch, SomeException)
import Effectful.Log (Log)
import Effectful.FileSystem (FileSystem)
import Effectful.Concurrent (Concurrent)
import Filehub.Server.Resoluiton (ConfirmDesktopOnly)


index :: SessionId -> Eff  [Reader Env, Log, Error ServerError, FileSystem, Concurrent, IOE] (Html ())
index sessionId = do
  display <- Env.getDisplay sessionId & withServerError
  clear sessionId
  fmap (Template.withDefault display) $ index' sessionId


fileDetailModal :: (Error ServerError :> es, Reader Env :> es, IOE :> es, Log :> es,  FileSystem :> es) => SessionId -> ConfirmDesktopOnly -> Maybe ClientPath -> Eff es (Html ())
fileDetailModal sessionId _ mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  root <- Env.getRoot sessionId
  file <- runStorage sessionId $ Storage.get (ClientPath.fromClientPath root clientPath)
  pure (Template.Desktop.fileDetailModal file)


editorModal :: (Error ServerError :> es, Reader Env :> es, IOE :> es, Log :> es, FileSystem :> es) => SessionId -> ConfirmDesktopOnly -> Maybe ClientPath -> Eff es (Html ())
editorModal sessionId _ mClientPath = withServerError do
    clientPath <- withQueryParam mClientPath
    root <- Env.getRoot sessionId
    let p = ClientPath.fromClientPath root clientPath
    content <- runStorage sessionId do
      f <- Storage.get p
      Storage.read f
    let filename = takeFileName p
    readOnly <- Env.getReadOnly
    pure $ Template.Desktop.editorModal readOnly filename content


selectRows :: (Error ServerError :> es, Reader Env :> es, Log :> es, IOE :> es) => SessionId -> Selected -> Eff es (Html ())
selectRows sessionId selected = do
  case selected of
    NoSelection -> do
      logAttention_ [i|No selection: #{sessionId}|]
      throwError InvalidSelection & withServerError
    _ -> do
      Selected.setSelected sessionId selected
      Template.Desktop.controlPanel
        <$> Env.getReadOnly
        <*> ControlPanel.getControlPanelState sessionId & withServerError


copy :: (Error ServerError :> es, Reader Env :> es, IOE :> es, FileSystem :> es, Log :> es) => SessionId -> p -> Eff es (Html ())
copy sessionId _ = withServerError do
  Copy.select sessionId
  Copy.copy sessionId
  Template.Desktop.controlPanel
    <$> Env.getReadOnly
    <*> ControlPanel.getControlPanelState sessionId


paste :: SessionId -> p -> Eff [Reader Env, Log, Error ServerError, FileSystem, Concurrent, IOE] (Html ())
paste sessionId _ = do
  withRunInIO $ \unlift -> do
    unlift (Copy.paste sessionId & withServerError) `catch` \(_ :: SomeException) -> unlift do
      throwError (err500 { errBody = [i|Paste failed|]})
  index' sessionId


contextMenu :: (Error ServerError :> es, Reader Env :> es, IOE :> es, Log :> es,  FileSystem :> es) => SessionId -> ConfirmDesktopOnly -> Maybe ClientPath -> Eff es (Html ())
contextMenu sessionId _ mClientPath = withServerError do
  clientPath <- withQueryParam mClientPath
  root <- Env.getRoot sessionId
  let filePath = ClientPath.fromClientPath root clientPath
  file <- runStorage sessionId $ Storage.get filePath
  readOnly <- Env.getReadOnly
  pure $ Template.Desktop.contextMenu readOnly root file


index' :: SessionId -> Filehub (Html ())
index' sessionId =
  Template.Desktop.index
  <$> Env.getReadOnly
  <*> sideBar sessionId
  <*> view sessionId
  <*> (ControlPanel.getControlPanelState sessionId & withServerError)


sideBar :: SessionId -> Filehub (Html ())
sideBar sessionId = withServerError $
  Template.Desktop.sideBar
  <$> Env.getTargets
  <*> Target.currentTarget sessionId


view :: SessionId -> Filehub (Html ())
view sessionId = do
  root <- Env.getRoot sessionId & withServerError
  order <- Env.getSortFileBy sessionId & withServerError
  files <- sortFiles order <$> runStorage sessionId Storage.lsCwd & withServerError
  TargetView target _ _ <- Env.currentTarget sessionId & withServerError
  selected <- Selected.getSelected sessionId & withServerError
  let table = Template.Desktop.table target root files selected order
  Template.Desktop.view table <$> pathBreadcrumb sessionId


pathBreadcrumb :: SessionId -> Filehub (Html ())
pathBreadcrumb sessionId =
  Template.Desktop.pathBreadcrumb
  <$> (Env.getCurrentDir sessionId & withServerError)
  <*> (Env.getRoot sessionId & withServerError)
