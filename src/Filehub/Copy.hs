module Filehub.Copy
  ( copy
  , select
  , getCopyState
  , setCopyState
  , CopyState
  )
  where

import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log)
import Filehub.Types (Env, CopyState(..), SessionId, Session(..), File(..))
import Filehub.Error (FilehubError)
import Filehub.Env (TargetView(..))
import Filehub.Env qualified as Env
import Filehub.Storage (runStorage)
import Filehub.Storage qualified as Storage
import Filehub.Target qualified as Target
import Control.Monad (forM_)
import System.FilePath (takeFileName, (</>))


getCopyState :: (Reader Env :> es, IOE :> es, Error FilehubError :> es) => SessionId -> Eff es CopyState
getCopyState sessionId = (^. #sessionData . #copyState) <$> Target.currentTarget sessionId


setCopyState :: (Reader Env :> es, IOE :> es) => SessionId -> CopyState -> Eff es ()
setCopyState sessionId copyState = Env.updateSession sessionId $ \s -> s & #targets . ix s.index . #copyState .~ copyState


select :: (Reader Env :> es, Error FilehubError :> es, IOE :> es,  FileSystem :> es, Log :> es)
       => SessionId -> CopyState -> [File] -> Eff es (Maybe CopyState)
select sessionId NoCopyPaste files = Just <$> (copy sessionId $ CopySelect [] files)
select sessionId (CopySelected selections) files = Just <$> (copy sessionId $ CopySelect selections files)
select _ _ _ = pure Nothing


copy :: (Reader Env :> es, IOE :> es, FileSystem :> es, Log :> es, Error FilehubError :> es)
     => SessionId -> CopyState -> Eff es CopyState
copy sessionId state =
  case state of
    CopySelect selections files -> do
      TargetView currentTarget _ _ <- Env.currentTarget sessionId
      let selections' =
            if currentTarget `elem` fmap fst selections
               then (currentTarget, files) : filter ((/= currentTarget) . fst) selections
               else selections
      pure $ CopySelected selections'
    CopySelected selections -> do
      TargetView currentTarget _ _ <- Env.currentTarget sessionId
      pure (Paste selections currentTarget "/")
    Paste selections to dirPath -> do
      TargetView saved _ _ <- Env.currentTarget sessionId
      forM_ selections $ \(from, files)-> do
        forM_ files $ \file -> do
          Env.changeCurrentTarget sessionId (Target.getTargetId from)
          bytes <- runStorage sessionId $ Storage.readFileContent file
          Env.changeCurrentTarget sessionId (Target.getTargetId to)
          let destination = dirPath </> takeFileName file.path
          runStorage sessionId $ Storage.writeFile destination bytes
      Env.changeCurrentTarget sessionId (Target.getTargetId saved)
      pure CopyFinished
    CopyFinished -> pure NoCopyPaste
    NoCopyPaste -> pure NoCopyPaste
