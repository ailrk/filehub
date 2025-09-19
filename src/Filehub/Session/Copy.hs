{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Copy
  ( select
  , copy
  , paste
  , getCopyState
  , setCopyState
  , clearCopyState
  , CopyState
  )
  where

import Control.Monad (forM_)
import Data.ClientPath qualified as ClientPath
import Data.Function (on, fix)
import Data.List (nub)
import Data.File (File(..), FileType(..))
import Data.String.Interpolate (i)
import Effectful (Eff, (:>), Eff, (:>), IOE)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Extended.Cache (Cache)
import Effectful.Extended.LockManager (LockManager)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log, logAttention_)
import Effectful.Reader.Dynamic (Reader)
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Selected qualified as Selected
import Filehub.Types (Env, CopyState(..), SessionId, Selected (..), TargetSessionData (..))
import Lens.Micro hiding (to)
import Lens.Micro.Platform ()
import System.FilePath (takeFileName, (</>))
import Target.Types qualified as Target
import Filehub.Session (TargetView(..))
import Effectful.Temporary (Temporary)


getCopyState :: (Reader Env :> es, IOE :> es, Log :> es, Error FilehubError :> es) => SessionId -> Eff es CopyState
getCopyState sessionId = (^. #copyState) <$> Session.Pool.get sessionId


setCopyState :: (Reader Env :> es, IOE :> es) => SessionId -> CopyState -> Eff es ()
setCopyState sessionId copyState = Session.Pool.update sessionId \s -> s & #copyState .~ copyState


clearCopyState :: (Reader Env :> es, IOE :> es) => SessionId -> Eff es ()
clearCopyState sessionId = setCopyState sessionId NoCopyPaste


-- | Add selected to copy state.
select :: ( Reader Env         :> es
          , IOE                :> es
          , FileSystem         :> es
          , Temporary          :> es
          , Log                :> es
          , Cache              :> es
          , LockManager        :> es
          , Error FilehubError :> es)
       => SessionId -> Eff es ()
select sessionId = do
  allSelecteds <- Selected.allSelecteds sessionId
  forM_ allSelecteds \(target, selected) -> do
    do
      let tid = Target.getTargetId target
      logAttention_ [i|#{tid}, #{selected}|]
    Session.withTarget sessionId (Target.getTargetId target) \_ storage -> do
      case selected of
        NoSelection -> do
          state <- getCopyState sessionId
          case onNoSelection state of
            Right (Just state') -> setCopyState sessionId state'
            Right Nothing       -> pure ()
            Left err -> do
              logAttention_ [i|#{err}|]
              throwError err
        Selected x xs -> do
          root      <- Session.getRoot sessionId
          let paths =  (x:xs) & fmap (ClientPath.fromClientPath root)
          files     <- traverse storage.get paths
          state     <- getCopyState sessionId
          case onSelected (target, files) state of
            Right state' -> setCopyState sessionId state'
            Left err -> do
              logAttention_ [i|#{err}|]
              throwError err
  where
    merge sel [] = [sel]
    merge sel@(target, files) (h@(target', files'):rest)
      | on (==) Target.getTargetId target target' = (target, nub (files <> files')):rest
      | otherwise = h:merge sel rest

    onNoSelection = \case
      NoCopyPaste    -> Right . Just $ CopySelected []
      CopySelected _ -> Right Nothing
      Paste _        -> Left (FilehubError SelectError "Invalid selection")

    onSelected (target, files) = \case
      NoCopyPaste             -> Right (CopySelected [(target, files)])
      CopySelected selections -> Right (CopySelected (merge (target, files) selections))
      Paste _                 -> Left (FilehubError SelectError "Invalid selection")


-- | Confirm selection
copy :: (Reader Env :> es, IOE :> es, Error FilehubError :> es, Log :> es) => SessionId -> Eff es ()
copy sessionId = do
  state <- getCopyState sessionId
  case step state of
    Right state' -> setCopyState sessionId state'
    Left err -> do
      logAttention_ [i|#{err}|]
      throwError err
  where
    step = \case
      CopySelected selections -> Right (Paste selections)
      _                       -> Left (FilehubError SelectError "Not in a copyable state")


-- | Paste files.
paste :: ( Reader Env         :> es
         , IOE                :> es
         , FileSystem         :> es
         , Temporary          :> es
         , Log                :> es
         , Cache              :> es
         , LockManager        :> es
         , Error FilehubError :> es)
      => SessionId -> Eff es ()
paste sessionId = do
  state <- getCopyState sessionId
  case state of
    Paste selections -> do
      TargetView to sessionData _ <- Session.currentTarget sessionId
      forM_ selections \(from, files) -> do
        forM_ files $ flip fix sessionData.currentDir
          \rec currentDir file -> do -- expose the recursion with the fix point
            case file.content of
              Regular -> do
                conduit <- Session.withTarget sessionId (Target.getTargetId from) \_ storage -> do
                  storage.readStream file
                Session.withTarget sessionId (Target.getTargetId to) \_ storage -> do
                  storage.writeStream (currentDir </> takeFileName file.path) conduit file.size
              Dir -> do -- copy directory layer by layer
                dst <- Session.withTarget sessionId (Target.getTargetId to) \_ storage -> do
                  let dst = currentDir </> takeFileName file.path
                  storage.newFolder dst
                  pure dst
                Session.withTarget sessionId (Target.getTargetId from)
                  \(TargetView _ (TargetSessionData { currentDir = savedDir }) _) storage -> do
                    storage.cd file.path
                    do
                      dirFiles <- storage.lsCwd
                      forM_ dirFiles (rec dst)
                    storage.cd savedDir -- go back
      setCopyState sessionId NoCopyPaste
      Selected.clearSelectedAllTargets sessionId
    _ -> do
      logAttention_ [i|Paste error: #{sessionId}, not in pastable state.|]
      throwError (FilehubError SelectError "Not in a pastable state")
