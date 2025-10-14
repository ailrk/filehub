{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Session.Copy
  ( select
  , copy
  , getCopyState
  , setCopyState
  , clearCopyState
  , CopyState
  )
  where

import Control.Monad (forM_)
import Data.ClientPath qualified as ClientPath
import Data.Function (on)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.String.Interpolate (i)
import Effectful.Error.Dynamic (throwError)
import Effectful.Log (logAttention_)
import Filehub.Error (FilehubError (..), Error' (..))
import Filehub.Session qualified as Session
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Selected qualified as Selected
import Filehub.Types (CopyState(..), SessionId, Selected (..))
import Lens.Micro hiding (to)
import Target.Types qualified as Target
import Filehub.Monad (Filehub)


getCopyState :: SessionId -> Filehub CopyState
getCopyState sessionId = (^. #copyState) <$> Session.Pool.get sessionId


setCopyState :: SessionId -> CopyState -> Filehub ()
setCopyState sessionId copyState = Session.Pool.update sessionId \s -> s & #copyState .~ copyState


clearCopyState :: SessionId -> Filehub ()
clearCopyState sessionId = setCopyState sessionId NoCopyPaste


-- | Add selected to copy state.
select :: SessionId -> Filehub ()
select sessionId = do
  allSelecteds <- Selected.allSelecteds sessionId
  forM_ allSelecteds \(target, selected) -> do
    Session.withTarget sessionId (Target.getTargetId target) \_ storage -> do
      case selected of
        NoSelection -> do
          state <- getCopyState sessionId
          case onNoSelection state of
            Right (Just state') -> setCopyState sessionId state'
            Right Nothing       -> pure ()
            Left err -> do
              logAttention_ [i|[asckkk] #{err}|]
              throwError err
        Selected x xs -> do
          root <- Session.getRoot sessionId
          let paths = (x:xs) & fmap (ClientPath.fromClientPath root)
          files <- traverse storage.get paths <&> catMaybes
          state <- getCopyState sessionId
          case onSelected (target, files) state of
            Right state' -> setCopyState sessionId state'
            Left err -> do
              logAttention_ [i|[ascks1] #{err}|]
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
copy :: SessionId -> Filehub ()
copy sessionId = do
  state <- getCopyState sessionId
  case step state of
    Right state' -> do
      setCopyState sessionId state'
    Left err -> do
      logAttention_ [i|[tyy33d] #{err}|]
      throwError err
  where
    step = \case
      CopySelected selections -> Right (Paste selections)
      _                       -> Left (FilehubError SelectError "Not in a copyable state")
