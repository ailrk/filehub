module Filehub.Session.Target where

import Data.Map.Strict qualified as Map
import Filehub.Monad (Filehub)
import Filehub.Session.Internal (targetToSessionData)
import Filehub.Session.Pool qualified as Session.Pool
import Filehub.Session.Types (Session(..), TargetView(..))
import Filehub.Session.Types (SessionGet(..), SessionSet(..))
import Filehub.Session.Types.SessionId (SessionId(..))
import Target.Types (AnyTarget (..), HasTargetId (..))
import UnliftIO (finally)
import {-# SOURCE #-} Filehub.Session.Handle (newSessionSet, get)


attachTarget :: SessionId -> AnyTarget -> Filehub ()
attachTarget sessionId target = do
  TargetView current _ <- get sessionId (.currentTarget)
  if current == target
     then pure ()
     else do
       Session.Pool.update sessionId \session -> do
         session { targets = Map.insert (getTargetId target) (targetToSessionData target) session.targets
                 }


detachTarget :: HasTargetId t => SessionId -> t -> Filehub ()
detachTarget sessionId target = do
  let tid = getTargetId target
  Session.Pool.update sessionId \session -> do
    session { targets = Map.delete tid session.targets
            }


withTarget :: HasTargetId t => SessionId -> t -> Filehub a -> Filehub a
withTarget sid t action = do
  oldS <- Session.Pool.get sid
  let oldTid = oldS.currentTargetId
  (newSessionSet sid).currentTarget (getTargetId t)
  action `finally` (newSessionSet sid).currentTarget oldTid
