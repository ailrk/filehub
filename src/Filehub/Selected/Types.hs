module Filehub.Selected.Types where

import Filehub.ClientPath (ClientPath)
import Web.FormUrlEncoded (FromForm (..), parseAll)


data Selected
  = Selected ClientPath [ClientPath] -- non empty list
  | NoSelection
  deriving (Show, Eq)


instance FromForm Selected where
  fromForm f = do
    selected <- parseAll "selected" f
    case selected of
      [] -> pure NoSelection
      x:xs->  pure $ Selected x xs



