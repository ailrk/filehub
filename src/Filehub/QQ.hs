{-# LANGUAGE TemplateHaskell #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- Some convenient templates

module Filehub.QQ
  (getGitRev)
  where

import Language.Haskell.TH
import System.Environment (lookupEnv)


getGitRev :: Q Exp
getGitRev = runIO do
  m <- lookupEnv "GIT_REV"
  case m of
    Just rev -> pure (LitE (StringL rev))
    Nothing -> pure (LitE (StringL "dirty"))
