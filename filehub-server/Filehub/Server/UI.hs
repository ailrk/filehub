{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , editorModal
  , selectLayout
  , sortTable
  , toggleTheme
  , changeLocale
  , toggleSidebar
  , selectRows
  , entry
  , entries
  , cancel
  )
  where

import Filehub.Server.UI.Render
import Filehub.Server.UI.Action
