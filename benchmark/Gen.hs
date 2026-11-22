{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Synthetic data generators and NFData orphans for benchmarks.
module Gen
  ( mkFileInfo
  , mkFiles
  , mkDirInfo
  , mkMixedFiles
  , benchCtx
  , benchCtxThumbnail
  ) where

import Control.DeepSeq (NFData(..), deepseq)
import Data.ClientPath (AbsPath(..), ClientPath(..), Root(..))
import Data.File (File(..), FileType(..), IsLink(..))
import Filehub.SharedLink (SharedLink(..), SharedLinkHash(..))
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Filehub.Locale (Locale(..))
import Filehub.Plugin.Types (StaticPlugin)
import Filehub.Session.Types (TargetSessionData(..), TargetView(..), Selected(..), Layout(..))
import Filehub.Sort (SortFileBy(..))
import Filehub.Template (TemplateContext(..))
import Filehub.Session.Types (ControlPanelState(..))
import Filehub.Theme (Theme(..))
import Filehub.Types (Display(..))
import Target.Dummy (newDummyTarget)
import Target.Types (AnyTarget(..))
import UnliftIO.STM (newTVarIO)
import System.IO.Unsafe (unsafePerformIO)


epoch :: UTCTime
epoch = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)


mkFileInfo :: Int -> File FileType
mkFileInfo i = File
  { path     = AbsPath ("/bench/file_" <> show i <> ".txt")
  , atime    = Just epoch
  , mtime    = Just epoch
  , size     = Just (fromIntegral (i * 1024))
  , mimetype = "text/plain"
  , isLink   = NotLink
  , content  = Regular
  }


mkDirInfo :: Int -> File FileType
mkDirInfo i = File
  { path     = AbsPath ("/bench/dir_" <> show i)
  , atime    = Just epoch
  , mtime    = Just epoch
  , size     = Just 0
  , mimetype = "inode/directory"
  , isLink   = NotLink
  , content  = Dir
  }


mkFiles :: Int -> [File FileType]
mkFiles n = map mkFileInfo [1..n]


-- | Half dirs, half files — exercises the dir-first sort comparator.
mkMixedFiles :: Int -> [File FileType]
mkMixedFiles n =
  map mkDirInfo  [1 .. n `div` 2] <>
  map mkFileInfo [1 .. n - n `div` 2]


-- | Minimal TemplateContext for list-layout table benchmarks.
-- Uses lazy (~) fields so unneeded fields remain as error thunks.
benchCtx :: TemplateContext
benchCtx = unsafePerformIO mkBenchCtx
{-# NOINLINE benchCtx #-}


benchCtxThumbnail :: TemplateContext
benchCtxThumbnail = unsafePerformIO (mkBenchCtxWith ThumbnailLayout)
{-# NOINLINE benchCtxThumbnail #-}


mkBenchCtx :: IO TemplateContext
mkBenchCtx = mkBenchCtxWith ListLayout


-- ─── Orphan NFData instances ─────────────────────────────────────────────────

instance NFData AbsPath where
  rnf (AbsPath s) = s `deepseq` ()

instance NFData ClientPath where
  rnf (ClientPath s) = s `deepseq` ()

instance NFData FileType where
  rnf Regular = ()
  rnf Dir     = ()

instance NFData IsLink where
  rnf Link       = ()
  rnf BrokenLink = ()
  rnf NotLink    = ()

instance NFData a => NFData (File a) where
  rnf f = rnf f.path   `seq`
          rnf f.atime  `seq`
          rnf f.mtime  `seq`
          rnf f.size   `seq`
          rnf f.isLink `seq`
          rnf f.content

instance NFData SharedLinkHash where
  rnf (SharedLinkHash t) = t `deepseq` ()

instance NFData SharedLink where
  rnf l = rnf l.hash `seq` rnf l.accessCount `seq` ()


-- ─────────────────────────────────────────────────────────────────────────────

mkBenchCtxWith :: Layout -> IO TemplateContext
mkBenchCtxWith lay = do
  let dummyTarget = AnyTarget newDummyTarget
  dummySelectedTvar  <- newTVarIO NoSelection
  dummySortedByTvar  <- newTVarIO ByNameUp
  dummyCurrentDirTvar <- newTVarIO (AbsPath "/bench")
  let dummyTd = TargetSessionData
        { currentDir   = dummyCurrentDirTvar
        , sortedFileBy = dummySortedByTvar
        , selected     = dummySelectedTvar
        }
  let dummyTargetView = TargetView
        { target      = dummyTarget
        , sessionData = dummyTd
        }
  pure TemplateContext
    { readOnly         = False
    , noLogin          = True
    , display          = Desktop
    , sidebarCollapsed = False
    , layout           = lay
    , theme            = Light
    , selected         = NoSelection
    , sortedBy         = ByNameUp
    , locale           = EN
    , state            = ControlPanelDefault
    , currentDir       = AbsPath "/bench"
    , currentTarget    = dummyTargetView
    , root             = Root (AbsPath "/bench")
    , simpleAuthUserDB = error "benchCtx: simpleAuthUserDB not set"
    , oidcAuthProviders = error "benchCtx: oidcAuthProviders not set"
    , plugins          = ([] :: [StaticPlugin])
    }
