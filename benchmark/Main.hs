{-# LANGUAGE NumericUnderscores #-}
module Main where

import Criterion.Main

import Data.ByteString.Char8 qualified as BC
import Data.ClientPath (AbsPath(..), Root(..), ClientPath(..), toClientPath, fromClientPath, withinRoot)
import Data.File (File(..), FileType)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Filehub.RateLimit qualified as RateLimit
import Filehub.SharedLink
import Filehub.Sort (sortFiles, SortFileBy(..))
import Filehub.Template (runTemplate)
import Filehub.Template.Desktop (table)
import Lucid (renderBS)
import Target.Types (TargetId(..))
import Data.UUID.V4 qualified as UUID
import UnliftIO.Async (mapConcurrently)
import UnliftIO.STM (newTVarIO, readTVar, atomically)

import Gen


benchRoot :: Root
benchRoot = Root (AbsPath "/home/user/files")

benchDeepRoot :: Root
benchDeepRoot = Root (AbsPath "/home/user/projects/work/client/data/archive/files")


main :: IO ()
main = do
  let files10    = mkFiles 10
      files100   = mkFiles 100
      files1000  = mkFiles 1000
      files10000 = mkFiles 10_000
      mixed100   = mkMixedFiles 100
      mixed1000  = mkMixedFiles 1000

  pool10   <- buildPool 10
  pool100  <- buildPool 100
  pool1000 <- buildPool 1000
  knownHash10   <- pickHash pool10
  knownHash100  <- pickHash pool100
  knownHash1000 <- pickHash pool1000

  rlB1    <- RateLimit.new 1_000_000 60
  rlB2    <- RateLimit.new 1_000_000 60
  rlBurst <- RateLimit.new 1_000_000 60
  rlConc  <- RateLimit.new 1_000_000 60

  sessionTvar <- newTVarIO (Map.empty :: Map.Map Int Int)

  defaultMain
    [ -- ── 1. Sort ─────────────────────────────────────────────────────────
      bgroup "Sort"
        [ bgroup "10 files"              (sortBenches files10)
        , bgroup "100 files"             (sortBenches files100)
        , bgroup "1000 files"            (sortBenches files1000)
        , bgroup "10000 files"           (sortBenches files10000)
        , bgroup "mixed dirs+files 100"  (sortBenches mixed100)
        , bgroup "mixed dirs+files 1000" (sortBenches mixed1000)
        ]

    , -- ── 2. ClientPath ───────────────────────────────────────────────────
      bgroup "ClientPath"
        [ bgroup "toClientPath"
            [ bench "shallow root / short path" $ nf
                (toClientPath benchRoot)
                (AbsPath "/home/user/files/readme.txt")
            , bench "deep root / deep path" $ nf
                (toClientPath benchDeepRoot)
                (AbsPath "/home/user/projects/work/client/data/archive/files/2024/q4/report.pdf")
            , bench "batch 100" $ nf
                (map (toClientPath benchRoot . (.path)))
                files100
            ]
        , bgroup "fromClientPath"
            [ bench "shallow" $ nf
                (fromClientPath benchRoot)
                (ClientPath "readme.txt")
            , bench "deep" $ nf
                (fromClientPath benchDeepRoot)
                (ClientPath "2024%2Fq4%2Freport.pdf")
            , bench "batch 100 (round-trip)" $ nf
                (map (\f -> fromClientPath benchRoot (toClientPath benchRoot f.path)))
                files100
            ]
        , bgroup "withinRoot"
            [ bench "inside  (true)"  $ nf (withinRoot benchRoot) (AbsPath "/home/user/files/readme.txt")
            , bench "outside (false)" $ nf (withinRoot benchRoot) (AbsPath "/etc/passwd")
            , bench "batch 100"       $ nf (map (withinRoot benchRoot . (.path))) files100
            ]
        ]

    , -- ── 3. SharedLink ───────────────────────────────────────────────────
      bgroup "SharedLink"
        [ bench "mkSharedLinkHash" $ nf
            mkSharedLinkHash
            (BC.pack "/home/user/files/secret-document.pdf")
        , bench "lookup hit  pool=10"   $ nfIO (lookupSharedLink knownHash10   pool10)
        , bench "lookup hit  pool=100"  $ nfIO (lookupSharedLink knownHash100  pool100)
        , bench "lookup hit  pool=1000" $ nfIO (lookupSharedLink knownHash1000 pool1000)
        , bench "lookup miss pool=1000" $ nfIO
            (lookupSharedLink (mkSharedLinkHash "not-in-pool") pool1000)
        , bench "10 concurrent lookups pool=1000" $ nfIO $
            mapConcurrently
              (\_ -> lookupSharedLink knownHash1000 pool1000)
              [1..10 :: Int]
        ]

    , -- ── 4. RateLimit ────────────────────────────────────────────────────
      bgroup "RateLimit.check"
        [ bench "single key"                $ nfIO (RateLimit.check rlB1 "bench-ip")
        , bench "10 distinct keys parallel" $ nfIO $
            mapConcurrently
              (\i -> RateLimit.check rlB2 (T.pack (show (i :: Int))))
              [1..10]
        , bench "burst 1000 same key" $ nfIO $
            mapConcurrently
              (\_ -> RateLimit.check rlBurst "flood-ip")
              [1..1000 :: Int]
        , bench "100 distinct keys parallel" $ nfIO $
            mapConcurrently
              (\i -> RateLimit.check rlConc (T.pack (show (i :: Int))))
              [1..100 :: Int]
        ]

    , -- ── 5. Session pool (raw STM TVar read) ─────────────────────────────
      bgroup "SessionPool (STM read)"
        [ bench "single reader" $ nfIO $
            atomically (readTVar sessionTvar)
        , bench "10 concurrent readers" $ nfIO $
            mapConcurrently
              (\_ -> atomically (readTVar sessionTvar))
              [1..10 :: Int]
        , bench "50 concurrent readers" $ nfIO $
            mapConcurrently
              (\_ -> atomically (readTVar sessionTvar))
              [1..50 :: Int]
        ]

    , -- ── 6. Template rendering ────────────────────────────────────────────
      bgroup "Template / list layout"
        [ bench "10 files"   $ nf (renderBS . runTemplate benchCtx . table) files10
        , bench "100 files"  $ nf (renderBS . runTemplate benchCtx . table) files100
        , bench "1000 files" $ nf (renderBS . runTemplate benchCtx . table) files1000
        ]

    , bgroup "Template / thumbnail layout"
        [ bench "10 files"   $ nf (renderBS . runTemplate benchCtxThumbnail . table) files10
        , bench "100 files"  $ nf (renderBS . runTemplate benchCtxThumbnail . table) files100
        , bench "1000 files" $ nf (renderBS . runTemplate benchCtxThumbnail . table) files1000
        ]
    ]


-- ─── Helpers ─────────────────────────────────────────────────────────────────

sortBenches :: [File FileType] -> [Benchmark]
sortBenches files =
  [ bench "ByNameUp"       $ nf (sortFiles ByNameUp)       files
  , bench "ByNameDown"     $ nf (sortFiles ByNameDown)     files
  , bench "ByModifiedUp"   $ nf (sortFiles ByModifiedUp)   files
  , bench "ByModifiedDown" $ nf (sortFiles ByModifiedDown) files
  , bench "BySizeUp"       $ nf (sortFiles BySizeUp)       files
  , bench "BySizeDown"     $ nf (sortFiles BySizeDown)     files
  ]


buildPool :: Int -> IO SharedLinkPool
buildPool n = do
  p   <- newShareLinkPool
  tid <- TargetId <$> UUID.nextRandom
  mapM_ (\i -> do
    link <- createSharedLink (mkFileInfo i) tid Indefinite False Nothing
    addSharedLink link p
    ) [1..n]
  pure p


pickHash :: SharedLinkPool -> IO SharedLinkHash
pickHash p = do
  links <- getAllSharedLinks p
  case links of
    []    -> error "pickHash: empty pool"
    (l:_) -> pure l.hash
