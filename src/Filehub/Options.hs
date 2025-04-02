module Filehub.Options
  ( Options(..)
  , TargetOption(..)
  , parseOptions
  )
  where

import Options.Applicative
import Filehub.Domain (Theme(..))


data Options = Options
  { port :: Int
  , theme :: Theme
  , targets :: [TargetOption]
  }
  deriving (Show)


data TargetOption = TargetOption
  { root :: FilePath
  }
  deriving (Show)


targetOption :: Parser TargetOption
targetOption =
  TargetOption
    <$> option str
          (mconcat
            [ long "root"
            , metavar "PATH"
            , help "root path to serve the file"
            ])


options :: Parser Options
options =
  Options
    <$> option auto
          (mconcat
            [ long "port"
            , metavar "PORT"
            , help "port filehub runs on"
            ])
    <*> option auto
          (mconcat
            [ long "theme"
            , metavar "THEME"
            , help "dark[1-3], light[1-3]"
            , value Dark1
            ]
          )
    <*> some targetOption


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper) $
      mconcat
      [ fullDesc
      , progDesc "Filehub"
      ]
