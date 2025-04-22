module Filehub.Options
  ( Options(..)
  , TargetOption(..)
  , FSTargetOption(..)
  , S3TargetOption(..)
  , parseOptions
  )
  where

import Options.Applicative
import Filehub.Types (Theme(..))


data Options = Options
  { port :: Int
  , theme :: Theme
  , targets :: [TargetOption]
  }
  deriving (Show)


data TargetOption
  = FSTargetOption FSTargetOption
  | S3TargetOption S3TargetOption
  deriving (Show)


data FSTargetOption = FSTargetOption_
  { root :: FilePath
  }
  deriving (Show)


data S3TargetOption = S3TargetOption_
  { bucket :: String
  }
  deriving (Show)


targetOption :: Parser TargetOption
targetOption = (S3TargetOption <$> s3TargetOption) <|> (FSTargetOption <$> fsTargetOption)
  where
    s3TargetOption =
        S3TargetOption_
            <$> option str
                  (mconcat
                    [ long "s3"
                    , metavar "BUCKET"
                    , help "S3 bucket"
                    ])

    fsTargetOption =
      FSTargetOption_
        <$> option str
              (mconcat
                [ long "fs"
                , metavar "ROOT"
                , help "File system target"
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
            , help "dark, light"
            , value Dark
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
