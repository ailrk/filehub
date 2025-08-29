-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the command line option parser. Config generated from
-- the option parser will be combined with Config generated from the config file
-- to create the final `Config`.

module Filehub.Options
  ( parseOptions
  , Options(..)
  )
  where


import Filehub.Config
import Filehub.Auth.Simple (LoginUser (..))
import Options.Applicative
  ( Parser
  , ReadM
  , Alternative (..)
  , option
  , str
  , long
  , metavar
  , help
  , eitherReader
  , switch
  , auto
  , value
  , flag'
  , short
  , execParser
  , info, (<**>)
  , helper
  , fullDesc
  , progDesc
  , optional
  )
import Effectful.Log (LogLevel (..))
import Filehub.Theme (Theme (..))
import Data.List.Split (splitOn)
import Filehub.Locale (Locale (..))


data Options = Options
  { optionConfig :: Config Maybe
  , configFile :: Maybe FilePath
  -- ^ Path of the toml configuration file.
  }


targetConfig :: Parser TargetConfig
targetConfig = (S3TargetConfig <$> s3TargetConfig) <|> (FSTargetConfig <$> fsTargetConfig)
  where
    s3TargetConfig =
        S3TargetConfig_
            <$> option str
                  (mconcat
                    [ long "s3"
                    , metavar "BUCKET"
                    , help "S3 bucket"
                    ])

    fsTargetConfig =
      FSTargetConfig_
        <$> option str
              (mconcat
                [ long "fs"
                , metavar "ROOT"
                , help "File system target"
                ])


simpleAuthLoginUsers :: Parser [LoginUser]
simpleAuthLoginUsers = pure [] <|> some loginUserConfig


loginUserConfig :: Parser LoginUser
loginUserConfig =
  option parseLoginUser
      $ mconcat
      $ [ long "login"
        , metavar "USERNAME PASSWORD"
        ]


parseLoginUser :: ReadM LoginUser
parseLoginUser = eitherReader $ \s ->
  case splitOn "," s of
    [u, p] -> Right $ LoginUser u p
    _      -> Left "Expected USERNAME and PASSWORD separated by space"


port :: Parser Int
port = option auto
     $ mconcat
     $ [ long "port"
       , metavar "PORT"
       , help "port filehub runs on"
       ]


theme :: Parser Theme
theme = option auto
      $ mconcat
      $ [ long "theme"
        , metavar "THEME"
        , help "dark, light"
        , value Dark
        ]


verbosity :: Parser LogLevel
verbosity = toVerbosity . length <$> many (flag' () (short 'v' <> help "Increase verbosity"))
  where
    toVerbosity 0 = LogInfo
    toVerbosity 1 = LogAttention
    toVerbosity 2 = LogInfo
    toVerbosity 3 = LogTrace
    toVerbosity _ = LogTrace


readonly :: Parser Bool
readonly = switch
         $ mconcat
         $ [ long "readonly"
           , help "Enable read only mode"
           ]


configFile :: Parser FilePath
configFile = option str
           $ mconcat
           $ [ long "config-file"
             , help "Config file"
             ]


locale :: Parser Locale
locale = option auto
      $ mconcat
      $ [ long "theme"
        , metavar "THEME"
        , help "default EN. EN | ZH_CN | ZH_TW | ZH_HK | ES | FR | DE | KR | RU | PT | IT"
        , value EN
        ]


config :: Parser (Config Maybe)
config =
  Config
  <$> optional port
  <*> optional theme
  <*> optional verbosity
  <*> optional readonly
  <*> optional locale
  <*> optional (some targetConfig)
  <*> optional simpleAuthLoginUsers
  <*> pure Nothing


options :: Parser Options
options = Options <$> config <*> optional configFile


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper) $
      mconcat
      [ fullDesc
      , progDesc "Filehub"
      ]
