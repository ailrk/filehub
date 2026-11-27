-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module implements the command line option parser. Config generated from
-- the option parser will be combined with Config generated from the config file
-- to create the final `Config`.
module Filehub.Config.Options (parseOptions , Options(..)) where

import Filehub.Prelude
import Filehub.Config
import Filehub.Auth.Types.Simple qualified as Auth.Simple
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
import Filehub.Theme (Theme (..))
import Data.List.Split (splitOn)
import Filehub.Locale (Locale (..))
import Target.File qualified
import Target.S3 qualified
import Log (LogLevel (..))


data Options = Options
  { optionConfig      :: Config Maybe
  , configFiles       :: [FilePath]
  -- ^ Paths of toml configuration files (merged in order, last wins for scalars).
  , dynamicConfigPath :: Maybe FilePath
  -- ^ Path to the writable dynamic-config file managed by the settings UI.
  }


targetConfig :: Parser TargetConfig
targetConfig = (S3TargetConfig <$> s3TargetConfig) <|> (FSTargetConfig <$> fsTargetConfig)
  where
    s3TargetConfig =
      Target.S3.Config
        <$> option str
              (mconcat
                [ long "s3"
                , metavar "BUCKET"
                , help "S3 bucket"
                ])

    fsTargetConfig =
      Target.File.Config
        <$> option str
              (mconcat
                [ long "fs"
                , metavar "ROOT"
                , help "File system target"
                ])


simpleAuthUserRecords :: Parser SimpleAuthUserRecords
simpleAuthUserRecords = SimpleAuthUserRecords <$> (pure [] <|> some loginUserConfig)


loginUserConfig :: Parser Auth.Simple.UserRecord
loginUserConfig =
  option parseUserRecord
      $ mconcat
      $ [ long "login"
        , metavar "USERNAME PASSWORD"
        ]


parseUserRecord :: ReadM Auth.Simple.UserRecord
parseUserRecord = eitherReader $ \s ->
  case splitOn "," s of
    [u, p] -> Right $ Auth.Simple.UserRecord u p
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


configFilePath :: Parser FilePath
configFilePath = option str
               $ mconcat
               $ [ long "config"
                 , metavar "PATH"
                 , help "Config file (can be specified multiple times; later files override scalars, lists concatenate)"
                 ]


dynamicConfigPath :: Parser (Maybe FilePath)
dynamicConfigPath = optional $ option str
                  $ mconcat
                  $ [ long "dynamic-config"
                    , metavar "PATH"
                    , help "Writable TOML file for settings managed via the UI (created on first change)"
                    ]


locale :: Parser Locale
locale = option auto
      $ mconcat
      $ [ long "theme"
        , metavar "THEME"
        , help "default en. en | zh_cn | zh_tw | zh_hk | es | fr | de | kr | ru | pt | it"
        , value EN
        ]


pluginJsFile :: Parser FilePath
pluginJsFile = option str
             $ mconcat
             $ [ long "plugin-js"
               , metavar "PATH"
               , help "JavaScript plugin file (can be specified multiple times)"
               ]


config :: Parser (Config Maybe)
config =
  Config
  <$> optional port
  <*> optional theme
  <*> optional verbosity
  <*> optional readonly
  <*> optional locale
  <*> pure Nothing
  <*> pure Nothing
  <*> (pure mempty <|> (Targets <$> some targetConfig))
  <*> simpleAuthUserRecords
  <*> pure mempty
  <*> pure mempty
  <*> many pluginJsFile


options :: Parser Options
options = Options <$> config <*> many configFilePath <*> dynamicConfigPath


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper) $
      mconcat
      [ fullDesc
      , progDesc "Filehub"
      ]
