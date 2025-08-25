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


data Options = Options
  { optionConfig :: Config Maybe
  , configFile :: Maybe FilePath
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


loginUsersConfig :: Parser [LoginUser]
loginUsersConfig = pure [] <|> some loginUserConfig


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


config :: Parser (Config Maybe)
config =
  Config
  <$> optional port
  <*> optional theme
  <*> optional verbosity
  <*> optional readonly
  <*> optional (some targetConfig)
  <*> optional loginUsersConfig


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
