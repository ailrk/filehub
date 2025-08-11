module Filehub.Options
  ( Options(..)
  , TargetOption(..)
  , FSTargetOption(..)
  , S3TargetOption(..)
  , LoginInfo(..)
  , parseOptions
  )
  where

import Data.List.Split (splitOn)
import Options.Applicative
import Filehub.Theme (Theme(..))
import Log (LogLevel(..))


data Options = Options
  { port :: Int
  , theme :: Theme
  , verbosity :: LogLevel
  , readOnly :: Bool
  , targets :: [TargetOption]
  , loginInfo :: LoginInfo
  }
  deriving (Show)


data LoginInfo
  = LoginInfo1 [(String, String)]
  | LoginInfo2 FilePath
  | NoLogin
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


loginInfoOption :: Parser LoginInfo
loginInfoOption =
  (noLogin *> pure NoLogin)
  <|> (LoginInfo1 <$> some loginUserOption)
  <|> (LoginInfo2 <$> loginInfoFileOption)


loginUserOption :: Parser (String, String)
loginUserOption =
  option parseLoginUser
  $ mconcat
  $ [ long "login"
    , metavar "USERNAME PASSWORD"
    ]


parseLoginUser :: ReadM (String, String)
parseLoginUser = eitherReader $ \s ->
  case splitOn "," s of
    [u, p] -> Right (u, p)
    _      -> Left "Expected USERNAME and PASSWORD separated by space"


loginInfoFileOption :: Parser FilePath
loginInfoFileOption =
  option str
  $ mconcat
  $ [ long "login-config"
    , metavar "CONFIG-FILE"
    ]


noLogin :: Parser Bool
noLogin = switch
         $ mconcat
         $ [ long "no-login"
           , help "Disable login"
           ]


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


options :: Parser Options
options =
  Options
  <$> port
  <*> theme
  <*> verbosity
  <*> readonly
  <*> some targetOption
  <*> loginInfoOption


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper) $
      mconcat
      [ fullDesc
      , progDesc "Filehub"
      ]
