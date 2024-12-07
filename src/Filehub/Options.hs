module Filehub.Options where

import Options.Applicative


data Options = Options
  { root :: String
  , port :: Int
  }


options :: Parser Options
options =
  Options
    <$> option str
          (mconcat
            [ long "root"
            , metavar "PATH"
            , help "root path to serve the file"
            , value "."
            ])
    <*> option auto
          (mconcat
            [ long "port"
            , metavar "PORT"
            , help "port filehub runs on"
            , value 8000
            ])


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper) $
      mconcat
      [ fullDesc
      , progDesc "Filehub"
      ]
