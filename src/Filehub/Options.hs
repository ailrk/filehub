module Filehub.Options where

import Options.Applicative
import Filehub.Domain (Theme(..))


data Options = Options
  { root :: String
  , port :: Int
  , theme :: Theme
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
    <*> option auto
          (mconcat
            [ long "theme"
            , metavar "THEME"
            , help "dark[1-3], light[1-3]"
            , value Dark1
            ]
          )


parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper) $
      mconcat
      [ fullDesc
      , progDesc "Filehub"
      ]
