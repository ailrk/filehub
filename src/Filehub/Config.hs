module Filehub.Config where


import Filehub.Theme (Theme (..))
import Filehub.Auth.Simple (LoginUser)
import Effectful.Log (LogLevel (..))
import Data.Functor.Identity
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.List (nub)


data Config f = Config
  { port :: f Int
  , theme :: f Theme
  , verbosity :: f LogLevel
  , readOnly :: f Bool
  , targets :: f [TargetConfig]
  , loginUsers :: f [LoginUser]
  }


data TargetConfig
  = FSTargetConfig FSTargetConfig
  | S3TargetConfig S3TargetConfig
  deriving (Show, Eq)


data FSTargetConfig = FSTargetConfig_
  { root :: FilePath
  }
  deriving (Show, Eq)


data S3TargetConfig = S3TargetConfig_
  { bucket :: String
  }
  deriving (Show, Eq)


-- | Left config overrides the right one
merge :: Config Maybe -> Config Maybe -> Either String (Config Identity)
merge cfg1 cfg2 = do
  port      <-  maybe (Left "port is missing")     Right $ cfg1.port <|> cfg2.port
  theme     <-  maybe (Right Dark)                 Right $ cfg1.theme <|> cfg2.theme
  verbosity <-  maybe (Right LogInfo)              Right $ cfg1.verbosity <|> cfg2.verbosity
  readOnly  <-  maybe (Right True)                 Right $ cfg1.readOnly <|> cfg2.readOnly
  let targets    = nub . mconcat . fmap (fromMaybe []) $ [cfg1.targets, cfg2.targets]
  let loginUsers = nub . mconcat . fmap (fromMaybe []) $ [cfg1.loginUsers, cfg2.loginUsers]
  pure
    Config
      { port = Identity port
      , theme = Identity theme
      , verbosity = Identity verbosity
      , readOnly = Identity readOnly
      , targets = Identity targets
      , loginUsers = Identity loginUsers
      }
