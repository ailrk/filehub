module Servant.Extended where

import Data.Text (Text)
import Data.Text qualified as T
import Servant (Link)
import Servant.Links ( linkURI )
import Network.URI qualified as URI


linkToString :: Link -> String
linkToString = ('/':) . (\s -> s "") . URI.uriToString id . linkURI


linkToText :: Link -> Text
linkToText = T.pack . linkToString
