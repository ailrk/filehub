module Servant.Extended where

import Servant (Link)
import Servant.Links ( linkURI )
import Data.Text (Text)
import Network.URI qualified as URI
import Data.Text qualified as Text


linkToString :: Link -> String
linkToString = ('/':) . (\s -> s "") . URI.uriToString id . linkURI


linkToText :: Link -> Text
linkToText = Text.pack . linkToString
