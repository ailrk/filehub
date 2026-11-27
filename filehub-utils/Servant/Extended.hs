module Servant.Extended where

import Filehub.Prelude
import Servant (Link)
import Servant.Links ( linkURI )
import Network.URI qualified as URI


linkToString :: Link -> String
linkToString = ('/':) . (\s -> s "") . URI.uriToString id . linkURI


linkToText :: Link -> Text
linkToText = txt_pack . linkToString
