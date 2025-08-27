{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Avoid restricted function" #-}
module Filehub.Links
  ( sitemapLinks
  , sitemap
  , apiLinks
  , linkToText
  , linkToString
  )
  where

import Filehub.Routes (Api(..))
import Filehub.Types (ClientPath (..), SortFileBy (..), TargetId (..), OpenTarget (..))
import Servant.Links ( AsLink, allFieldLinks, linkURI )
import Servant (Link)
import Network.URI qualified as URI
import Data.UUID qualified as UUID
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Filehub.Layout (Layout(..))


apiLinks :: Api (AsLink Link)
apiLinks = allFieldLinks @Api


sitemapLinks :: [Link]
sitemapLinks =
  [ apiLinks.home
  , apiLinks.cd $ Just (ClientPath "dummy.txt")
  , apiLinks.newFile
  , apiLinks.updateFile
  , apiLinks.deleteFile [ClientPath "dummy.txt"] True
  , apiLinks.copy
  , apiLinks.paste
  , apiLinks.newFolder
  , apiLinks.newFileModal
  , apiLinks.newFolderModal
  , apiLinks.fileDetailModal $ Just (ClientPath "dummy.txt")
  , apiLinks.editorModal $ Just (ClientPath "dummy.txt")
  , apiLinks.search
  , apiLinks.sortTable (Just BySizeDown)
  , apiLinks.selectLayout (Just ThumbnailLayout)
  , apiLinks.selectRows
  , apiLinks.upload
  , apiLinks.download $ [ClientPath "dummy.txt"]
  , apiLinks.contextMenu $ [ ClientPath "dummy.txt"]
  , apiLinks.initViewer $ Just (ClientPath "dummy.txt")
  , apiLinks.open (Just OpenDOMBlank) $ Just (ClientPath "dummy.txt")
  , apiLinks.changeTarget $ Just (TargetId $ fromJust $ UUID.fromString "8d8ac610-566d-4ef0-9c22-186b2a5ed793")
  , apiLinks.themeCss
  , apiLinks.healthz
  ]


sitemap :: [String]
sitemap = sitemapLinks
        & fmap linkURI
        & fmap (URI.uriToString id)
        & fmap (\s -> s "")


linkToString :: Link -> String
linkToString = ('/':) . (\s -> s "") . URI.uriToString id . linkURI


linkToText :: Link -> Text
linkToText = Text.pack . linkToString
