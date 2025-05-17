{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Avoid restricted function" #-}
module Filehub.Links
  ( sitemapLinks
  , sitemap
  )
  where

import Filehub.Routes (Api(..))
import Filehub.Types (ClientPath (..), SortFileBy (..), TargetId (..))
import Servant.Links ( AsLink, allFieldLinks, linkURI )
import Servant (Link)
import Network.URI qualified as URI
import Data.UUID qualified as UUID
import Data.Function ((&))
import Data.Maybe (fromJust)


dummyClientPath :: Maybe ClientPath
dummyClientPath = Just (ClientPath "dummy.txt")


dummySort :: Maybe SortFileBy
dummySort = Just BySizeDown


dummyTargetId :: Maybe TargetId
dummyTargetId = Just (TargetId $ fromJust $ UUID.fromString "8d8ac610-566d-4ef0-9c22-186b2a5ed793")


apiLinks :: Api (AsLink Link)
apiLinks = allFieldLinks @Api


sitemapLinks :: [Link]
sitemapLinks =
  [ apiLinks.index
  , apiLinks.cd dummyClientPath
  , apiLinks.newFile
  , apiLinks.updateFile
  , apiLinks.deleteFile dummyClientPath True
  , apiLinks.newFolder
  , apiLinks.newFileModal
  , apiLinks.newFolderModal
  , apiLinks.fileDetailModal dummyClientPath
  , apiLinks.editorModal dummyClientPath
  , apiLinks.search
  , apiLinks.sortTable dummySort
  , apiLinks.selectRows
  , apiLinks.upload
  , apiLinks.download dummyClientPath
  , apiLinks.contextMenu dummyClientPath
  , apiLinks.initViewer dummyClientPath
  , apiLinks.changeTarget dummyTargetId
  , apiLinks.themeCss
  , apiLinks.healthz
  ]


sitemap :: [String]
sitemap = sitemapLinks
        & fmap linkURI
        & fmap (URI.uriToString id)
        & fmap (\s -> s "")
