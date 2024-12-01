{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module Filehub.Index where

import Effectful (Eff, (:>), IOE)
import Lucid
import Filehub.Template (withDefault)
import Effectful.Reader.Dynamic (Reader, asks)
import Filehub.Env (Env(..))
import Filehub.Domain (lsCurrentDir, changeDir, File(..), FileContent (..), loadDirContents, dirtree)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack, throwError)
import Effectful.FileSystem (FileSystem)
import Servant (Get, errBody, err500, (:-), ServerError (..), QueryParam, Post, Put, ReqBody, PlainText, FormUrlEncoded)
import Servant qualified as S
import Servant.HTML.Lucid (HTML)
import Data.String (IsString(..))
import Data.Foldable
import UnliftIO (readIORef, writeIORef)
import System.FilePath (splitPath, takeFileName)
import Servant.Server.Generic (AsServerT)
import GHC.Generics (Generic)
import Data.Sequence qualified as Seq
import Data.Bifunctor (Bifunctor(..))
import Data.Sequence (Seq(..))
import Data.Text qualified as Text
import Data.Maybe (fromMaybe, listToMaybe)
import Lens.Micro
import Data.Text (Text)
import Servant.Multipart (Mem, MultipartForm, MultipartData(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Fuzzy (simpleFilter)
import Web.FormUrlEncoded


data Api mode = Api
  { index :: mode :- Get '[HTML] (Html ())
  , cd :: mode :- "cd" S.:> QueryParam "dir" FilePath S.:> Get '[HTML] (Html ())
  , dirs :: mode :- "dirs" S.:> QueryParam "path" FilePath S.:> Get '[HTML] (Html ())
  , newFile :: mode :- "files" S.:> "new" S.:> Put '[HTML] (Html ())
  , upload :: mode :- "files" S.:> "upload" S.:> MultipartForm Mem (MultipartData Mem) S.:> Post '[HTML] (Html ())
  , newFolder :: mode :- "folder" S.:> "new" S.:> Put '[HTML] (Html ())
  , info :: mode :- "info" S.:> Get '[HTML] (Html ())
  , search :: mode :- "search" S.:> ReqBody '[FormUrlEncoded] SearchWord S.:> Post '[HTML] (Html ())
  }
  deriving (Generic)


newtype SearchWord = SearchWord Text deriving (Show, Eq, Generic)

instance FromForm SearchWord where
  fromForm f = SearchWord <$> parseUnique "search" f


server
  :: ( Reader Env :> es
     , Error ServerError :> es
     , FileSystem :> es, IOE :> es )
  => Api (AsServerT (Eff es))
server = Api
  { index = index
  , cd = cd
  , dirs = dirs
  , newFile = newFile
  , newFolder = newFolder
  , upload = upload
  , info = info
  , search = search
  }


withServerError :: (Error ServerError :> es) => Eff (Error String : es) b -> Eff es b
withServerError action = do
  runErrorNoCallStack @String action >>= \case
    Left err -> throwError err500 { errBody = fromString  err }
    Right res -> pure res


cd
  :: ( Reader Env :> es
     , Error ServerError :> es
     , IOE :> es
     , FileSystem :> es)
  => Maybe FilePath -> Eff es (Html ())
cd path = do
  withServerError $ changeDir (fromMaybe "/" path)
  view


dirs
  :: ( Reader Env :> es
     , Error ServerError :> es
     , IOE :> es
     , FileSystem :> es)
  => Maybe FilePath -> Eff es (Html ())
dirs Nothing = tree
dirs (Just path) = do
  ref <- asks @Env (.rootTree)
  root <- readIORef ref
  case listToMaybe (root ^.. fileL) of
    Nothing -> pure ()
    Just f@File { content = Dir Nothing } -> do
      f' <- withServerError $ loadDirContents f
      let root' = root & fileL .~ f'
      ref `writeIORef` root'
    Just f@File { content = Dir (Just _)} -> do
      let f' = f & #content .~ Dir Nothing
      let root' = root & fileL .~ f'
      ref `writeIORef` root'
    _ -> pure ()
  tree
  where
    fileL :: Traversal' File File
    fileL = dirtree . filtered (\s -> s.path == path)


newFile :: Eff es (Html ())
newFile = undefined


newFolder :: Eff es (Html ())
newFolder = undefined


upload :: MultipartData Mem -> Eff es (Html ())
upload multipartForm  = undefined


info :: Eff es (Html ())
info = undefined


search
  :: ( Error ServerError :> es
     , FileSystem :> es
     , Reader Env :> es
     , IOE :> es
     )
  => SearchWord -> Eff es (Html ())
search (SearchWord searchWord) = do
  files <- withServerError lsCurrentDir
  let matched = files <&> Text.pack . (.path) & simpleFilter searchWord
  let isMatched file = Text.pack file.path `elem` matched
  let filteredFiles = files ^.. each . filtered isMatched
  pure $ table filteredFiles



------------------------------------
-- index
------------------------------------


index
  :: ( Reader Env :> es
     , Error ServerError :> es
     , FileSystem :> es
     , IOE :> es )
  => Eff es (Html ())
index = do
  controlPanel' <- controlPanel
  view' <- view
  tree' <- tree
  pure do
    withDefault do
      div_ [ class_ "filehub " ] do
        controlPanel'
        tree'
        view'


controlPanel :: Eff es (Html ())
controlPanel = do
  pure $ do
    div_ [ id_ elementId ] do
      newFolderBtn
      newFileBtn
      uploadBtn
      sortByBtn
      viewTypeBtn
      infoBtn
  where
    elementId = componentIds.controlPanel

    newFolderBtn :: Html ()
    newFolderBtn = button_ [class_ "btn btn-control", type_ "submit"] "New Folder"

    newFileBtn :: Html ()
    newFileBtn  = button_ [class_ "btn btn-control", type_ "submit"] "New File"

    uploadBtn :: Html ()
    uploadBtn = button_ [class_ "btn btn-control", type_ "submit"] "Upload"

    sortByBtn :: Html ()
    sortByBtn = button_ [class_ "btn btn-control", type_ "submit"] "Sort By"

    viewTypeBtn :: Html ()
    viewTypeBtn = button_ [class_ "btn btn-control", type_ "submit"] "view"

    infoBtn :: Html ()
    infoBtn = button_ [class_ "btn btn-control", type_ "submit"] "info"


view
  :: ( Error ServerError :> es
     , FileSystem :> es
     , Reader Env :> es
     , IOE :> es
     )
  => Eff es (Html ())
view = do
  files <- withServerError lsCurrentDir
  let table' = table files
  pathBreadcrumb' <- pathBreadcrumb
  pure do
    div_ [ id_ componentIds.view ] do
      div_ [ id_ "tool-bar" ] do
        pathBreadcrumb'
        searchBar
      table'


pathBreadcrumb
  :: ( Reader Env :> es
     , IOE :> es
     )
  => Eff es (Html ())
pathBreadcrumb = do
  currentDir <- asks @Env (.currentDir) >>= readIORef
  root <- asks @Env (.root)
  let afterRoot path = length (splitPath path) >= length (splitPath root)
      breadcrumbItems =
        currentDir
        & splitPath
        & scanl1 (++)
        & (\xs -> if null xs then ["/"] else xs)
        & filter afterRoot
        & fmap toAttrsTuple
        & Seq.fromList
        & adjustLast (addAttr " active")
        & fmap toLi
        & sequence_
  pure do
    div_ [ class_ "breadcrumb", id_ componentIds.pathBreadcrumb ] do
      ol_ breadcrumbItems
  where
    adjustLast f xs = Seq.adjust f (length xs - 1) xs

    toAttrsTuple p = (attrs, p)
      where
        attrs =
          [ term "hx-get" ("/cd?dir=" <> Text.pack p)
          , term "hx-target" ("#" <> componentIds.view)
          , term "hx-swap" "outerHTML"
          ]

    addAttr a = first (class_ a :)

    toLi :: ([Attribute], FilePath) -> Html ()
    toLi (attrs, p) = li_ attrs . toHtml . pathShow $ p

    pathShow p =
      case Seq.fromList (splitPath p) of
        "/" :<| Seq.Empty  -> "Files"
        _ :|> l ->
          case Seq.fromList l of
            xs :|> '/' -> toList xs
            xs -> toList xs
        _ -> ""


table :: [File] -> Html ()
table files = do
  table_ [ id_ componentIds.table ] do
    thead_ do
      tr_ do
        th_ [ id_ "table-name" ] "Name"
        th_ [ id_ "table-modified" ] "Modified"
        th_ [ id_ "table-size" ] "Size"
    tbody_ $ do
      traverse_
        (\file@(File { size = size, mtime = mtime }) -> do
          tr_ do
            td_
              (fileNameElement file)
            td_ (toHtml $ formatTime defaultTimeLocale "%F %R" mtime)
            td_ (toHtml . show $ size))
        files
  where
    fileNameElement :: File -> Html ()
    fileNameElement file = do
      span_ [ class_ "file-name" ] do
        icon
        span_ attrs do
          toHtml . takeFileName $ file.path
      where
        icon =
          case file.content of
            Dir _ -> i_ [ class_ "bx bxs-folder"] mempty
            Content _ -> i_ [ class_ "bx bxs-file-blank"] mempty
        cdAttrs =
          [ class_ "breadcrumb-item "
          , term "hx-get" ("/cd?dir=" <> Text.pack file.path)
          , term "hx-target" ("#" <> componentIds.view)
          , term "hx-swap" "outerHTML"
          ]
        otherAttrs =
          case file.content of
            Dir _ -> [ class_ "dir " ]
            _ -> []
        attrs = mconcat [ cdAttrs, otherAttrs ]


searchBar :: Html ()
searchBar = do
  div_ [ id_ componentIds.searchBar ] do
    input_ [ class_ "form-control "
           , type_ "search"
           , name_ "search"
           , placeholder_ "Search as you type"
           , term "hx-post" "/search"
           , term "hx-trigger" "input changed delay:200ms, search"
           , term "hx-target" "#table"
           , term "hx-swap" "outerHTML"
           , term "hx-indicator" ".htmx-indicator"
           ]

tree
  :: ( Reader Env :> es
     , IOE :> es
     )
  => Eff es (Html ())
tree = do
  root <- asks @Env (.rootTree) >>= readIORef
  pure do
    ul_ [ id_ elementId ] $ renderFile 0 root
  where
    elementId = componentIds.tree

    cdAttrs p =
      [ term "hx-get" ("/cd?dir=" <> Text.pack p)
      , term "hx-target" ("#" <> componentIds.view)
      , term "hx-swap" "outerHTML"
      ]

    treeFoldAttrs p =
      [ term "hx-get" ("/dirs?path=" <> Text.pack p)
      , term "hx-swap" "outerHTML"
      , term "hx-target" ("#" <> elementId)
      ]

    indent n = term "indent" (Text.pack . show $ n)

    renderFile :: Int -> File -> Html ()
    renderFile n file = do
      let path = file.path
      let name = toHtml . takeFileName $ path
      case file.content of
        Content _ -> do
          li_ [ class_ "dirtree-entry file-name", indent n ] name

        Dir Nothing -> do
          let icon = i_ [ class_ "bx bx-caret-right"] mempty
          li_ [ class_ "dirtree-entry dir file-name", indent n ] do
            span_ (treeFoldAttrs path) icon
            span_ (cdAttrs path) name

        Dir (Just files) -> do
          let icon = i_ [ class_ "bx bx-caret-down"] mempty
          li_ [ class_ "dirtree-entry dir file-name", indent n ] do
            span_ (treeFoldAttrs path) icon
            span_ (cdAttrs path) name
          traverse_ (renderFile (n + 1)) files


------------------------------------
-- index
------------------------------------


data ComponentIds = ComponentIds
  { tree :: Text
  , view :: Text
  , controlPanel :: Text
  , searchBar :: Text
  , pathBreadcrumb :: Text
  , table :: Text
  }
  deriving Show


componentIds :: ComponentIds
componentIds = ComponentIds
  { tree = "tree"
  , view = "view"
  , controlPanel = "control-panel"
  , searchBar = "search-bar"
  , pathBreadcrumb = "path-breadcrumb"
  , table = "table"
  }
