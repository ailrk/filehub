{-# LANGUAGE NamedFieldPuns #-}
module Main (main, mainDev) where

import Data.Functor ((<&>))
import Data.String.Interpolate (iii)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Filehub.Display (Display(..))
import Filehub.Locale (Locale(..))
import Filehub.Sort (SortFileBy(..))
import Filehub.Template
import Filehub.Template qualified as Template
import Filehub.Template.Desktop qualified as Template.Desktop
import Filehub.Template.Mobile qualified as Template.Mobile
import Filehub.Theme (Theme(..))
import Filehub.Types ( ControlPanelState(..), Layout(..), Selected(..) )
import Lucid
import Network.HTTP.Types (status200)
import Network.Mime qualified as Mime
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import Data.ByteString.Lazy qualified as LBS
import Control.Monad (join)
import Target.Dummy qualified
import Target.Types (Target(..))
import Filehub.Session.Types ( TargetView(..), TargetSessionData(..) )
import Data.File (FileInfo, File(..), FileType(..))
import Data.Time.QQ (utcIso8601, UTCTime (..))
import Data.ClientPath qualified as ClientPath
import System.Process (readProcess)


data Options = Options
  { port :: Int
  }


optionsParser :: Parser Options
optionsParser = Options <$> option auto (short 'p' <> help "port" <> value 9843)


main :: IO ()
main = do
  options <- execParser (info (optionsParser <**> helper) mempty)
  mainDev options.port


mainDev :: Int -> IO ()
mainDev port = do
  putStrLn ("STARTING STORYBOOK on port " <> show port <> "....")
  run port app


app :: Application
app req respond = do
  let trim =  reverse . dropWhile (`elem` ['\n', '\r']). reverse
  dir <- trim <$> readProcess "git" ["rev-parse", "--show-toplevel"] "" >>= makeAbsolute <&> (++ "/data/filehub")

  case req.pathInfo of
    [] -> respond do
      responseLBS
        status200
        [ ("Content-Type", "text/html; charset=utf-8")
        , ("Set-Cookie",
            case (lookup "display" req.queryString) of
              Just (Just "desktop") -> "display=Desktop; Path=/"
              Just (Just "mobile") -> "display=Mobile; Path=/"
              _ -> "display=NoDisplay; Path=/"
          )
        ]
        (html
          (lookup "story" req.queryString)
          (lookup "display" req.queryString))

    "static":rest -> do
      let path     = dir </> Text.unpack (Text.intercalate "/" rest)
      let mimetype = Mime.defaultMimeLookup (Text.pack path)
      respond do responseFile status200 [("Content-Type", mimetype)] path Nothing

    "theme.css":_ -> do
      let path = case lookup "theme" req.queryString of
                   Just (Just "light") -> dir </> "theme-light.css"
                   _ -> dir </> "theme-dark.css"
      bytes <- LBS.readFile path
      respond do responseLBS status200 [("Content-Type", "text/css")] bytes

    "mainifest.json":_ -> do
      let path = dir </> "mainifest.json"
      respond do responseFile status200 [("Content-Type", "application/json")] path Nothing

    "favicon.ico":_ -> do
      let path = dir </> "favicon.ico"
      respond do responseFile status200 [("Content-Type", "image/ico")] path Nothing

    _ -> do
      respond do
        responseLBS status200 [("Content-Type", "text/plain")] "Not Found"
  where
    html mStory mDisplay = do
      let display = maybe Mobile (\case "desktop" -> Desktop; "mobile" -> Mobile; _ -> error "impossible") (join mDisplay)
      let ctx = defaultCtx { display = display }

      let content = do
            doctypehtml_ $ do
              Template.withDefault ctx.display "#000000" do
                case mStory of
                  Just (Just "editor") -> do
                    case ctx.display of
                      Mobile    -> Template.Mobile.editorModal False "filename" "File content"
                      Desktop   -> runTemplate ctx $ Template.Desktop.editorModal "filename" "File content"
                      NoDisplay -> mempty

                  Just (Just "control-panel") -> do
                    case ctx.display of
                      Mobile    -> runTemplate ctx Template.Mobile.controlPanel
                      Desktop   -> runTemplate ctx Template.Desktop.controlPanel
                      NoDisplay -> mempty

                  Just (Just "view") -> do
                    case ctx.display of
                      Mobile    -> do
                        let table = runTemplate ctx $ Template.Mobile.table files
                        Template.Mobile.view table
                      Desktop   -> do
                        let table = runTemplate ctx $ Template.Desktop.table files
                        Template.Desktop.view table
                      NoDisplay -> mempty

                  Just (Just "new-folder")    -> runTemplate ctx Template.Desktop.newFolderModal
                  Just (Just "new-file")      -> runTemplate ctx Template.Desktop.newFileModal
                  Just (Just "locale-button") -> Template.Desktop.localeBtn
                  Just (Just "contextmenu1")  -> runTemplate ctx $ Template.Desktop.contextMenu1 (head files)
                  Just (Just "contextmenuN")  -> runTemplate ctx $ Template.Desktop.contextMenuMany (fmap (ClientPath.toClientPath "/" . (.path)) files)
                  Nothing -> mempty
                  _ -> "unknown story"

      renderBS do
        html_ do
          head_ do
            style_ previewCSS

          body_ do
            div_ [ id_ "preview-side-bar" ] do
              ul_ do
                li_ do a_ [ href_ "/?story=editor&display=desktop" ]          "D editor"
                li_ do a_ [ href_ "/?story=editor&display=mobile" ]           "M editor"

                li_ do a_ [ href_ "/?story=control-panel&display=desktop" ]   "D control-panel"
                li_ do a_ [ href_ "/?story=control-panel&display=mobile" ]    "M control-panel"

                li_ do a_ [ href_ "/?story=view&display=desktop" ]            "D view"
                li_ do a_ [ href_ "/?story=view&display=mobile" ]             "M view"

                li_ do a_ [ href_ "/?story=new-folder&display=desktop" ]      "D new-folder"
                li_ do a_ [ href_ "/?story=new-file&display=desktop" ]        "D new-file"
                li_ do a_ [ href_ "/?story=locale-button&display=desktop" ]   "D locale-button"
                li_ do a_ [ href_ "/?story=contextmenu1&display=desktop" ]    "D contextmenu1"
                li_ do a_ [ href_ "/?story=contextmenuN&display=desktop" ]    "D contextmenuN"

            div_ [ id_ "preview-container" ] do
              iframe_ [ id_ "preview-frame"
                      , class_ do
                          case display of
                            Desktop -> "desktop "
                            _ -> "mobile "
                      , srcdoc_ (LText.toStrict (renderText content))
                      , sandbox_ "allow-same-origin allow-scripts" ]
                      mempty

    defaultCtx = TemplateContext
        { readOnly        = False
        , noLogin         = False
        , display         = Mobile
        , layout          = ThumbnailLayout
        , theme           = Dark
        , sortedBy        = ByNameDown
        , selected        = NoSelection
        , state           = ControlPanelDefault
        , root            = ""
        , locale          = EN
        , currentDir      = ""
        , currentTarget   = TargetView
                              { target = Target (Target.Dummy.newDummyTarget)
                              , sessionData = TargetSessionData
                                              { currentDir = ""
                                              , sortedFileBy = ByNameUp
                                              , selected = NoSelection
                                              }
                              , index = 0
                              }
        , simpleAuthUserDB   = undefined
        , oidcAuthProviders  = undefined
        }


files :: [FileInfo]
files =
  [ File
    { path     = "/foo/bar/cat.png"
    , atime    = Just [utcIso8601| 1998-12-01 |]
    , mtime    = Just [utcIso8601| 1998-11-01 |]
    , size     = Just 1000
    , mimetype = "image/png"
    , content  = Regular
    }
  , File
    { path     = "/foo/bar/how-to-feed-cat.pdf"
    , atime    = Just [utcIso8601| 1998-11-01 |]
    , mtime    = Just [utcIso8601| 1997-10-01 |]
    , size     = Just 1000
    , mimetype = "application/pdf"
    , content  = Regular
    }
  , File
    { path     = "/foo/bar/fish.txt"
    , atime    = Just [utcIso8601| 2000-11-01 |]
    , mtime    = Just [utcIso8601| 2007-08-01 |]
    , size     = Just 1000
    , mimetype = "tetx/plain"
    , content  = Regular
    }
  , File
    { path     = "/foo/bar/silly-cat-pictures"
    , atime    = Just [utcIso8601| 1998-10-31 |]
    , mtime    = Just [utcIso8601| 1997-11-01 |]
    , size     = Just 1000
    , mimetype = ""
    , content  = Dir
    }
  ]


previewCSS :: Text
previewCSS =
  [iii|
    body {
      margin: 0;
      height: 100vh;
      display: flex;
      flex-direction: row;
      font-family: sans-serif;
      }

    li a {
      display: inline;
      width: 100%;
      height: 100%;
      text-decoration: none;
      color: inherit;
      padding: 1em;
    }

    \#preview-side-bar {
      width: 200px;
      background-color: \#222;
      color: white;
      display: flex;
      flex-direction: column;
      padding: 1rem;
      box-sizing: border-box;
      }

    \#preview-side-bar ul { list-style: none; padding: 0; margin: 0; }

    \#preview-side-bar li { padding: 0.5rem 0; cursor: pointer; }

    \#preview-side-bar li:hover { background-color: \#444; }

    \#preview-container {
      flex: 1;
      display: flex;
      justify-content: center;
      align-items: center;
      background-color: \#f0f0f0;
      padding: 1rem;
      box-sizing: border-box;
      }

    \#preview-frame {
      width: 90%;
      height: 100%;
      border: 1px solid \#ccc;
      border-radius: 8px;
      box-shadow: 0 0 10px rgba(0,0,0,0.1);
      background: white;
      }

    \#preview-frame.mobile {
      width: 400px !important;
      height: 100% !important;
      }
  |]
