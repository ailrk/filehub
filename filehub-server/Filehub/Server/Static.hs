{-# LANGUAGE TemplateHaskell #-}

module Filehub.Server.Static where

import Filehub.Prelude
import Control.Monad.Reader (asks)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (object, KeyValue (..), Value)
import Data.ByteString.Base64 qualified as Base64
import Data.List qualified as L
import Filehub.Env (Env(..))
import Filehub.Error ( FilehubError(..) )
import Filehub.Monad
import Filehub.Orphan ()
import Filehub.Server.Static.QQ (staticFiles)
import Filehub.Session (Session(..), SessionId(..), withSession)
import Filehub.Theme qualified as Theme
import Filehub.Types (Theme(..))
import Network.Mime qualified as Mime
import Prelude hiding (init, readFile)
import Servant (Header , Headers  , addHeader    , err404     , errBody)


themeCss :: SessionId -> Filehub ByteString
themeCss sessionId = do
  customThemeDark  <- (fmap . fmap) Theme.customTheme2Css (asks (.customThemeDark))
  customThemeLight <- (fmap . fmap) Theme.customTheme2Css (asks (.customThemeLight))
  withSession sessionId \s -> pure
    case s.theme of
      Dark  -> fromMaybe "no-theme" $ customThemeDark <|> map_lookup "theme-dark.css" staticFiles
      Light -> fromMaybe "no-theme" $ customThemeLight <|> map_lookup "theme-light.css" staticFiles


-- The production implementation uses static files embeded in the executable, while the
-- debug implementation uses path so you can hot reload frontend code.
--
-- == Content-Type header is used to specify the content type. Note it's hard to set the
-- content type dynamically with servant because the type makes an assumption of the content
-- type. This handler simply adds the correct Content-Type, and the `dedupHeadersKeepLast`
-- middleware will strip the default servant header.
--
-- == Cache-Control header is required to make sure static files are properly cached.
static :: [FilePath] -> Filehub (Headers '[ Header "Content-Type" String
                                          , Header "Cache-Control" String
                                          , Header "ETag" String ] ByteString)
static paths = do
  let path = L.intercalate "/" paths
  content <- case map_lookup path staticFiles of
    Just c -> pure c
    Nothing -> throwIO do HTTPError (err404 { errBody = [i|File doesn't exist|]})

  let mimetype = Mime.defaultMimeLookup (txt_pack path)
  let etag    = "\"" <> bs_unpackChars (Base64.encode (SHA256.hash content)) <> "\""
  pure
    . addHeader (bs_unpackChars mimetype)
    . addHeader "public, no-cache"
    . addHeader etag
    $ content


-- It's for PWA. More on https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Manifest
manifest :: Filehub Value
manifest = do
  let t = txt_pack
  pure $
    object
      [ "name"       .= t "FileHub"
      , "short_name" .= t "FileHub"
      , "start_url"  .= t "/"
      , "display"    .= t "standalone"
      , "icons" .=
          [ object
                [ "src"     .= t "/static/web-app-manifest-192x192.png"
                , "sizes"   .= t "192x192"
                , "type"    .= t "image/png"
                , "purpose" .= t "any"
                ]
          , object
                [ "src"     .= t "/static/web-app-manifest-512x512.png"
                , "sizes"   .= t "512x512"
                , "type"    .= t "image/png"
                , "purpose" .= t "maskable"
                ]
          ]
      ]
