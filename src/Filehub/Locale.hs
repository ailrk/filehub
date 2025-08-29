{-# LANGUAGE TemplateHaskell #-}
module Filehub.Locale (Locale(..), Phrase(..), phrase) where


import Data.FileEmbed qualified as FileEmbed
import Toml.Codec (TomlCodec, (.=))
import Toml.Codec qualified as Toml
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Either (fromRight)


data Locale = EN | ZH_CN | ZH_TW | ZH_HK | ES | FR | DE | KR | RU | PT | IT
  deriving (Show, Read, Eq)


data Phrase = Phrase
  { target_filesystem        :: Text
  , target_s3                :: Text
  , contextmenu_open         :: Text
  , contextmenu_play         :: Text
  , contextmenu_view         :: Text
  , contextmenu_download     :: Text
  , contextmenu_delete       :: Text
  , contextmenu_delete_local :: Text
  , contextmenu_details      :: Text
  , contextmenu_selected     :: Text
  , contextmenu_copy         :: Text
  , contextmenu_cancel       :: Text
  , search_as_you_type       :: Text
  , control_panel_list       :: Text
  , control_panel_gird       :: Text
  , control_panel_new_folder :: Text
  , control_panel_new_file   :: Text
  , control_panel_upload     :: Text
  , sort_name                :: Text
  , sort_modified            :: Text
  , sort_size                :: Text
  , login_button             :: Text
  , login_or                 :: Text
  , login_username           :: Text
  , login_password           :: Text
  }
  deriving (Show)


localeCodec :: Locale -> TomlCodec Phrase
localeCodec EN    = Toml.table phraseCodec "en"
localeCodec ZH_CN = Toml.table phraseCodec "zh_cn"
localeCodec ZH_TW = Toml.table phraseCodec "zh_tw"
localeCodec ZH_HK = Toml.table phraseCodec "zh_hk"
localeCodec ES    = Toml.table phraseCodec "es"
localeCodec FR    = Toml.table phraseCodec "fr"
localeCodec DE    = Toml.table phraseCodec "de"
localeCodec KR    = Toml.table phraseCodec "kr"
localeCodec RU    = Toml.table phraseCodec "ru"
localeCodec PT    = Toml.table phraseCodec "pt"
localeCodec IT    = Toml.table phraseCodec "it"


phraseEN :: Phrase
phraseEN = fromRight (error "error in en locale") $ Toml.decode (localeCodec EN) config


phrase :: Locale -> Phrase
phrase EN    = phraseEN
phrase ZH_CN = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase ZH_TW = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase ZH_HK = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase ES    = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase FR    = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase DE    = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase KR    = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase RU    = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase PT    = fromRight phraseEN $ Toml.decode (localeCodec EN) config
phrase IT    = fromRight phraseEN $ Toml.decode (localeCodec EN) config


phraseCodec :: TomlCodec Phrase
phraseCodec =
  Phrase
  <$> Toml.text "TARGET_FILESYSTEM"        .= (.target_filesystem)
  <*> Toml.text "TARGET_S3"                .= (.target_s3)
  <*> Toml.text "CONTEXTMENU_OPEN"         .= (.contextmenu_open)
  <*> Toml.text "CONTEXTMENU_PLAY"         .= (.contextmenu_play)
  <*> Toml.text "CONTEXTMENU_VIEW"         .= (.contextmenu_view)
  <*> Toml.text "CONTEXTMENU_DOWNLOAD"     .= (.contextmenu_download)
  <*> Toml.text "CONTEXTMENU_DELETE"       .= (.contextmenu_delete)
  <*> Toml.text "CONTEXTMENU_DELETE_LOCAL" .= (.contextmenu_delete_local)
  <*> Toml.text "CONTEXTMENU_DETAILS"      .= (.contextmenu_details)
  <*> Toml.text "CONTEXTMENU_SELECTED"     .= (.contextmenu_selected)
  <*> Toml.text "CONTEXTMENU_COPY"         .= (.contextmenu_copy)
  <*> Toml.text "CONTEXTMENU_CANCEL"       .= (.contextmenu_cancel)
  <*> Toml.text "SEARCH_AS_YOU_TYPE"       .= (.search_as_you_type)
  <*> Toml.text "CONTROL_PANEL_LIST"       .= (.control_panel_list)
  <*> Toml.text "CONTROL_PANEL_GIRD"       .= (.control_panel_gird)
  <*> Toml.text "CONTROL_PANEL_NEW_FOLDER" .= (.control_panel_new_folder)
  <*> Toml.text "CONTROL_PANEL_NEW_FILE"   .= (.control_panel_new_file)
  <*> Toml.text "CONTROL_PANEL_UPLOAD"     .= (.control_panel_upload)
  <*> Toml.text "SORT_NAME"                .= (.sort_name)
  <*> Toml.text "SORT_MODIFIED"            .= (.sort_modified)
  <*> Toml.text "SORT_SIZE"                .= (.sort_size)
  <*> Toml.text "LOGIN_BUTTON"             .= (.login_button)
  <*> Toml.text "LOGIN_OR"                 .= (.login_or)
  <*> Toml.text "LOGIN_USERNAME"           .= (.login_username)
  <*> Toml.text "LOGIN_PASSWORD"           .= (.login_password)


config :: Text
config = Text.decodeUtf8 $ $(FileEmbed.embedFile "data/locale.toml")
