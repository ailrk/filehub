{-# LANGUAGE TemplateHaskell #-}
module Filehub.Locale (Locale(..), Phrase(..), phrase) where


import Data.Either (fromRight)
import Data.FileEmbed qualified as FileEmbed
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Lens.Micro.Platform ()
import Toml.Codec (TomlCodec, (.=))
import Toml.Codec qualified as Toml
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))


data Locale = EN | ZH_CN | ZH_TW | ZH_HK | JA | ES | FR | DE | KO | RU | PT | IT
  deriving Eq


instance Show Locale where
  show = \case
    EN    -> "en"
    ZH_CN -> "zh_cn"
    ZH_TW -> "zh_tw"
    ZH_HK -> "zh_hk"
    JA    -> "ja"
    ES    -> "es"
    FR    -> "fr"
    DE    -> "de"
    KO    -> "ko"
    RU    -> "ru"
    PT    -> "pt"
    IT    -> "it"


instance Read Locale where
  readsPrec _ s = do
    let locale =
          case s of
          "en"    -> EN
          "zh_cn" -> ZH_CN
          "zh_tw" -> ZH_TW
          "zh_hk" -> ZH_HK
          "ja"    -> JA
          "es"    -> ES
          "fr"    -> FR
          "de"    -> DE
          "ko"    -> KO
          "ru"    -> RU
          "pt"    -> PT
          "it"    -> IT
          _       -> EN
    pure (locale, "")


data Phrase = Phrase
  { target_filesystem        :: Text
  , target_s3                :: Text
  , contextmenu_open         :: Text
  , contextmenu_play         :: Text
  , contextmenu_view         :: Text
  , contextmenu_edit         :: Text
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
  , control_panel_copy       :: Text
  , control_panel_paste      :: Text
  , control_panel_delete     :: Text
  , control_panel_cancel     :: Text
  , sort_name                :: Text
  , sort_modified            :: Text
  , sort_size                :: Text
  , login_button             :: Text
  , login_or                 :: Text
  , login_username           :: Text
  , login_password           :: Text
  , login_error              :: Text
  , detail_filename          :: Text
  , detail_modified          :: Text
  , detail_accessed          :: Text
  , detail_size              :: Text
  , detail_content_type      :: Text
  , modal_detail             :: Text
  , modal_file               :: Text
  , modal_folder             :: Text
  , modal_create             :: Text
  , modal_edit               :: Text
  , modal_readonly           :: Text
  , placeholder_newfile      :: Text
  , placeholder_newfoler     :: Text
  , placeholder_empty_file   :: Text
  , placeholder_filename     :: Text
  , confirm_logout           :: Text
  , confirm_save_edit        :: Text
  , confirm_delete1          :: Text
  , confirm_delete_local     :: Text
  , confirm_delete_all       :: Text
  }
  deriving (Show)


localeCodec :: Locale -> TomlCodec Phrase
localeCodec EN    = Toml.table phraseCodec "en"
localeCodec ZH_CN = Toml.table phraseCodec "zh_cn"
localeCodec ZH_TW = Toml.table phraseCodec "zh_tw"
localeCodec ZH_HK = Toml.table phraseCodec "zh_hk"
localeCodec JA    = Toml.table phraseCodec "ja"
localeCodec ES    = Toml.table phraseCodec "es"
localeCodec FR    = Toml.table phraseCodec "fr"
localeCodec DE    = Toml.table phraseCodec "de"
localeCodec KO    = Toml.table phraseCodec "ko"
localeCodec RU    = Toml.table phraseCodec "ru"
localeCodec PT    = Toml.table phraseCodec "pt"
localeCodec IT    = Toml.table phraseCodec "it"


instance ToHttpApiData Locale where
  toUrlPiece EN    = "en"
  toUrlPiece ZH_CN = "zh_cn"
  toUrlPiece ZH_TW = "zh_tw"
  toUrlPiece ZH_HK = "zh_hk"
  toUrlPiece JA    = "ja"
  toUrlPiece ES    = "es"
  toUrlPiece FR    = "fr"
  toUrlPiece DE    = "de"
  toUrlPiece KO    = "ko"
  toUrlPiece RU    = "ru"
  toUrlPiece PT    = "pt"
  toUrlPiece IT    = "it"


instance FromHttpApiData Locale where
  parseUrlPiece "en"     = pure EN
  parseUrlPiece "zh_cn"  = pure ZH_CN
  parseUrlPiece "zh_tw"  = pure ZH_TW
  parseUrlPiece "zh_hk"  = pure ZH_HK
  parseUrlPiece "ja"     = pure JA
  parseUrlPiece "es"     = pure ES
  parseUrlPiece "fr"     = pure FR
  parseUrlPiece "de"     = pure DE
  parseUrlPiece "ko"     = pure KO
  parseUrlPiece "ru"     = pure RU
  parseUrlPiece "pt"     = pure PT
  parseUrlPiece "it"     = pure IT
  parseUrlPiece _        = Left "unknown locale"


phraseEN :: Phrase
phraseEN = fromRight (error "error in en locale") $ Toml.decode (localeCodec EN) config


phrase :: Locale -> Phrase
phrase EN    = phraseEN
phrase ZH_CN = fromRight phraseEN $ Toml.decode (localeCodec ZH_CN) config
phrase ZH_TW = fromRight phraseEN $ Toml.decode (localeCodec ZH_TW) config
phrase ZH_HK = fromRight phraseEN $ Toml.decode (localeCodec ZH_HK) config
phrase JA    = fromRight phraseEN $ Toml.decode (localeCodec JA)    config
phrase ES    = fromRight phraseEN $ Toml.decode (localeCodec ES)    config
phrase FR    = fromRight phraseEN $ Toml.decode (localeCodec FR)    config
phrase DE    = fromRight phraseEN $ Toml.decode (localeCodec DE)    config
phrase KO    = fromRight phraseEN $ Toml.decode (localeCodec KO)    config
phrase RU    = fromRight phraseEN $ Toml.decode (localeCodec RU)    config
phrase PT    = fromRight phraseEN $ Toml.decode (localeCodec PT)    config
phrase IT    = fromRight phraseEN $ Toml.decode (localeCodec IT)    config


phraseCodec :: TomlCodec Phrase
phraseCodec =
  Phrase
  <$> Toml.text "TARGET_FILESYSTEM"        .= (.target_filesystem)
  <*> Toml.text "TARGET_S3"                .= (.target_s3)
  <*> Toml.text "CONTEXTMENU_OPEN"         .= (.contextmenu_open)
  <*> Toml.text "CONTEXTMENU_PLAY"         .= (.contextmenu_play)
  <*> Toml.text "CONTEXTMENU_VIEW"         .= (.contextmenu_view)
  <*> Toml.text "CONTEXTMENU_EDIT"         .= (.contextmenu_edit)
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
  <*> Toml.text "CONTROL_PANEL_COPY"       .= (.control_panel_copy)
  <*> Toml.text "CONTROL_PANEL_PASTE"      .= (.control_panel_paste)
  <*> Toml.text "CONTROL_PANEL_DELETE"     .= (.control_panel_delete)
  <*> Toml.text "CONTROL_PANEL_CANCEL"     .= (.control_panel_cancel)
  <*> Toml.text "SORT_NAME"                .= (.sort_name)
  <*> Toml.text "SORT_MODIFIED"            .= (.sort_modified)
  <*> Toml.text "SORT_SIZE"                .= (.sort_size)
  <*> Toml.text "LOGIN_BUTTON"             .= (.login_button)
  <*> Toml.text "LOGIN_OR"                 .= (.login_or)
  <*> Toml.text "LOGIN_USERNAME"           .= (.login_username)
  <*> Toml.text "LOGIN_PASSWORD"           .= (.login_password)
  <*> Toml.text "LOGIN_ERROR"              .= (.login_error)
  <*> Toml.text "DETAIL_FILENAME"          .= (.detail_filename)
  <*> Toml.text "DETAIL_MODIFIED"          .= (.detail_modified)
  <*> Toml.text "DETAIL_ACCESSED"          .= (.detail_accessed)
  <*> Toml.text "DETAIL_SIZE"              .= (.detail_size)
  <*> Toml.text "DETAIL_CONTENT_TYPE"      .= (.detail_content_type)
  <*> Toml.text "MODAL_DETAIL"             .= (.modal_detail)
  <*> Toml.text "MODAL_FILE"               .= (.modal_file)
  <*> Toml.text "MODAL_FOLDER"             .= (.modal_folder)
  <*> Toml.text "MODAL_CREATE"             .= (.modal_create)
  <*> Toml.text "MODAL_EDIT"               .= (.modal_edit)
  <*> Toml.text "MODAL_READONLY"           .= (.modal_readonly)
  <*> Toml.text "PLACEHOLDER_NEWFILE"      .= (.placeholder_newfile)
  <*> Toml.text "PLACEHOLDER_NEWFOLER"     .= (.placeholder_newfoler)
  <*> Toml.text "PLACEHOLDER_EMPTY_FILE"   .= (.placeholder_empty_file)
  <*> Toml.text "PLACEHOLDER_FILENAME"     .= (.placeholder_filename)
  <*> Toml.text "CONFIRM_LOGOUT"           .= (.confirm_logout)
  <*> Toml.text "CONFIRM_SAVE_EDIT"        .= (.confirm_save_edit)
  <*> Toml.text "CONFIRM_DELETE1"          .= (.confirm_delete1)
  <*> Toml.text "CONFIRM_DELETE_LOCAL"     .= (.confirm_delete_local)
  <*> Toml.text "CONFIRM_DELETE_ALL"       .= (.confirm_delete_all)


config :: Text
config = Text.decodeUtf8 $ $(FileEmbed.embedFile "data/locale.toml")
