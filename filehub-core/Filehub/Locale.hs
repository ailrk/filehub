{-# LANGUAGE TemplateHaskell #-}
-- |
-- Maintainer  :  jimmy@ailrk.com
-- Copyright   :  (c) 2025-present Jinyang yao
--
-- This module defines locale-related types and logic. Website copy is
-- translated into multiple languages, with the translations stored in
-- "data/locale.toml". The TOML parser in this module is used solely for
-- decoding that file.
--
-- The `Phrase` type is the main interface for working with translated text.
-- Each UI string has a corresponding field in `Phrase`. Use `phrase` to
-- construct the appropriate `Phrase` value for a given language.
module Filehub.Locale (Locale(..), Phrase(..), phrase) where

import Filehub.Prelude
import Data.Either (fromRight)
import Data.FileEmbed qualified as FileEmbed
import Text.Toml qualified as Toml
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
  , toggle_sidebar           :: Text
  , contextmenu_open         :: Text
  , contextmenu_play         :: Text
  , contextmenu_view         :: Text
  , contextmenu_edit         :: Text
  , contextmenu_rename       :: Text
  , contextmenu_download     :: Text
  , contextmenu_delete       :: Text
  , contextmenu_delete_local :: Text
  , contextmenu_details      :: Text
  , contextmenu_selected     :: Text
  , contextmenu_copy         :: Text
  , contextmenu_cut          :: Text
  , contextmenu_cancel       :: Text
  , search_as_you_type       :: Text
  , control_panel_list       :: Text
  , control_panel_grid       :: Text
  , control_panel_light      :: Text
  , control_panel_dark       :: Text
  , control_panel_new_folder :: Text
  , control_panel_new_file   :: Text
  , control_panel_upload     :: Text
  , control_panel_copy       :: Text
  , control_panel_cut        :: Text
  , control_panel_paste      :: Text
  , control_panel_delete     :: Text
  , control_panel_cancel     :: Text
  , control_panel_logout     :: Text
  , control_panel_scroll2top :: Text
  , control_panel_language   :: Text
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
  , modal_confirm            :: Text
  , placeholder_newfile      :: Text
  , placeholder_newfoler     :: Text
  , placeholder_empty_file   :: Text
  , placeholder_filename     :: Text
  , confirm_logout           :: Text
  , confirm_save_edit        :: Text
  , confirm_delete1          :: Text
  , confirm_delete_local     :: Text
  , confirm_delete_all       :: Text
  , contextmenu_share        :: Text
  , share_link_indefinite    :: Text
  , share_link_singleuse     :: Text
  , share_link_expiry        :: Text
  , share_passcode           :: Text
  , share_create             :: Text
  , control_panel_share_links :: Text
  , history_title            :: Text
  , history_empty            :: Text
  , history_col_time         :: Text
  , history_col_op           :: Text
  , history_col_target       :: Text
  , history_col_details      :: Text
  , history_col_status       :: Text
  , history_ok               :: Text
  , history_fail             :: Text
  , history_items            :: Text
  , history_op_rename        :: Text
  , history_op_new_file      :: Text
  , history_op_new_folder    :: Text
  , history_op_move          :: Text
  , history_op_delete        :: Text
  , history_op_upload        :: Text
  , history_op_copy          :: Text
  , history_op_paste         :: Text
  , history_op_cut           :: Text
  , history_op_share         :: Text
  , history_op_revoke_link   :: Text
  , folder_empty             :: Text
  }
  deriving (Show)


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


localeKey :: Locale -> Text
localeKey EN    = "en"
localeKey ZH_CN = "zh_cn"
localeKey ZH_TW = "zh_tw"
localeKey ZH_HK = "zh_hk"
localeKey JA    = "ja"
localeKey ES    = "es"
localeKey FR    = "fr"
localeKey DE    = "de"
localeKey KO    = "ko"
localeKey RU    = "ru"
localeKey PT    = "pt"
localeKey IT    = "it"


phraseEN :: Phrase
phraseEN = fromRight (error "error in en locale") $
  Toml.parse localeToml >>= Toml.getTable "en" >>= phraseFromTable


phrase :: Locale -> Phrase
phrase locale = fromRight phraseEN $
  Toml.parse localeToml >>= Toml.getTable (localeKey locale) >>= phraseFromTable


phraseFromTable :: Toml.TomlTable -> Either Toml.TomlError Phrase
phraseFromTable tbl = Phrase
  <$> get "TARGET_FILESYSTEM"
  <*> get "TARGET_S3"
  <*> get "TOGGLE_SIDEBAR"
  <*> get "CONTEXTMENU_OPEN"
  <*> get "CONTEXTMENU_PLAY"
  <*> get "CONTEXTMENU_VIEW"
  <*> get "CONTEXTMENU_EDIT"
  <*> get "CONTEXTMENU_RENAME"
  <*> get "CONTEXTMENU_DOWNLOAD"
  <*> get "CONTEXTMENU_DELETE"
  <*> get "CONTEXTMENU_DELETE_LOCAL"
  <*> get "CONTEXTMENU_DETAILS"
  <*> get "CONTEXTMENU_SELECTED"
  <*> get "CONTEXTMENU_COPY"
  <*> get "CONTEXTMENU_CUT"
  <*> get "CONTEXTMENU_CANCEL"
  <*> get "SEARCH_AS_YOU_TYPE"
  <*> get "CONTROL_PANEL_LIST"
  <*> get "CONTROL_PANEL_GIRD"
  <*> get "CONTROL_PANEL_LIGHT"
  <*> get "CONTROL_PANEL_DARK"
  <*> get "CONTROL_PANEL_NEW_FOLDER"
  <*> get "CONTROL_PANEL_NEW_FILE"
  <*> get "CONTROL_PANEL_UPLOAD"
  <*> get "CONTROL_PANEL_COPY"
  <*> get "CONTROL_PANEL_CUT"
  <*> get "CONTROL_PANEL_PASTE"
  <*> get "CONTROL_PANEL_DELETE"
  <*> get "CONTROL_PANEL_CANCEL"
  <*> get "CONTROL_PANEL_LOGOUT"
  <*> get "CONTROL_PANEL_SCROLL2TOP"
  <*> get "CONTROL_PANEL_LANGUAGE"
  <*> get "SORT_NAME"
  <*> get "SORT_MODIFIED"
  <*> get "SORT_SIZE"
  <*> get "LOGIN_BUTTON"
  <*> get "LOGIN_OR"
  <*> get "LOGIN_USERNAME"
  <*> get "LOGIN_PASSWORD"
  <*> get "LOGIN_ERROR"
  <*> get "DETAIL_FILENAME"
  <*> get "DETAIL_MODIFIED"
  <*> get "DETAIL_ACCESSED"
  <*> get "DETAIL_SIZE"
  <*> get "DETAIL_CONTENT_TYPE"
  <*> get "MODAL_DETAIL"
  <*> get "MODAL_FILE"
  <*> get "MODAL_FOLDER"
  <*> get "MODAL_CREATE"
  <*> get "MODAL_EDIT"
  <*> get "MODAL_READONLY"
  <*> get "MODAL_CONFIRM"
  <*> get "PLACEHOLDER_NEWFILE"
  <*> get "PLACEHOLDER_NEWFOLER"
  <*> get "PLACEHOLDER_EMPTY_FILE"
  <*> get "PLACEHOLDER_FILENAME"
  <*> get "CONFIRM_LOGOUT"
  <*> get "CONFIRM_SAVE_EDIT"
  <*> get "CONFIRM_DELETE1"
  <*> get "CONFIRM_DELETE_LOCAL"
  <*> get "CONFIRM_DELETE_ALL"
  <*> get "CONTEXTMENU_SHARE"
  <*> get "SHARE_LINK_INDEFINITE"
  <*> get "SHARE_LINK_SINGLEUSE"
  <*> get "SHARE_LINK_EXPIRY"
  <*> get "SHARE_PASSCODE"
  <*> get "SHARE_CREATE"
  <*> get "CONTROL_PANEL_SHARE_LINKS"
  <*> get "HISTORY_TITLE"
  <*> get "HISTORY_EMPTY"
  <*> get "HISTORY_COL_TIME"
  <*> get "HISTORY_COL_OP"
  <*> get "HISTORY_COL_TARGET"
  <*> get "HISTORY_COL_DETAILS"
  <*> get "HISTORY_COL_STATUS"
  <*> get "HISTORY_OK"
  <*> get "HISTORY_FAIL"
  <*> get "HISTORY_ITEMS"
  <*> get "HISTORY_OP_RENAME"
  <*> get "HISTORY_OP_NEW_FILE"
  <*> get "HISTORY_OP_NEW_FOLDER"
  <*> get "HISTORY_OP_MOVE"
  <*> get "HISTORY_OP_DELETE"
  <*> get "HISTORY_OP_UPLOAD"
  <*> get "HISTORY_OP_COPY"
  <*> get "HISTORY_OP_PASTE"
  <*> get "HISTORY_OP_CUT"
  <*> get "HISTORY_OP_SHARE"
  <*> get "HISTORY_OP_REVOKE_LINK"
  <*> get "FOLDER_EMPTY"
  where
    get k = Toml.getText k tbl


localeToml :: Text
localeToml = txt_decode $(FileEmbed.embedFile "data/locale.toml")
