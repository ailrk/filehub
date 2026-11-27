module Filehub.Template.Settings
  ( settingsModal
  , pluginsPanel
  , pluginsPanelWithNewForm
  , pluginItem
  , pluginItemError
  , newPluginButton
  , newPluginForm
  , settingsAppearanceSection
  , settingsTargetsSection
  ) where

import Filehub.Prelude
import Filehub.Locale (Locale(..))
import Filehub.Plugin.Types
import Filehub.Plugin.Toml qualified as Plugin.Toml
import Filehub.Theme (Theme(..))
import Lucid
import Target.File (FileSys, Target(..))
import Target.S3 (S3, Target(..))
import Target.Types (AnyTarget, TargetId, handleTarget, targetHandler)
import Data.ClientPath (Root(..), AbsPath(..))
import System.FilePath (takeFileName)
import Web.HttpApiData (toUrlPiece)


------------------------------------
-- URL helpers
------------------------------------


settingsExportUrl :: Text
settingsExportUrl = "/settings/export"

settingsAddPluginUrl :: Text
settingsAddPluginUrl = "/settings/plugins"

settingsGetNewPluginUrl :: Text
settingsGetNewPluginUrl = "/settings/plugins/new"

settingsCancelNewPluginUrl :: Text
settingsCancelNewPluginUrl = "/settings/plugins/cancel"

settingsUpdatePluginUrl :: Text -> Text
settingsUpdatePluginUrl name = "/settings/plugins/" <> name

settingsDeletePluginUrl :: Text -> Text
settingsDeletePluginUrl name = "/settings/plugins/" <> name

settingsAddTargetUrl :: Text
settingsAddTargetUrl = "/settings/targets"

settingsDeleteTargetUrl :: TargetId -> Text
settingsDeleteTargetUrl tid = "/settings/targets/" <> toUrlPiece tid

settingsUpdateAppearanceUrl :: Text
settingsUpdateAppearanceUrl = "/settings/appearance"


------------------------------------
-- IDs
------------------------------------


pluginsPanelId :: Text
pluginsPanelId = "settings-plugins"

newPluginAreaId :: Text
newPluginAreaId = "plugin-new-area"

pluginItemId :: Text -> Text
pluginItemId name = "plugin-" <> txt_replace " " "-" (txt_toLower name)

targetsListId :: Text
targetsListId = "settings-targets"

appearanceSectionId :: Text
appearanceSectionId = "settings-appearance"


------------------------------------
-- Full settings modal
------------------------------------


settingsModal
  :: [StaticPlugin]
  -> Set Text
  -> [(TargetId, AnyTarget)]
  -> Set TargetId
  -> Theme
  -> Locale
  -> Html ()
settingsModal allPlugins dynNames allTargets staticTids theme locale =
  div_ [class_ "modal settings-modal"] do
    div_ [class_ "modal-underlay"] mempty
    div_ [class_ "modal-content settings-content"] do
      div_ [class_ "settings-header"] do
        span_ [class_ "settings-title"] "Settings"
        div_ [class_ "btn-modal-close", term "data-close-modal" "true"] do
          i_ [class_ "bx bx-x"] mempty
      div_ [class_ "settings-body"] do
        section_ [class_ "settings-section"] do
          h3_ [class_ "settings-section-title"] do
            i_ [class_ "bx bx-plug"] mempty; " Plugins"
          pluginsPanel allPlugins dynNames

        hr_ [class_ "settings-divider"]

        section_ [class_ "settings-section"] do
          h3_ [class_ "settings-section-title"] do
            i_ [class_ "bx bx-data"] mempty; " Targets"
          settingsTargetsSection allTargets staticTids
          addTargetForm

        hr_ [class_ "settings-divider"]

        section_ [class_ "settings-section"] do
          h3_ [class_ "settings-section-title"] do
            i_ [class_ "bx bx-palette"] mempty; " Appearance"
          settingsAppearanceSection theme locale

        hr_ [class_ "settings-divider"]

        section_ [class_ "settings-section"] do
          h3_ [class_ "settings-section-title"] do
            i_ [class_ "bx bx-export"] mempty; " Export"
          p_ [class_ "settings-hint"]
            "Download the current dynamic settings as a TOML file."
          a_ [ href_ settingsExportUrl
             , class_ "btn btn-secondary"
             , term "download" "dynamic.toml"
             ] do
            i_ [class_ "bx bx-download"] mempty; " dynamic.toml"


------------------------------------
-- Plugins section
------------------------------------


pluginsPanel :: [StaticPlugin] -> Set Text -> Html ()
pluginsPanel ps dynNames =
  div_ [id_ pluginsPanelId, class_ "plugin-panel"] do
    traverse_ (pluginRow dynNames) ps
    newPluginButton


pluginsPanelWithNewForm :: [StaticPlugin] -> Set Text -> Maybe Text -> Text -> Html ()
pluginsPanelWithNewForm ps dynNames mErr tomlContent =
  div_ [id_ pluginsPanelId, class_ "plugin-panel"] do
    traverse_ (pluginRow dynNames) ps
    newPluginForm mErr tomlContent


pluginRow :: Set Text -> StaticPlugin -> Html ()
pluginRow dynNames p
  | set_member p.name dynNames = pluginItem p
  | otherwise                  = pluginItemStatic p


pluginItemStatic :: StaticPlugin -> Html ()
pluginItemStatic p =
  div_ [class_ "plugin-item plugin-item-static"] do
    div_ [class_ "plugin-item-header"] do
      span_ [class_ "plugin-item-toggle"] $
        i_ [class_ "bx bx-lock-alt"] mempty
      span_ [class_ "plugin-item-name"] (toHtml p.name)


pluginItem :: StaticPlugin -> Html ()
pluginItem p = pluginItemRaw False Nothing p.name (Plugin.Toml.serializePlugins [p])


pluginItemError :: Text -> Text -> Text -> Html ()
pluginItemError name rawToml err = pluginItemRaw True (Just err) name rawToml


pluginItemRaw :: Bool -> Maybe Text -> Text -> Text -> Html ()
pluginItemRaw open mErr name tomlContent =
  div_ [id_ (pluginItemId name), class_ cls] do
    div_ [class_ "plugin-item-header"] do
      span_ [class_ "plugin-item-toggle"] $
        i_ [class_ chevron] mempty
      span_ [class_ "plugin-item-name"] (toHtml name)
      button_ [ class_ "btn btn-icon btn-urgent btn-xs"
              , term "hx-delete" (settingsDeletePluginUrl name)
              , term "hx-target" ("#" <> pluginsPanelId)
              , term "hx-swap" "outerHTML"
              , term "hx-confirm" ("Delete plugin \"" <> name <> "\"?")
              ] (i_ [class_ "bx bx-trash"] mempty)
    div_ [class_ "plugin-item-body"] do
      form_ [ term "hx-put" (settingsUpdatePluginUrl name)
            , term "hx-target" ("#" <> pluginItemId name)
            , term "hx-swap" "outerHTML"
            ] do
        tomlEditorWidget tomlContent
        div_ [class_ "plugin-editor-footer"] do
          case mErr of
            Nothing  -> mempty
            Just err -> div_ [class_ "plugin-editor-error"] do
              i_ [class_ "bx bx-error-circle"] mempty; " "; toHtml err
          button_ [type_ "submit", class_ "btn btn-accent btn-sm"] do
            i_ [class_ "bx bx-save"] mempty; " Save"
  where
    cls    = if open then "plugin-item expanded" else "plugin-item"
    chevron = if open then "bx bx-chevron-down"  else "bx bx-chevron-right"


newPluginButton :: Html ()
newPluginButton =
  div_ [id_ newPluginAreaId] do
    button_ [ class_ "btn btn-accent"
            , term "hx-get" settingsGetNewPluginUrl
            , term "hx-target" ("#" <> newPluginAreaId)
            , term "hx-swap" "outerHTML"
            ] do
      i_ [class_ "bx bx-plus"] mempty; " Add Plugin"


newPluginForm :: Maybe Text -> Text -> Html ()
newPluginForm mErr tomlContent =
  div_ [id_ newPluginAreaId] do
    form_ [ term "hx-post" settingsAddPluginUrl
          , term "hx-target" ("#" <> pluginsPanelId)
          , term "hx-swap" "outerHTML"
          ] do
      tomlEditorWidget tomlContent
      div_ [class_ "plugin-editor-footer"] do
        case mErr of
          Nothing  -> mempty
          Just err -> div_ [class_ "plugin-editor-error"] do
            i_ [class_ "bx bx-error-circle"] mempty; " "; toHtml err
        button_ [ type_ "button", class_ "btn btn-sm"
                , term "hx-get" settingsCancelNewPluginUrl
                , term "hx-target" ("#" <> pluginsPanelId)
                , term "hx-swap" "outerHTML"
                ] "Cancel"
        button_ [type_ "submit", class_ "btn btn-accent btn-sm"] do
          i_ [class_ "bx bx-save"] mempty; " Save"


tomlEditorWidget :: Text -> Html ()
tomlEditorWidget content =
  div_ [class_ "toml-editor-wrapper"] do
    pre_ [class_ "toml-highlight", term "aria-hidden" "true"] mempty
    textarea_ [name_ "pluginsToml", class_ "toml-textarea", spellcheck_ "false"] (toHtml content)


------------------------------------
-- Targets section
------------------------------------


settingsTargetsSection :: [(TargetId, AnyTarget)] -> Set TargetId -> Html ()
settingsTargetsSection allTargets staticTids =
  div_ [id_ targetsListId, class_ "settings-targets-list"] do
    traverse_ (targetRow staticTids) allTargets


targetRow :: Set TargetId -> (TargetId, AnyTarget) -> Html ()
targetRow staticTids (tid, t) =
  div_ [class_ "target-row"] do
    div_ [class_ "target-row-info"] do
      icon
      span_ [class_ "target-row-name"] label
    if set_member tid staticTids
      then i_ [class_ "bx bx-lock-alt"] mempty
      else button_ [ class_ "btn btn-icon btn-urgent"
                   , term "hx-delete" (settingsDeleteTargetUrl tid)
                   , term "hx-target" ("#" <> targetsListId)
                   , term "hx-swap" "outerHTML"
                   , term "hx-confirm" "Remove this target?"
                   ] (i_ [class_ "bx bx-trash"] mempty)
  where
    icon = fromMaybe (i_ [class_ "bx bx-folder"] mempty) $
      handleTarget t
        [ targetHandler @S3 \_ ->
            i_ [class_ "bx bxs-cube"] mempty
        , targetHandler @FileSys \_ ->
            i_ [class_ "bx bx-folder"] mempty
        ]
    label = fromMaybe "" $
      handleTarget t
        [ targetHandler @S3 \(S3Backend { bucket = b }) ->
            toHtml ("s3: " <> b)
        , targetHandler @FileSys \(FileBackend { root = Root (AbsPath p) }) ->
            let display = if set_member tid staticTids then takeFileName p else p
            in toHtml ("fs: " <> txt_pack display)
        ]


addTargetForm :: Html ()
addTargetForm =
  form_ [ class_ "add-target-form"
        , term "hx-post" settingsAddTargetUrl
        , term "hx-target" ("#" <> targetsListId)
        , term "hx-swap" "outerHTML"
        ] do
    select_ [name_ "type", class_ "form-control form-select"] do
      option_ [value_ "fs"] "Filesystem"
      option_ [value_ "s3"] "S3 Bucket"
    input_ [ class_ "form-control", name_ "path"
           , placeholder_ "/path/to/dir  or  bucket-name", required_ ""]
    button_ [type_ "submit", class_ "btn btn-accent"] do
      i_ [class_ "bx bx-plus"] mempty; " Add Target"


------------------------------------
-- Appearance section
------------------------------------


settingsAppearanceSection :: Theme -> Locale -> Html ()
settingsAppearanceSection theme locale =
  div_ [id_ appearanceSectionId, class_ "settings-appearance"] do
    form_ [ term "hx-post" settingsUpdateAppearanceUrl
          , term "hx-target" ("#" <> appearanceSectionId)
          , term "hx-swap" "outerHTML"
          ] do
      div_ [class_ "appearance-row"] do
        label_ [class_ "appearance-label"] "Default Theme"
        div_ [class_ "appearance-options"] do
          label_ [class_ "radio-label"] do
            input_ $ [type_ "radio", name_ "theme", value_ "dark"]
                  <> [checked_ | theme == Dark]
            " Dark"
          label_ [class_ "radio-label"] do
            input_ $ [type_ "radio", name_ "theme", value_ "light"]
                  <> [checked_ | theme == Light]
            " Light"
      div_ [class_ "appearance-row"] do
        label_ [class_ "appearance-label"] "Default Language"
        select_ [name_ "locale", class_ "form-control form-select"] do
          opt EN    "English"
          opt ZH_CN "简体中文"
          opt ZH_TW "繁體中文 (TW)"
          opt ZH_HK "繁體中文 (HK)"
          opt JA    "日本語"
          opt ES    "Español"
          opt FR    "Français"
          opt DE    "Deutsch"
          opt KO    "한국어"
          opt RU    "Русский"
          opt PT    "Português"
          opt IT    "Italiano"
      button_ [type_ "submit", class_ "btn btn-accent"] "Apply"
  where
    opt l lbl =
      option_ ([value_ (localeVal l)] <> [selected_ "selected" | l == locale]) lbl
    localeVal EN    = "en"
    localeVal ZH_CN = "zh_cn"
    localeVal ZH_TW = "zh_tw"
    localeVal ZH_HK = "zh_hk"
    localeVal JA    = "ja"
    localeVal ES    = "es"
    localeVal FR    = "fr"
    localeVal DE    = "de"
    localeVal KO    = "ko"
    localeVal RU    = "ru"
    localeVal PT    = "pt"
    localeVal IT    = "it"
