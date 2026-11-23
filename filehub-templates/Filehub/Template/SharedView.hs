{-# LANGUAGE NamedFieldPuns #-}
module Filehub.Template.SharedView
  ( sharedPage
  , sharedPreviewPage
  , sharedAuthPage
  , shareCreatedModal
  , shareLinksModal
  , linkRow
  , linkEditModal
  ) where

import Control.Monad (when)
import Data.ClientPath (ClientPath(..), AbsPath(..))
import Data.ClientPath qualified as ClientPath
import Data.Coerce (coerce)
import Data.File (File(..), FileType(..), FileInfo, IsLink(..))
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (iii)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import Filehub.Session (targetRoot)
import Filehub.SharedLink (SharedLink(..), SharedLinkHash(..), SharedLinkType(..))
import Filehub.Template.Common (icon, sizeElement, modifiedDateElement, withDefault)
import Filehub.Types (Display(..))
import Lucid
import Network.Mime.Extended (isMime)
import System.FilePath (takeFileName)
import Target.Dummy (DummyTarget)
import Target.File (FileSys)
import Target.S3 (S3)
import Target.Types (targetHandler, handleTarget, AnyTarget)


-- | Full-page shared link viewer.
-- `mCurrentPath` is the directory currently being browsed (relative to shared root).
sharedPage :: SharedLink -> AnyTarget -> [FileInfo] -> Maybe ClientPath -> Html ()
sharedPage link target files mCurrentPath = do
  withDefault Desktop "#000000" do
    div_ [ id_ "shared-index" ] do
      header
      div_ [ id_ "shared-view" ] do
        table_ [ id_ "shared-table", class_ "list-view" ] do
          colgroup_ do
            col_ [ style_ "width:60%" ]
            col_ [ style_ "width:20%" ]
            col_ [ style_ "width:20%" ]
          thead_ do
            tr_ do
              th_ "Name"
              th_ "Size"
              th_ "Modified"
          tbody_ do
            case mCurrentPath of
              Just cp | cp /= sharedRootPath ->
                tr_ [ class_ "table-item dir"
                    , onclick_ [iii|window.location.href='/s/#{hashText}?file=#{parentPath cp}'|]
                    ] do
                  td_ do i_ [ class_ "bx bxs-folder" ] mempty >> span_ " .."
                  td_ mempty
                  td_ mempty
              _ -> mempty
            mapM_ (fileRow link target) files
  where
    SharedLinkHash hashText = link.hash
    sharedRootPath = ClientPath.toClientPath (targetRoot target) link.file.path
    header = div_ [ id_ "shared-header" ] do
      span_ [ class_ "field shared-title" ] do
        i_ [ class_ "bx bxs-share-alt" ] mempty
        span_ [ class_ "shared-link-name" ] do
          toHtml $ (T.pack . takeFileName . coerce) link.file.path
      when link.readonly do
        span_ [ class_ "shared-readonly-badge" ] "read-only"
    parentPath (ClientPath cp) =
      T.intercalate "/" . init . T.splitOn "/" . T.pack $ cp


fileRow :: SharedLink -> AnyTarget -> FileInfo -> Html ()
fileRow link target file = do
  tr_ (rowAttrs file) do
    td_ do
      span_ [ class_ "field" ] do
        icon file
        span_ (toHtml displayName)
          `with` [ title_ (T.pack displayName) ]
      case file.isLink of
        BrokenLink -> mempty
        _ -> case file.content of
          Regular ->
            a_ [ class_ "shared-download-btn"
               , href_ downloadUrl
               , term "download" ""
               , onclick_ "event.stopPropagation()"
               ] (i_ [ class_ "bx bx-download" ] mempty)
          Dir -> mempty
    td_ (sizeElement file)
    td_ (modifiedDateElement file)
  where
    SharedLinkHash hashText = link.hash
    fileClientPath = ClientPath.toClientPath (targetRoot target) file.path
    ClientPath fileCP = fileClientPath
    downloadUrl = [iii|/s/#{hashText}/file?file=#{fileCP}|]
    displayName = fromMaybe "-" $ handleTarget target
      [ targetHandler @S3      \_ -> coerce file.path
      , targetHandler @FileSys \_ -> coerce takeFileName file.path
      , targetHandler @DummyTarget \_ -> coerce takeFileName file.path
      ]
    rowAttrs f =
      let clientPath = ClientPath.toClientPath (targetRoot target) f.path
          ClientPath cp = clientPath
       in case f.isLink of
            BrokenLink -> [ class_ "table-item broken-symlink" ]
            _ -> case f.content of
              Dir     ->
                [ class_ "table-item dir"
                , onclick_ [iii|window.location.href='/s/#{hashText}?file=#{cp}'|]
                ]
              Regular ->
                [ class_ "table-item"
                , onclick_ [iii|window.location.href='/s/#{hashText}/preview?file=#{cp}'|]
                ]


-- | Preview page for a single file in a shared link.
-- `mTextContent` carries the file text (server-read) for text files; Nothing for binary.
sharedPreviewPage :: SharedLink -> AnyTarget -> FileInfo -> ClientPath -> Maybe Text -> Html ()
sharedPreviewPage link target file clientPath mTextContent = do
  withDefault Desktop "#000000" do
    div_ [ id_ "shared-preview-index" ] do
      previewHeader
      div_ [ id_ "shared-preview-content" ] previewContent
  where
    SharedLinkHash hashText = link.hash
    ClientPath cp = clientPath
    fileUrl = [iii|/s/#{hashText}/file?file=#{cp}|] :: Text
    inlineUrl = [iii|/s/#{hashText}/file?inline&file=#{cp}|] :: Text
    backUrl  = [iii|/s/#{hashText}|] :: Text
    displayName = fromMaybe "-" $ handleTarget target
      [ targetHandler @S3          \_ -> coerce file.path
      , targetHandler @FileSys     \_ -> coerce takeFileName file.path
      , targetHandler @DummyTarget \_ -> coerce takeFileName file.path
      ]
    previewHeader = div_ [ id_ "shared-header" ] do
      a_ [ href_ backUrl, class_ "btn btn-control" ] do
        i_ [ class_ "bx bx-arrow-back" ] mempty
      span_ [ class_ "field shared-title" ] do
        icon file
        span_ [ class_ "shared-link-name" ] (toHtml displayName)
      a_ [ href_ fileUrl, class_ "btn btn-control", term "download" "" ] do
        i_ [ class_ "bx bx-download" ] mempty
        " Download"
    previewContent
      | file.mimetype `isMime` "image" =
          img_ [ src_ fileUrl, class_ "shared-preview-img", alt_ (T.pack displayName) ]
      | file.mimetype `isMime` "video" =
          video_ [ src_ fileUrl, controls_ "", class_ "shared-preview-video" ] mempty
      | file.mimetype `isMime` "audio" =
          audio_ [ src_ fileUrl, controls_ "", class_ "shared-preview-audio" ] mempty
      | file.mimetype `isMime` "application/pdf" =
          iframe_ [ src_ inlineUrl, class_ "shared-preview-iframe" ] mempty
      | Just text <- mTextContent =
          pre_ [ class_ "shared-preview-text" ] (toHtml text)
      | otherwise =
          div_ [ class_ "shared-preview-unsupported" ] do
            i_ [ class_ "bx bxs-file-blank" ] mempty
            p_ "No preview available."
            a_ [ href_ fileUrl, class_ "btn btn-control", term "download" "" ] "Download"


-- | Passcode authentication page for a shared link.
sharedAuthPage :: SharedLinkHash -> Bool -> Html ()
sharedAuthPage (SharedLinkHash hashText) hasError = do
  withDefault Desktop "#000000" do
    div_ [ class_ "shared-auth-container" ] do
      div_ [ class_ "shared-auth-card" ] do
        div_ [ class_ "shared-auth-title" ] do
          i_ [ class_ "bx bxs-lock-alt" ] mempty
          span_ " Protected link"
        when hasError do
          div_ [ class_ "shared-auth-error" ] "Incorrect passcode."
        form_ [ action_ "/s/auth", method_ "post" ] do
          input_ [ type_ "hidden", name_ "hash", value_ hashText ]
          div_ [ class_ "shared-auth-field" ] do
            input_ [ class_ "form-control"
                   , type_ "password"
                   , name_ "passcode"
                   , placeholder_ "Enter passcode"
                   , term "autofocus" ""
                   ]
          button_ [ class_ "btn btn-control shared-auth-submit", type_ "submit" ] "Access"


-- | Modal shown after a shared link is created — displays the URL to copy.
shareCreatedModal :: SharedLinkHash -> Html ()
shareCreatedModal (SharedLinkHash hashText) = do
  div_ [ id_ "share-created-modal", class_ "modal" ] do
    div_ [ class_ "modal-underlay", onclick_ "this.closest('.modal').remove()" ] mempty
    div_ [ class_ "modal-content", style_ "width:480px" ] do
      div_ [ class_ "modal-title-bar" ] do
        span_ "Share link created"
        div_ [ class_ "modal-title-bar-control" ] do
          span_ [ class_ "btn-modal-close"
                , onclick_ "this.closest('.modal').remove()"
                ] do
            i_ [ class_ "bx bx-x" ] mempty
      div_ [ style_ "padding:16px; display:flex; flex-direction:column; gap:12px" ] do
        div_ [ style_ "display:flex; gap:8px; align-items:center" ] do
          input_ [ class_ "form-control"
                 , type_ "text"
                 , id_ "share-url-input"
                 , value_ [iii|/s/#{hashText}|]
                 , readonly_ ""
                 ]
          button_ [ class_ "btn btn-control"
                  , onclick_ "navigator.clipboard.writeText(document.getElementById('share-url-input').value); this.textContent='Copied!'; setTimeout(()=>this.textContent='Copy',1500)"
                  ] "Copy"
        script_ [iii|(function(){ var el = document.getElementById('share-url-input'); if(el) el.value = window.location.origin + el.value; })()|]


-- | Modal listing all active shared links.
shareLinksModal :: [SharedLink] -> Html ()
shareLinksModal links =
  div_ [ id_ "share-links-modal", class_ "modal" ] do
    div_ [ class_ "modal-underlay", onclick_ "this.closest('.modal').remove()" ] mempty
    div_ [ class_ "modal-content share-links-content" ] do
      div_ [ class_ "modal-title-bar" ] do
        span_ do
          i_ [ class_ "bx bxs-share-alt" ] mempty
          " Shared Links"
        div_ [ class_ "modal-title-bar-control" ] do
          span_ [ class_ "btn-modal-close", onclick_ "this.closest('.modal').remove()" ] do
            i_ [ class_ "bx bx-x" ] mempty
      div_ [ class_ "share-links-body" ] do
        if null links
          then div_ [ class_ "share-links-empty" ] "No active shared links."
          else div_ [ class_ "share-links-list" ] (mapM_ linkRow links)


-- | A single row in the links list.
linkRow :: SharedLink -> Html ()
linkRow link =
  div_ [ id_ rowId, class_ "share-link-item" ] do
    div_ [ class_ "share-link-info" ] do
      div_ [ class_ "share-link-name" ] do
        icon link.file
        span_ (toHtml displayName)
      div_ [ class_ "share-link-meta" ] do
        span_ [ class_ "share-link-type-label" ] (toHtml typeLabel)
        span_ (toHtml (show link.accessCount <> " reads"))
        when link.readonly do
          span_ [ class_ "shared-readonly-badge" ] "ro"
        when (isJust link.passcode) do
          i_ [ class_ "bx bxs-lock-alt", title_ "password protected" ] mempty
        when (link.linkType == SingleUse && link.accessCount > 0) do
          span_ [ class_ "shared-readonly-badge" ] "used"
    div_ [ class_ "share-link-actions" ] do
      button_ [ class_ "btn btn-control"
              , title_ "Copy link"
              , onclick_ [iii|navigator.clipboard.writeText(window.location.origin+'/s/#{hashText}');this.innerHTML='<i class=\\"bx bx-check\\"></i>';setTimeout(()=>this.innerHTML='<i class=\\"bx bxs-copy\\"></i>',1500)|]
              ] do
        i_ [ class_ "bx bxs-copy" ] mempty
      button_ [ class_ "btn btn-control"
              , title_ "Edit"
              , term "hx-get" [iii|/share/links/edit?hash=#{hashText}|]
              , term "hx-target" "#share-links-modal"
              , term "hx-swap" "outerHTML"
              ] do
        i_ [ class_ "bx bxs-edit" ] mempty
      button_ [ class_ "btn btn-control urgent"
              , title_ "Revoke"
              , term "hx-delete" [iii|/share/links/revoke?hash=#{hashText}|]
              , term "hx-target" "#share-links-modal"
              , term "hx-swap" "outerHTML"
              ] do
        i_ [ class_ "bx bxs-trash" ] mempty
  where
    SharedLinkHash hashText = link.hash
    rowId = "link-row-" <> hashText
    displayName = takeFileName (coerce link.file.path)
    typeLabel = case link.linkType of
      SingleUse  -> "Single-use" :: Text
      Indefinite -> "Indefinite"
      Expiry t   -> "Expires " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t)


-- | Full modal replacement showing the edit form for one link.
-- Cancel and Save both swap the whole modal back to the list.
linkEditModal :: SharedLink -> Html ()
linkEditModal link =
  div_ [ id_ "share-links-modal", class_ "modal" ] do
    div_ [ class_ "modal-underlay", onclick_ "this.closest('.modal').remove()" ] mempty
    div_ [ class_ "modal-content share-links-content" ] do
      div_ [ class_ "modal-title-bar" ] do
        span_ [ style_ "display:flex;align-items:center;gap:6px" ] do
          button_ [ class_ "btn btn-control"
                  , style_ "padding:2px 6px"
                  , term "hx-get" "/share/links"
                  , term "hx-target" "#share-links-modal"
                  , term "hx-swap" "outerHTML"
                  ] do
            i_ [ class_ "bx bx-chevron-left" ] mempty
          toHtml displayName
        div_ [ class_ "modal-title-bar-control" ] do
          span_ [ class_ "btn-modal-close", onclick_ "this.closest('.modal').remove()" ] do
            i_ [ class_ "bx bx-x" ] mempty
      div_ [ class_ "share-links-body" ] do
        form_ [ term "hx-post" "/share/links/update"
               , term "hx-target" "#share-links-modal"
               , term "hx-swap" "outerHTML"
               ] do
          input_ [ type_ "hidden", name_ "hash", value_ hashText ]
          div_ [ class_ "share-edit-form" ] do
            div_ [ class_ "share-link-type-group" ] do
              label_ [ class_ "share-radio-label" ] do
                input_ ([ type_ "radio", name_ "linkType", value_ "indefinite" ]
                        <> [ checked_ | link.linkType == Indefinite ])
                " Indefinite"
              label_ [ class_ "share-radio-label" ] do
                input_ ([ type_ "radio", name_ "linkType", value_ "singleuse" ]
                        <> [ checked_ | link.linkType == SingleUse ])
                " Single-use"
              label_ [ class_ "share-radio-label" ] do
                input_ ([ type_ "radio", name_ "linkType", value_ "expiry" ]
                        <> [ checked_ | isExpiry link.linkType ])
                " Expires after "
                input_ [ class_ "share-expiry-input"
                       , type_ "number", name_ "expiryHours"
                       , value_ "24", min_ "1", max_ "8760"
                       ]
                " h"
            input_ [ class_ "form-control"
                   , type_ "password"
                   , name_ "passcode"
                   , placeholder_ (if isJust link.passcode
                                     then "New passcode (blank = clear existing)"
                                     else "Set passcode (optional)")
                   ]
            label_ [ style_ "display:flex;align-items:center;gap:6px;font-size:0.9rem" ] do
              input_ ([ type_ "checkbox", name_ "readonly", value_ "true" ]
                      <> [ checked_ | link.readonly ])
              " Read-only"
          div_ [ class_ "share-edit-actions" ] do
            button_ [ class_ "btn btn-control"
                    , type_ "button"
                    , term "hx-get" "/share/links"
                    , term "hx-target" "#share-links-modal"
                    , term "hx-swap" "outerHTML"
                    ] "Cancel"
            button_ [ class_ "btn btn-accent", type_ "submit" ] "Save"
  where
    SharedLinkHash hashText = link.hash
    displayName = takeFileName (coerce link.file.path)
    isExpiry (Expiry _) = True
    isExpiry _          = False
