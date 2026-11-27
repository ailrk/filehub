{-# LANGUAGE LambdaCase #-}
module Filehub.Template.History (historyModal) where

import Filehub.Prelude
import Filehub.History (HistoryEntry(..), Operation(..))
import Filehub.History.Types (clientName, clientFullPath)
import Filehub.Locale (Phrase(..))
import Lucid
import System.FilePath (takeExtension)
import Target.Types (TargetId)


historyModal :: Phrase -> (TargetId -> Text) -> [HistoryEntry] -> Html ()
historyModal p labelOf entries =
  div_ [ id_ "history-modal", class_ "modal" ] do
    div_ [ class_ "modal-underlay", onclick_ "this.closest('.modal').remove()" ] mempty
    div_ [ class_ "modal-content history-modal-content" ] do
      div_ [ class_ "modal-title-bar" ] do
        span_ do
          i_ [ class_ "bx bx-history" ] mempty
          " " <> toHtml p.history_title
        div_ [ class_ "modal-title-bar-control" ] do
          span_ [ class_ "btn-modal-close", onclick_ "this.closest('.modal').remove()" ] do
            i_ [ class_ "bx bx-x" ] mempty
      div_ [ class_ "history-modal-body" ] do
        if null entries
          then div_ [ class_ "history-empty" ] (toHtml p.history_empty)
          else table_ [ class_ "history-table" ] do
            thead_ do
              tr_ do
                th_ [ class_ "col-time"   ] (toHtml p.history_col_time)
                th_ [ class_ "col-op"     ] (toHtml p.history_col_op)
                th_ [ class_ "col-target" ] (toHtml p.history_col_target)
                th_ [ class_ "col-desc"   ] (toHtml p.history_col_details)
                th_ [ class_ "col-status" ] (toHtml p.history_col_status)
            tbody_ do
              mapM_ (entryRow p labelOf) entries


entryRow :: Phrase -> (TargetId -> Text) -> HistoryEntry -> Html ()
entryRow p labelOf e =
  tr_ [ class_ rowClass ] do
    td_ [ class_ "history-time"   ] (toHtml timeStr)
    td_ [ class_ "history-op"     ] (toHtml (translateOp p e.operation))
    td_ [ class_ "history-target" ] (toHtml (labelOf (opTargetId e.operation)))
    td_ [ class_ "history-desc"   ] (descCell p e.operation)
    td_ [ class_ "history-status" ] statusBadge
  where
    timeStr  = txt_pack $ time_format "%m/%d %H:%M" e.timestamp
    rowClass = if e.succeeded then "history-row" else "history-row history-row-failed" :: Text
    statusBadge
      | e.succeeded = span_ [ class_ "badge badge-ok"   ] (toHtml p.history_ok)
      | otherwise   = span_ [ class_ "badge badge-fail" ] (toHtml p.history_fail)


opTargetId :: Operation -> TargetId
opTargetId = \case
  OpRename    tid _ _   -> tid
  OpNewFile   tid _     -> tid
  OpNewFolder tid _     -> tid
  OpMove      tid _ _   -> tid
  OpDelete    tid _     -> tid
  OpUpload    tid _     -> tid
  OpCopy      tid _     -> tid
  OpPaste     tid _ _   -> tid
  OpShare     tid _ _   -> tid
  OpRevokeLink tid _    -> tid


descCell :: Phrase -> Operation -> Html ()
descCell p op = case expandItems op of
  Nothing    -> toHtml (describeOp p op)
  Just []    -> toHtml (describeOp p op)
  Just names ->
    details_ [ class_ "history-expand" ] do
      summary_ [ class_ "history-expand-summary" ] do
        toHtml (describeOp p op)
        i_ [ class_ "bx bx-chevron-right history-expand-chevron" ] mempty
      div_ [ class_ "history-expand-list" ] do
        mapM_ itemRow names
  where
    itemRow :: Text -> Html ()
    itemRow name =
      div_ [ class_ "history-expand-item" ] do
        i_ [ class_ itemIcon ] mempty
        toHtml name
      where
        ext      = takeExtension (txt_unpack name)
        itemIcon
          | ext /= "" = "bx bxs-file-blank history-item-icon"
          | otherwise = "bx bx-file history-item-icon"


-- | Returns the item paths to show in the expandable list, Nothing if not expandable.
expandItems :: Operation -> Maybe [Text]
expandItems = \case
  OpMove   _ srcs _ -> Just [ txt_pack (clientFullPath p) | p <- srcs ]
  OpDelete _ ps     -> Just [ txt_pack (clientFullPath p) | p <- ps   ]
  OpPaste  _ _ ns   -> Just ns
  _                 -> Nothing


describeOp :: Phrase -> Operation -> Text
describeOp p = \case
  OpRename    _ from to -> txt_pack $ clientName from <> " → " <> clientName to
  OpNewFile   _ path    -> txt_pack $ clientName path
  OpNewFolder _ path    -> txt_pack $ clientName path
  OpMove      _ srcs _  -> txt_pack (show (length srcs)) <> " " <> p.history_items
  OpDelete    _ ps      -> txt_pack (show (length ps))   <> " " <> p.history_items
  OpUpload    _ path    -> txt_pack $ clientName path
  OpCopy      _ n       -> txt_pack (show n)             <> " " <> p.history_items
  OpPaste     _ _ ns    -> txt_pack (show (length ns))   <> " " <> p.history_items
  OpShare     _ path _  -> txt_pack $ clientName path
  OpRevokeLink _ _      -> ""


translateOp :: Phrase -> Operation -> Text
translateOp p = \case
  OpRename    {} -> p.history_op_rename
  OpNewFile   {} -> p.history_op_new_file
  OpNewFolder {} -> p.history_op_new_folder
  OpMove      {} -> p.history_op_move
  OpDelete    {} -> p.history_op_delete
  OpUpload    {} -> p.history_op_upload
  OpCopy      {} -> p.history_op_copy
  OpPaste     {} -> p.history_op_paste
  OpShare     {} -> p.history_op_share
  OpRevokeLink{} -> p.history_op_revoke_link
