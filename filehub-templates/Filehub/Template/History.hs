module Filehub.Template.History (historyModal) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import Filehub.History (HistoryEntry(..), Operation(..))
import Filehub.History.Types (clientName, clientFullPath)
import Filehub.Locale (Locale(..))
import Lucid
import System.FilePath (takeExtension)
import Target.Types (TargetId)


historyModal :: Locale -> (TargetId -> Text) -> [HistoryEntry] -> Html ()
historyModal loc labelOf entries =
  div_ [ id_ "history-modal", class_ "modal" ] do
    div_ [ class_ "modal-underlay", onclick_ "this.closest('.modal').remove()" ] mempty
    div_ [ class_ "modal-content history-modal-content" ] do
      div_ [ class_ "modal-title-bar" ] do
        span_ do
          i_ [ class_ "bx bx-history" ] mempty
          " History"
        div_ [ class_ "modal-title-bar-control" ] do
          span_ [ class_ "btn-modal-close", onclick_ "this.closest('.modal').remove()" ] do
            i_ [ class_ "bx bx-x" ] mempty
      div_ [ class_ "history-modal-body" ] do
        if null entries
          then div_ [ class_ "history-empty" ] "No operations recorded yet."
          else table_ [ class_ "history-table" ] do
            thead_ do
              tr_ do
                th_ [ class_ "col-time"   ] "Time"
                th_ [ class_ "col-op"     ] "Operation"
                th_ [ class_ "col-target" ] "Target"
                th_ [ class_ "col-desc"   ] "Details"
                th_ [ class_ "col-status" ] "Status"
            tbody_ do
              mapM_ (entryRow loc labelOf) entries


entryRow :: Locale -> (TargetId -> Text) -> HistoryEntry -> Html ()
entryRow loc labelOf e =
  tr_ [ class_ rowClass ] do
    td_ [ class_ "history-time"   ] (toHtml timeStr)
    td_ [ class_ "history-op"     ] (toHtml (translateOp loc e.operation))
    td_ [ class_ "history-target" ] (toHtml (labelOf (opTargetId e.operation)))
    td_ [ class_ "history-desc"   ] (descCell e.operation)
    td_ [ class_ "history-status" ] statusBadge
  where
    timeStr  = T.pack $ formatTime defaultTimeLocale "%m/%d %H:%M" e.timestamp
    rowClass = if e.succeeded then "history-row" else "history-row history-row-failed" :: Text
    statusBadge
      | e.succeeded = span_ [ class_ "badge badge-ok"   ] "ok"
      | otherwise   = span_ [ class_ "badge badge-fail" ] "fail"


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


descCell :: Operation -> Html ()
descCell op = case expandItems op of
  Nothing    -> toHtml (describeOp op)
  Just []    -> toHtml (describeOp op)
  Just names ->
    details_ [ class_ "history-expand" ] do
      summary_ [ class_ "history-expand-summary" ] do
        toHtml (describeOp op)
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
        ext      = takeExtension (T.unpack name)
        itemIcon
          | ext /= "" = "bx bxs-file-blank history-item-icon"
          | otherwise = "bx bx-file history-item-icon"


-- | Returns the item paths to show in the expandable list, Nothing if not expandable.
expandItems :: Operation -> Maybe [Text]
expandItems = \case
  OpMove   _ srcs _ -> Just [ T.pack (clientFullPath p) | p <- srcs ]
  OpDelete _ ps     -> Just [ T.pack (clientFullPath p) | p <- ps   ]
  OpPaste  _ _ ns   -> Just ns
  _                 -> Nothing


describeOp :: Operation -> Text
describeOp = \case
  OpRename    _ from to -> T.pack $ clientName from <> " → " <> clientName to
  OpNewFile   _ p       -> T.pack $ clientName p
  OpNewFolder _ p       -> T.pack $ clientName p
  OpMove      _ srcs _  -> T.pack $ show (length srcs) <> " item(s)"
  OpDelete    _ ps      -> T.pack $ show (length ps)   <> " item(s)"
  OpUpload    _ p       -> T.pack $ clientName p
  OpCopy      _ n       -> T.pack $ show n              <> " item(s)"
  OpPaste     _ _ ns    -> T.pack $ show (length ns)    <> " item(s)"
  OpShare     _ p _     -> T.pack $ clientName p
  OpRevokeLink _ _      -> ""


translateOp :: Locale -> Operation -> Text
translateOp loc op = case loc of
  EN    -> en
  ZH_CN -> zh
  ZH_TW -> zht
  ZH_HK -> zht
  JA    -> ja
  ES    -> es
  FR    -> fr
  DE    -> de
  KO    -> ko
  RU    -> ru
  PT    -> pt
  IT    -> it
  where
    (en, zh, zht, ja, es, fr, de, ko, ru, pt, it) = case op of
      OpRename    {} -> ("Rename",      "重命名",    "重新命名",   "名前変更",     "Renombrar",       "Renommer",         "Umbenennen",    "이름 변경",  "Переименовать", "Renomear",     "Rinomina")
      OpNewFile   {} -> ("New file",    "新建文件",   "新增檔案",   "新規ファイル",  "Nuevo archivo",   "Nouveau fichier",  "Neue Datei",    "새 파일",    "Новый файл",    "Novo arquivo", "Nuovo file")
      OpNewFolder {} -> ("New folder",  "新建文件夹",  "新增資料夾",  "新規フォルダ",  "Nueva carpeta",   "Nouveau dossier",  "Neuer Ordner",  "새 폴더",    "Новая папка",   "Nova pasta",   "Nuova cartella")
      OpMove      {} -> ("Move",        "移动",      "移動",      "移動",         "Mover",           "Déplacer",         "Verschieben",   "이동",       "Переместить",   "Mover",        "Sposta")
      OpDelete    {} -> ("Delete",      "删除",      "刪除",      "削除",         "Eliminar",        "Supprimer",        "Löschen",       "삭제",       "Удалить",       "Excluir",      "Elimina")
      OpUpload    {} -> ("Upload",      "上传",      "上傳",      "アップロード",   "Subir",           "Téléverser",       "Hochladen",     "업로드",     "Загрузить",     "Enviar",       "Carica")
      OpCopy      {} -> ("Copy",        "复制",      "複製",      "コピー",        "Copiar",          "Copier",           "Kopieren",      "복사",       "Копировать",    "Copiar",       "Copia")
      OpPaste     {} -> ("Paste",       "粘贴",      "貼上",      "貼り付け",      "Pegar",           "Coller",           "Einfügen",      "붙여넣기",   "Вставить",      "Colar",        "Incolla")
      OpShare     {} -> ("Share",       "分享",      "分享",      "共有",         "Compartir",       "Partager",         "Teilen",        "공유",       "Поделиться",    "Compartilhar", "Condividi")
      OpRevokeLink{} -> ("Revoke link", "撤销链接",   "撤銷連結",   "リンク取消",    "Revocar enlace",  "Révoquer le lien", "Link widerrufen","링크 취소",  "Отозвать ссылку","Revogar link", "Revoca link")
