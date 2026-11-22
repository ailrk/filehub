export interface I18n {
  delete: string;
  rename: string;
  copy: string;
  cut: string;
  download: string;
  details: string;
  edit: string;
  view: string;
  play: string;
  open: string;
  cancel: string;
  selected: string;
  confirmDelete1: string;
  confirmDeleteLocal: string;
  readOnly: boolean;
  modal_file: string;
  modal_folder: string;
  modal_create: string;
  modal_confirm: string;
  modal_readonly: string;
  modal_detail: string;
  detail_filename: string;
  detail_modified: string;
  detail_accessed: string;
  detail_size: string;
  detail_content_type: string;
  placeholder_newfile: string;
  placeholder_newfoler: string;
  control_panel_dark: string;
  control_panel_light: string;
  new_file: string;
  new_folder: string;
  upload: string;
  paste: string;
  share: string;
  share_link_indefinite: string;
  share_link_singleuse: string;
  share_link_expiry: string;
  share_passcode: string;
  share_create: string;
}

declare global {
  interface Window { filehubI18n?: Partial<I18n>; }
}

export const defaultI18n: I18n = {
  delete: 'Delete', rename: 'Rename', copy: 'Copy', cut: 'Cut', download: 'Download',
  details: 'Details', edit: 'Edit', view: 'View', play: 'Play', open: 'Open',
  cancel: 'Cancel', selected: 'selected',
  confirmDelete1: 'Delete {}?', confirmDeleteLocal: 'Delete {} items?',
  readOnly: false,
  modal_file: 'New File',
  modal_folder: 'New Folder',
  modal_create: 'Create',
  modal_confirm: 'Rename',
  modal_readonly: 'Read-only',
  modal_detail: 'Details',
  detail_filename: 'Filename',
  detail_modified: 'Modified',
  detail_accessed: 'Accessed',
  detail_size: 'Size',
  detail_content_type: 'Type',
  placeholder_newfile: 'filename',
  placeholder_newfoler: 'folder name',
  control_panel_dark: 'Dark',
  control_panel_light: 'Light',
  new_file: 'New File',
  new_folder: 'New Folder',
  upload: 'Upload',
  paste: 'Paste',
  share: 'Share',
  share_link_indefinite: 'Indefinite',
  share_link_singleuse: 'Single use',
  share_link_expiry: 'Expires after',
  share_passcode: 'Passcode (optional)',
  share_create: 'Create link',
};

export function i18n(): I18n {
  return { ...defaultI18n, ...(window.filehubI18n ?? {}) } as I18n;
}
