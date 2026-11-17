export const defaultI18n = {
    delete: 'Delete', rename: 'Rename', copy: 'Copy', download: 'Download',
    details: 'Details', edit: 'Edit', view: 'View', play: 'Play', open: 'Open',
    cancel: 'Cancel', selected: 'selected',
    confirmDelete1: 'Delete {}?', confirmDeleteLocal: 'Delete {} items?',
    readOnly: false,
    modal_file: 'New File',
    modal_folder: 'New Folder',
    modal_create: 'Create',
    modal_confirm: 'Rename',
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
};
export function i18n() {
    return { ...defaultI18n, ...(window.filehubI18n ?? {}) };
}
