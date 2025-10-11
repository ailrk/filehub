" Compile the frontend on buffer save. Works well with storybook
if isdirectory("js")
  augroup buildjs_on_save
    autocmd!
    autocmd BufWritePost js/* call jobstart(['just', 'buildjs'])
  augroup END
endif
