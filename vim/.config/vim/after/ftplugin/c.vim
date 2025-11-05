setlocal tabstop=4

if !has('nvim')
  setlocal foldmethod=syntax
  setlocal foldlevel=99
endif

" Minimal fold styling for C - light gray text with subtle background
highlight Folded term=NONE cterm=NONE ctermfg=Gray ctermbg=LightGray gui=NONE guifg=#999999 guibg=#f5f5f5
