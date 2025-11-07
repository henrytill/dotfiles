" Vim color file
" Maintainer:	Henry Till <henrytill@gmail.com>

" Remove all existing highlighting and set the defaults.
highlight clear Normal
highlight clear

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
  syntax reset
endif

let colors_name = "inl"

highlight Normal        term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Comment       term=NONE           cterm=NONE          ctermfg=Gray        ctermbg=NONE        gui=NONE            guifg=#595959       guibg=NONE
highlight Constant      term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight CursorLine    term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=LightGray   gui=NONE            guifg=NONE          guibg=#bfbfbf
highlight CursorLineNr  term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=LightGray   gui=NONE            guifg=NONE          guibg=#bfbfbf
highlight Error         term=reverse        cterm=NONE          ctermfg=Red         ctermbg=NONE        gui=NONE            guifg=Red           guibg=NONE
highlight Folded        term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=LightGray   gui=NONE            guifg=NONE          guibg=#f5f5f5
highlight Function      term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Identifier    term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Keyword       term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight LineNr        term=NONE           cterm=NONE          ctermfg=Gray        ctermbg=NONE        gui=NONE            guifg=#bfbfbf       guibg=NONE
highlight Number        term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight NonText       term=NONE           cterm=NONE          ctermfg=Gray        ctermbg=NONE        gui=NONE            guifg=#bfbfbf       guibg=NONE
highlight Operator      term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight PreProc       term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight SignColumn    term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight VertSplit     term=NONE           cterm=NONE          ctermfg=Gray        ctermbg=NONE        gui=NONE            guifg=#bfbfbf       guibg=NONE
highlight Special       term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Statement     term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight StatusLine    term=NONE           cterm=NONE          ctermfg=Black       ctermbg=LightGray   gui=NONE            guifg=#000000       guibg=#c8c8c8
highlight StatusLineNC  term=NONE           cterm=NONE          ctermfg=DarkGray    ctermbg=LightGray   gui=NONE            guifg=#585858       guibg=#e6e6e6
highlight String        term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Title         term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Type          term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Visual        term=reverse        cterm=NONE          ctermfg=Black       ctermbg=Cyan        gui=NONE            guifg=Black         guibg=Cyan

highlight link rustCommentLineDoc Comment

for group in getcompletion('@lsp', 'highlight')
  execute 'highlight clear' group
endfor

" vim: sw=2 nowrap
