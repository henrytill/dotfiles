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
highlight Comment       term=NONE           cterm=NONE          ctermfg=DarkGray    ctermbg=NONE        gui=NONE            guifg=DarkGray      guibg=NONE
highlight Constant      term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight CursorLine    term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=LightGray   gui=NONE            guifg=NONE          guibg=LightGray
highlight CursorLineNr  term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=LightGray   gui=NONE            guifg=NONE          guibg=LightGray
highlight Error         term=reverse        cterm=NONE          ctermfg=Red         ctermbg=NONE        gui=NONE            guifg=Red           guibg=NONE
highlight Identifier    term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Keyword       term=bold           cterm=bold          ctermfg=NONE        ctermbg=NONE        gui=bold            guifg=NONE          guibg=NONE
highlight LineNr        term=NONE           cterm=NONE          ctermfg=Gray        ctermbg=NONE        gui=NONE            guifg=Gray          guibg=NONE
highlight Number        term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Operator      term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight PreProc       term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Special       term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Statement     term=bold           cterm=bold          ctermfg=NONE        ctermbg=NONE        gui=bold            guifg=NONE          guibg=NONE
highlight Type          term=NONE           cterm=NONE          ctermfg=NONE        ctermbg=NONE        gui=NONE            guifg=NONE          guibg=NONE
highlight Visual        term=reverse        cterm=NONE          ctermfg=Black       ctermbg=Cyan        gui=NONE            guifg=Black         guibg=Cyan

highlight link rustCommentLineDoc Comment

" vim: sw=2 nowrap
