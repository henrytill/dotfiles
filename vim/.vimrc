" vimrc

set nocompatible

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'idris-hackers/idris-vim', { 'for': 'idris' }
Plug 'raichoo/purescript-vim',  { 'for': 'purescript' }
Plug 'derekwyatt/vim-scala',    { 'for': ['scala', 'sbt.scala'] }
Plug 'let-def/ocp-indent-vim',  { 'for': 'ocaml' }
Plug 'LnL7/vim-nix',            { 'for': 'nix' }
Plug 'rust-lang/rust.vim',      { 'for': 'rust' }
Plug 'racer-rust/vim-racer',    { 'for': 'rust' }
Plug 'lyuts/vim-rtags',         { 'for': ['cpp', 'c'] }
call plug#end()

syntax on
set background=light

set expandtab
set ignorecase
set incsearch
set modeline
set nowrap
set relativenumber
set ruler
set showcmd
set showmatch
set smartcase
set smartindent
set smarttab
set wildmenu

set backspace=indent,eol,start
set clipboard=unnamed
set laststatus=2
set path=.,,$PWD/**
set shiftwidth=2
set softtabstop=2
set tabstop=2
set textwidth=80

setglobal tags=./tags;

function! StripTrailingWhitespace()
  let myline = line(".")
  let mycolumn = col(".")
  silent! %s/\s\+$//
  call cursor(myline, mycolumn)
endfunction

function! RunClangFormat()
  if executable('clang-format')
    let myline = line(".")
    let mycolumn = col(".")
    silent! %!clang-format
    call cursor(myline, mycolumn)
  else
    echo 'Could not locate clang-format'
  endif
endfunction

function! RunGofmt()
  if executable('gofmt')
    let myline = line(".")
    let mycolumn = col(".")
    silent! %!gofmt
    call cursor(myline, mycolumn)
  else
    echo 'Could not locate gofmt'
  endif
endfunction

function! RunPrettier()
  if executable('prettier')
    let myline = line(".")
    let mycolumn = col(".")
    silent! %!prettier --stdin --single-quote
    call cursor(myline, mycolumn)
  else
    echo 'Could not locate prettier'
  endif
endfunction

function! RunCtags(...)
  if executable('ctags')
    if len(a:000) == 0
      let optionString = ""
    else
      let optionString = join(a:000, " ")
    endif
    execute "silent! !ctags " . optionString . " 2>/dev/null &"
  else
    echo 'Could not locate ctags'
  endif
endfunction

if has("autocmd")
  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " highlight unnecessary whitespace
  highlight ExtraWhitespace ctermbg=red guibg=red
  match ExtraWhitespace /\s\+$/
  au BufWinEnter,InsertLeave * match ExtraWhitespace /\s\+$/
  au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
  " prevent colorscheme from overriding these highlights
  au ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  " merlin
  let g:opamshare = substitute($OCAML_TOPLEVEL_PATH, 'lib/toplevel', 'share', "")
  execute "set rtp+=" . g:opamshare . "/merlin/vim"

  " strip trailing whitespace on save
  let s:strippable = '*.md,*.hs,*.scala,*.sbt,*.ml'
  execute "au BufWritePre " . s:strippable . " silent call StripTrailingWhitespace()"

  " run clang-format on save
  let s:clang_formattable = '*.cpp,*.cc,*.c,*.hpp,*.hh,*.h'
  execute "au BufWritePre " . s:clang_formattable .  " silent call RunClangFormat()"

  au BufWritePost *.go    silent call RunCtags("-R", "--languages=go")
  au BufWritePost *.scala silent call RunCtags("-R", "--languages=scala,java", "--exclude=target")

  " use c syntax for *.h files
  let g:c_syntax_for_h = 1
endif

highlight LineNr        term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
highlight CursorLineNr  term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

map <space> <leader>

nnoremap - :e %:h<CR>
nnoremap <F5> :make<CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
