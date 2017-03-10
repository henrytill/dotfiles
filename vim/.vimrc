" vimrc

set nocompatible

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'idris-hackers/idris-vim', { 'for': 'idris' }
Plug 'raichoo/purescript-vim',  { 'for': 'purescript' }
Plug 'derekwyatt/vim-scala',    { 'for': 'scala' }
Plug 'let-def/ocp-indent-vim',  { 'for': 'ocaml' }
Plug 'LnL7/vim-nix',            { 'for': 'nix' }
call plug#end()

syntax on
set background=dark

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

set backspace=indent,eol,start
set clipboard=unnamed
set laststatus=2
set shiftwidth=2
set softtabstop=2
set tabstop=2
set textwidth=80

map <space> <leader>

setglobal tags=./tags;

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
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  execute "set rtp+=" . g:opamshare . "/merlin/vim"
endif

highlight LineNr        term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
highlight CursorLineNr  term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
