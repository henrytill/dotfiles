""" formatting utilities

function! StripTrailingWhitespace()
  let pos = getpos('.')
  silent! %s/\s\+$//
  call setpos('.', pos)
endfunction

function! HiTabs()
  if has("autocmd")
    " highlight tabs
    highlight Tab ctermbg=LightGray guibg=LightGray
    match Tab /\t/
    au BufWinEnter,InsertLeave * match Tab /\t/
    au InsertEnter * match Tab /\t/
    " prevent colorscheme from overriding these highlights
    au ColorScheme * highlight Tab ctermbg=LightGray guibg=LightGray
  endif
endfunction

""" clang-format

function! RunClangFormat()
  if !executable('clang-format')
    echo 'Could not locate clang-format'
    return
  endif
  if !&modified || empty(findfile('.clang-format', expand('%:p:h') . ';'))
    return
  endif
  let pos = getpos('.')
  silent! %!clang-format
  call setpos('.', pos)
endfunction

""" gofmt

function! RunGofmt()
  if !executable('gofmt')
    echo 'Could not locate gofmt'
    return
  endif
  let pos = getpos('.')
  silent! %!gofmt
  call setpos('.', pos)
endfunction

""" ghc-tags

function! RunGhcTags()
  if !executable('ghc-tags')
    echo 'Could not locate ghc-tags'
    return
  endif
  execute "silent! !ghc-tags -c 2>/dev/null &"
endfunction

""" autocmd invocation

if has("autocmd")
  autocmd BufWritePre  *.h,*.c  :call RunClangFormat()
  autocmd BufWritePre  *.go     :call RunGofmt()
  autocmd BufWritePost *.hs     :call RunGhcTags()
endif
