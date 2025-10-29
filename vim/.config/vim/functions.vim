function! StripTrailingWhitespace()
  let pos = getpos('.')
  silent! %s/\s\+$//
  call setpos('.', pos)
endfunction

function! HiTrailingWhitespace()
  if !has("autocmd")
    return
  endif
  " highlight trailing whitespace
  highlight TrailingWhitespace ctermbg=LightRed guibg=LightRed
  match TrailingWhitespace /\s\+$/
  au BufWinEnter,InsertLeave * match TrailingWhitespace /\s\+$/
  au InsertEnter * match TrailingWhitespace /\s\+$/
  " prevent colorscheme from overriding these highlights
  au ColorScheme * highlight TrailingWhitespace ctermbg=LightRed guibg=LightRed
endfunction

function! HiTabs()
  if !has("autocmd")
    return
  endif
  " highlight tabs
  highlight Tab ctermbg=LightGray guibg=LightGray
  match Tab /\t/
  au BufWinEnter,InsertLeave * match Tab /\t/
  au InsertEnter * match Tab /\t/
  " prevent colorscheme from overriding these highlights
  au ColorScheme * highlight Tab ctermbg=LightGray guibg=LightGray
endfunction

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

function! RunFourmolu()
  if !executable('fourmolu')
    echo 'Could not locate fourmolu'
    return
  endif
  if !&modified || empty(findfile('fourmolu.yaml', expand('%:p:h') . ';'))
    return
  endif
  let pos = getpos('.')
  silent! %!fourmolu -q --stdin-input-file=%:p
  call setpos('.', pos)
endfunction

function! RunGhcTags()
  if !executable('ghc-tags')
    echo 'Could not locate ghc-tags'
    return
  endif
  execute "silent! !ghc-tags -c 2>/dev/null &"
endfunction

function! RunGofmt()
  if !executable('gofmt')
    echo 'Could not locate gofmt'
    return
  endif
  let pos = getpos('.')
  silent! %!gofmt
  call setpos('.', pos)
endfunction

function! RunNixfmt()
  if !executable('nixfmt')
    echo 'Could not locate nixfmt'
    return
  endif
  let pos = getpos('.')
  silent! %!nixfmt
  call setpos('.', pos)
endfunction

function! RunOcamlformat()
  if !executable('ocamlformat')
    echo 'Could not locate ocamlformat'
    return
  endif
  if !&modified || empty(findfile('.ocamlformat', expand('%:p:h') . ';'))
    return
  endif
  let pos = getpos('.')
  silent! %!ocamlformat --name=%:p -
  call setpos('.', pos)
endfunction

if has("autocmd")
  autocmd BufWritePre  *.h,*.c   :call RunClangFormat()
  autocmd BufWritePre  *.hs      :call RunFourmolu()
  autocmd BufWritePost *.hs      :call RunGhcTags()
  autocmd BufWritePre  *.go      :call RunGofmt()
  autocmd BufWritePre  *.nix     :call RunNixfmt()
  autocmd BufWritePre  *.ml{,i}  :call RunOcamlformat()
endif
