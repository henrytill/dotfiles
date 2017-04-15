" Some Haskell customizations

set wildignore+=**/dist/**

if executable('cabal')
  set makeprg=cabal\ build\ -v0
  command! RunTests !cabal test --show-details=streaming
endif

nnoremap <F5> :make<CR>
nnoremap <F6> :RunTests<CR>
