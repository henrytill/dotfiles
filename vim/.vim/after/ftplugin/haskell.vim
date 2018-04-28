" Some Haskell customizations

set wildignore+=*/dist/*
set wildignore+=*/dist-newstyle/*

if executable('cabal')
  set makeprg=cabal\ build\ -v0
  command! CabalTest    !cabal test --show-details=streaming
  command! CabalHaddock !cabal haddock
  nnoremap <F6> :CabalTest<CR>
  nnoremap <F7> :CabalHaddock<CR>
endif
