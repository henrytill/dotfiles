" Some Haskell customizations

if executable('nix-cabal')
  set makeprg=nix-cabal\ build\ -v0
  command! RunTests !nix-cabal test --show-details=streaming
elseif executable('cabal')
  set makeprg=cabal\ build\ -v0
  command! RunTests !cabal test --show-details=streaming
endif

nnoremap <F5> :make<CR>
nnoremap <F6> :RunTests<CR>
