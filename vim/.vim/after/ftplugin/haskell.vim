" Some Haskell customizations

set wildignore+=*/dist/*
set wildignore+=*/dist-newstyle/*

if executable('cabal')
  if filereadable("cabal.project.local")
    set makeprg=cabal\ new-build\ -v0
    command! CabalTest !cabal new-test
  else
    set makeprg=cabal\ build\ -v0
    command! CabalTest !cabal test --show-details=streaming
  endif

  command! CabalHaddock !cabal haddock

  nnoremap <F6> :CabalTest<CR>
  nnoremap <F7> :CabalHaddock<CR>
endif

function! RunHasktags(...)
  if executable('hasktags') && filereadable('tags')
    if len(a:000) == 0
      let optionString = ""
    else
      let optionString = join(a:000, " ")
    endif
    execute "silent! !hasktags " . optionString . " 2>/dev/null &"
  else
    echo 'Could not locate hasktags'
  endif
endfunction

au BufWritePost *.hs silent call RunHasktags("--ignore-close-implementation", "--ctags", ".")
