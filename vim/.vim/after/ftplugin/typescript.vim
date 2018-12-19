setlocal shiftwidth=4
setlocal tabstop=4

if isdirectory('dist')
  set wildignore+=**/dist/**
endif

if isdirectory('node_modules')
  set wildignore+=**/node_modules/**
endif

if filereadable("package.json")
  set makeprg=yarn\ $*
endif
