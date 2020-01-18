set wildignore+=*/target/*

if filereadable("Makefile")
  set makeprg=make\ $*
endif
