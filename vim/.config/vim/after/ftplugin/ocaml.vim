setlocal tabstop=2

if filereadable("dune-project")
  set makeprg=dune\ build\ $*
endif
