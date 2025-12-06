setlocal tabstop=2

if filereadable("dune-project")
  set makeprg=dune\ build\ $*
endif

for group in getcompletion('ocaml', 'highlight')
  execute 'highlight clear' group
endfor
