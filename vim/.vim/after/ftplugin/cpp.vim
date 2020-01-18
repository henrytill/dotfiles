setlocal noexpandtab
setlocal shiftwidth=8
setlocal softtabstop=0
setlocal tabstop=8

if filereadable('CMakeLists.txt') && isdirectory('build')
  set wildignore+=**/build/**
  set makeprg=cmake\ --build\ build
endif
