setlocal shiftwidth=2
setlocal tabstop=2

if filereadable('CMakeLists.txt') && isdirectory('build')
  set wildignore+=**/build/**
  set makeprg=cmake\ --build\ build
endif
