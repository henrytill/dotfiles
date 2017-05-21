setlocal shiftwidth=4
setlocal tabstop=4

if filereadable('CMakeLists.txt') && isdirectory('build')
  set wildignore+=**/build/**
  set makeprg=cmake\ --build\ build
endif
