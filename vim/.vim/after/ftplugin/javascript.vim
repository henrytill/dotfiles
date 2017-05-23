setlocal shiftwidth=2
setlocal tabstop=2

if isdirectory('dist')
  set wildignore+=**/dist/**
endif

if isdirectory('node_modules')
  set wildignore+=**/node_modules/**
endif

if filereadable('webpack.config.js')
  set backupcopy=yes
endif

nnoremap <leader>gp :silent call RunPrettier()<CR>
