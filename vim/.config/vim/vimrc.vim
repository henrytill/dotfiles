set ttimeoutlen=50

packadd vim-easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

function! s:LazyLoadFzf(cmd, args, bang)
  delcommand Files
  delcommand Buffers
  delcommand Rg
  packadd fzf
  packadd fzf.vim
  execute a:cmd . (a:bang ? '!' : '') . ' ' . a:args
endfunction

command! -nargs=* -bang Files   call s:LazyLoadFzf('Files',   <q-args>, <bang>0)
command! -nargs=* -bang Buffers call s:LazyLoadFzf('Buffers', <q-args>, <bang>0)
command! -nargs=* -bang Rg      call s:LazyLoadFzf('Rg',      <q-args>, <bang>0)
