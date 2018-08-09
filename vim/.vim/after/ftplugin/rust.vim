set wildignore+=*/target/*

set hidden
let g:racer_cmd = expand("$HOME/.cargo/bin/racer")
let g:racer_experimental_completer = 1

nmap gd <Plug>(rust-def)
nmap gs <Plug>(rust-def-split)
nmap gx <Plug>(rust-def-vertical)
nmap <leader>gd <Plug>(rust-doc)

nnoremap <F5> :make check --all<CR>
nnoremap <F6> :make build --all<CR>
nnoremap <F7> :make test --all<CR>
nnoremap <F8> :make run<CR>
