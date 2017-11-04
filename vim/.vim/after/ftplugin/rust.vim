set hidden
let g:racer_cmd = expand("$HOME/.cargo/bin/racer")

nmap gd <Plug>(rust-def)
nmap gs <Plug>(rust-def-split)
nmap gx <Plug>(rust-def-vertical)
nmap <leader>gd <Plug>(rust-doc)
