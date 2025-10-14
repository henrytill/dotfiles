vim.opt.termguicolors = true

vim.opt.runtimepath:append('~/.vim')

vim.cmd('source ~/.vim/vimrc')

vim.opt.laststatus = 0

vim.lsp.enable('rust_analyzer')
