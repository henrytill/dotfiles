vim.opt.runtimepath:append('~/.vim')

vim.cmd('source ~/.vim/vimrc')

-- Configure gopls to use GOPATH if not in PATH
local function find_gopls()
  if vim.fn.executable('gopls') == 1 then
    return 'gopls'
  end

  if vim.fn.executable('go') == 1 then
    local gopath = vim.fn.system('go env GOPATH'):gsub('%s+$', '')
    local gopls_path = gopath .. '/bin/gopls'
    if vim.fn.filereadable(gopls_path) == 1 then
      return gopls_path
    end
  end

  return 'gopls' -- fallback to default
end

vim.lsp.config('gopls', {
  cmd = { find_gopls() },
})

vim.lsp.enable('gopls')

vim.lsp.enable('rust_analyzer')
