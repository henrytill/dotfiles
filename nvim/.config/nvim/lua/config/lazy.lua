local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local vimpath = vim.fn.expand("~/.vim")
local vimafterpath = vim.fn.expand("~/.vim/after")

if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end

vim.opt.rtp:prepend(lazypath)

vim.opt.rtp:prepend(vimpath)
vim.opt.rtp:append(vimafterpath)
vim.cmd("source ~/.vim/vimrc")

require("lazy").setup({
	spec = {
		{ import = "plugins" },
	},
	install = {},
	checker = { enabled = true },
	performance = {
		rtp = {
			paths = { vimpath, vimafterpath },
		},
	},
})

vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostic float" })
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
