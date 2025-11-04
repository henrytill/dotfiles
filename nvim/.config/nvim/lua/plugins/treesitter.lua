return {
	"nvim-treesitter/nvim-treesitter",
	branch = "master",
	lazy = false,
	build = ":TSUpdate",
	config = function()
		require("nvim-treesitter.configs").setup({
			ensure_installed = {
				"c",
				"lua",
				"vim",
				"vimdoc",
				"query",
				"markdown",
				"markdown_inline",
				"go",
				"rust",
				"ocaml",
			},

			sync_install = false,

			auto_install = false,

			ignore_install = {},

			modules = {},

			highlight = {
				enable = false,
			},

			incremental_selection = {
				enable = true,
				keymaps = {
					init_selection = "gnn",
					node_incremental = "grn",
					scope_incremental = "grc",
					node_decremental = "grm",
				},
			},

			indent = {
				enable = true,
			},
		})

		vim.api.nvim_create_autocmd("FileType", {
			pattern = { "c", "go", "rust", "lua", "ocaml", "haskell" },
			callback = function()
				vim.opt_local.foldmethod = "expr"
				vim.opt_local.foldexpr = "v:lua.vim.treesitter.foldexpr()"
				vim.opt_local.foldlevel = 99
			end,
		})
	end,
}
