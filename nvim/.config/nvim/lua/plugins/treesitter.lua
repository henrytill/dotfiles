return {
	"nvim-treesitter/nvim-treesitter",
	branch = "master",
	lazy = false,
	build = ":TSUpdate",
	config = function()
		require("nvim-treesitter.configs").setup({
			-- Install parsers for languages you use
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

			-- Install parsers synchronously
			sync_install = false,

			-- Automatically install missing parsers when entering buffer
			auto_install = true,

			-- List of parsers to ignore installing
			ignore_install = {},

			-- Deprecated modules field (kept for compatibility)
			modules = {},

			highlight = {
				enable = true,
				-- Disable for very large files
				disable = function(_, buf)
					local max_filesize = 100 * 1024 -- 100 KB
					local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
					if ok and stats and stats.size > max_filesize then
						return true
					end
				end,
				additional_vim_regex_highlighting = false,
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
	end,
}
