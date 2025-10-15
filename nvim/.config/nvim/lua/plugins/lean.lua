return {
	"Julian/lean.nvim",
	event = { "BufReadPre *.lean", "BufNewFile *.lean" },

	dependencies = {
		"neovim/nvim-lspconfig",
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope.nvim",
	},

	opts = {
		mappings = true,

		lsp = {
			init_options = {
				-- Lower edit delay for faster feedback
				editDelay = 0,
				hasWidgets = true,
			},
		},

		infoview = {
			autoopen = true,
			width = 50,
			height = 20,
		},
	},
}
