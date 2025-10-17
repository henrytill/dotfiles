return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	opts = {
		formatters_by_ft = {
			go = { "gofmt" },
			haskell = { "fourmolu" },
			lua = { "stylua" },
			nix = { "nixfmt" },
			python = { "black" },
		},
		format_on_save = {
			timeout_ms = 500,
			lsp_fallback = true,
		},
	},
}
