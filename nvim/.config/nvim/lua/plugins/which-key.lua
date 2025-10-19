return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	opts = {
		delay = function(ctx)
			return ctx.plugin and 0 or 1000
		end,
	},
	keys = {
		{
			"<leader>?",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Buffer Local Keymaps (which-key)",
		},
	},
}
