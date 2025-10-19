return {
	"luc-tielen/telescope_hoogle",
	dependencies = { "nvim-telescope/telescope.nvim" },
	ft = "haskell",
	config = function()
		local telescope = require("telescope")
		telescope.load_extension("hoogle")
		vim.keymap.set("n", "<leader>fH", function()
			telescope.extensions.hoogle.hoogle({ default_text = vim.fn.expand("<cword>") })
		end, { desc = "Telescope hoogle" })
	end,
}
