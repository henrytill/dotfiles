return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.8",
	dependencies = { "nvim-lua/plenary.nvim" },
	cmd = { "Telescope" },
	keys = {
		{ "<leader>ff", desc = "Telescope find files" },
		{ "<leader>fg", desc = "Telescope live grep" },
		{ "<leader>fb", desc = "Telescope buffers" },
		{ "<leader>fh", desc = "Telescope help tags" },
	},
	config = function()
		local builtin = require("telescope.builtin")
		local actions = require("telescope.actions")

		require("telescope").setup({
			defaults = {
				mappings = {
					i = {
						["<C-d>"] = actions.delete_buffer,
					},
					n = {
						["<C-d>"] = actions.delete_buffer,
					},
				},
			},
		})

		local function find_files_all()
			builtin.find_files({
				hidden = true,
				file_ignore_patterns = { "^%.git/" },
			})
		end

		vim.keymap.set("n", "<leader>ff", find_files_all, { desc = "Telescope find files" })
		vim.keymap.set("n", "<leader>fg", builtin.live_grep, { desc = "Telescope live grep" })
		vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Telescope buffers" })
		vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Telescope help tags" })

		vim.api.nvim_create_autocmd("LspAttach", {
			callback = function(args)
				-- Navigation (g* prefix)
				vim.keymap.set("n", "gd", function()
					builtin.lsp_definitions({ reuse_win = true })
				end, { buffer = args.buf, desc = "Goto definition" })

				vim.keymap.set("n", "grr", function()
					builtin.lsp_references({ reuse_win = true })
				end, { buffer = args.buf, desc = "References (LSP via Telescope)" })

				vim.keymap.set("n", "gi", function()
					builtin.lsp_implementations({ reuse_win = true })
				end, { buffer = args.buf, desc = "Goto implementation" })

				vim.keymap.set("n", "gy", function()
					builtin.lsp_type_definitions({ reuse_win = true })
				end, { buffer = args.buf, desc = "Goto type definition" })

				-- Search (<leader>s* prefix)
				vim.keymap.set(
					"n",
					"<leader>ss",
					builtin.lsp_document_symbols,
					{ buffer = args.buf, desc = "Document symbols" }
				)

				vim.keymap.set(
					"n",
					"<leader>sS",
					builtin.lsp_workspace_symbols,
					{ buffer = args.buf, desc = "Workspace symbols" }
				)

				vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { buffer = args.buf, desc = "Diagnostics" })
			end,
		})
	end,
}
