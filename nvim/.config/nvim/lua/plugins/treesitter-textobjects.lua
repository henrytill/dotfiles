return {
	"nvim-treesitter/nvim-treesitter-textobjects",
	branch = "master",
	dependencies = { "nvim-treesitter/nvim-treesitter" },
	event = "VeryLazy",
	config = function()
		require("nvim-treesitter-textobjects").init()

		local keys = {
			goto_next_start = { ["]f"] = "@function.outer", ["]c"] = "@class.outer", ["]a"] = "@parameter.inner" },
			goto_next_end = { ["]F"] = "@function.outer", ["]C"] = "@class.outer", ["]A"] = "@parameter.inner" },
			goto_previous_start = {
				["[f"] = "@function.outer",
				["[c"] = "@class.outer",
				["[a"] = "@parameter.inner",
			},
			goto_previous_end = {
				["[F"] = "@function.outer",
				["[C"] = "@class.outer",
				["[A"] = "@parameter.inner",
			},
		}

		local function attach(buf)
			local ft = vim.bo[buf].filetype
			local has_parser = pcall(vim.treesitter.get_parser, buf, ft)
			if not has_parser then
				return
			end

			for method, keymaps in pairs(keys) do
				for key, query in pairs(keymaps) do
					local desc = query:gsub("@", ""):gsub("%..*", "")
					desc = desc:sub(1, 1):upper() .. desc:sub(2)
					desc = (key:sub(1, 1) == "[" and "Prev " or "Next ") .. desc
					desc = desc .. (key:sub(2, 2) == key:sub(2, 2):upper() and " End" or " Start")
					-- Skip ]c/[c in diff mode to preserve native diff navigation
					if not (vim.wo.diff and key:find("[cC]")) then
						vim.keymap.set({ "n", "x", "o" }, key, function()
							require("nvim-treesitter.textobjects.move")[method](query, "textobjects")
						end, {
							buffer = buf,
							desc = desc,
							silent = true,
						})
					end
				end
			end
		end

		-- Attach to new buffers when filetype is detected
		vim.api.nvim_create_autocmd("FileType", {
			group = vim.api.nvim_create_augroup("treesitter_textobjects", { clear = true }),
			callback = function(ev)
				attach(ev.buf)
			end,
		})
		-- Attach to all existing buffers
		vim.tbl_map(attach, vim.api.nvim_list_bufs())
	end,
}
