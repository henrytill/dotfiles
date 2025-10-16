return {
	"neovim/nvim-lspconfig",
	config = function()
		local function find_gopls()
			if vim.fn.executable("gopls") == 1 then
				return "gopls"
			end

			if vim.fn.executable("go") == 1 then
				local gopath = vim.fn.system("go env GOPATH"):gsub("%s+$", "")
				local gopls_path = gopath .. "/bin/gopls"
				if vim.fn.filereadable(gopls_path) == 1 then
					return gopls_path
				end
			end

			return "gopls"
		end

		vim.lsp.config("gopls", {
			cmd = { find_gopls() },
		})

		vim.lsp.config("lua_ls", {
			settings = {
				Lua = {
					telemetry = {
						enable = false,
					},
				},
			},
		})

		vim.lsp.enable("clangd")
		vim.lsp.enable("gopls")
		vim.lsp.enable("lua_ls")
		vim.lsp.enable("ocamllsp")
		vim.lsp.enable("pyright")
		vim.lsp.enable("rust_analyzer")
		vim.lsp.enable("zls")
	end,
}
