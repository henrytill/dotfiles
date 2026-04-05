-- conform

require("conform").setup({
  formatters_by_ft = {
    go = { "gofmt" },
    haskell = { "fourmolu" },
    lua = { "stylua" },
    nix = { "nixfmt" },
    perl = { "perltidy" },
    python = { "black" },
  },
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
})

-- lazydev

require("lazydev").setup({
  library = {
    -- Load luvit types when the `vim.uv` word is found
    { path = "${3rd}/luv/library", words = { "vim%.uv" } },
  },
})

-- lean

vim.api.nvim_create_autocmd({ "BufReadPre", "BufNewFile" }, {
  pattern = "*.lean",
  once = true,
  callback = function()
    require("lean").setup({
      mappings = true,

      lsp = {
        init_options = {
          editDelay = 0,
          hasWidgets = true,
        },
      },

      infoview = {
        autoopen = true,
        width = 50,
        height = 20,
      },
    })
  end,
})

-- lspconfig

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
vim.lsp.enable("nim_langserver")
vim.lsp.enable("ocamllsp")
vim.lsp.enable("pyright")
vim.lsp.enable("rust_analyzer")
vim.lsp.enable("yamlls")
vim.lsp.enable("zls")

-- ocaml.nvim

require("ocaml").setup({
  keymaps = {},
})

-- telescope

local telescope = require("telescope")

do
  local actions = require("telescope.actions")
  telescope.setup({
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
end

do
  local builtin = require("telescope.builtin")

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
      vim.keymap.set("n", "<leader>ss", builtin.lsp_document_symbols, {
        buffer = args.buf,
        desc = "Document symbols",
      })

      vim.keymap.set("n", "<leader>sS", builtin.lsp_workspace_symbols, {
        buffer = args.buf,
        desc = "Workspace symbols",
      })

      vim.keymap.set("n", "<leader>sd", builtin.diagnostics, {
        buffer = args.buf,
        desc = "Diagnostics",
      })
    end,
  })
end

-- telescope_hoogle

telescope.load_extension("hoogle")

local function hoogle()
  telescope.extensions.hoogle.hoogle({ default_text = vim.fn.expand("<cword>") })
end

vim.keymap.set("n", "<leader>fH", hoogle, { desc = "Telescope hoogle" })

-- which-key

require("which-key").setup({
  delay = function(ctx)
    return ctx.plugin and 0 or 1000
  end,
})

local function buffer_local_keymaps()
  require("which-key").show({ global = false })
end

vim.keymap.set("n", "<leader>?", buffer_local_keymaps, { desc = "Buffer Local Keymaps (which-key)" })

-- misc

local function run_ghc_tags()
  if vim.fn.executable("ghc-tags") == 0 then
    return
  end
  vim.fn.jobstart("ghc-tags -c 2>/dev/null", { detach = true })
end

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "*.hs",
  callback = run_ghc_tags,
  desc = "Generate ctags for Haskell files",
})

local function goto_prev()
  vim.diagnostic.jump({ count = -1, float = true })
end

local function goto_next()
  vim.diagnostic.jump({ count = 1, float = true })
end

vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Show diagnostic float" })
vim.keymap.set("n", "[d", goto_prev, { desc = "Previous diagnostic" })
vim.keymap.set("n", "]d", goto_next, { desc = "Next diagnostic" })
