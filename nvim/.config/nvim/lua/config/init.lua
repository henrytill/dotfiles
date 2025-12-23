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

-- treesitter

require("nvim-treesitter.configs").setup({
  -- Note: parsers are installed via Nix, not ensure_installed
  ensure_installed = {},

  sync_install = false,
  auto_install = false,
  ignore_install = {},
  modules = {},

  highlight = {
    enable = false,
  },

  incremental_selection = {
    enable = false,
  },

  indent = {
    enable = true,
  },
})

local c_folds = [[
; Function bodies (not the signature)
(function_definition
  body: (compound_statement) @fold)

; Control flow statement bodies (not the condition/header)
(if_statement
  consequence: (compound_statement) @fold)

(else_clause
  (compound_statement) @fold)

(for_statement
  body: (compound_statement) @fold)

(while_statement
  body: (compound_statement) @fold)

(do_statement
  body: (compound_statement) @fold)

(switch_statement
  body: (compound_statement) @fold)

; Struct and enum bodies (not the declaration)
(struct_specifier
  body: (field_declaration_list) @fold)

(enum_specifier
  body: (enumerator_list) @fold)

; These fold the entire construct (no separate body to target)
[
  (case_statement)
  (comment)
  (preproc_if)
  (preproc_elif)
  (preproc_else)
  (preproc_ifdef)
  (preproc_function_def)
  (initializer_list)
  (gnu_asm_expression)
  (preproc_include)+
] @fold

; Nested compound statements
(compound_statement
  (compound_statement) @fold)
]]

-- Custom C fold queries - fold bodies only, keeping declarations/conditions visible
vim.treesitter.query.set("c", "folds", c_folds)
vim.treesitter.query.set("cpp", "folds", c_folds)

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "c", "cpp", "go", "rust", "lua", "ocaml", "python", "haskell", "perl" },
  callback = function()
    vim.opt_local.foldmethod = "expr"
    vim.opt_local.foldexpr = "v:lua.vim.treesitter.foldexpr()"
    vim.opt_local.foldlevel = 99
  end,
})

-- treesitter-textobjects

require("nvim-treesitter-textobjects").init()

local keys = {
  goto_next_start = {
    ["]f"] = "@function.outer",
    ["]c"] = "@class.outer",
    ["]a"] = "@parameter.inner",
  },
  goto_next_end = {
    ["]F"] = "@function.outer",
    ["]C"] = "@class.outer",
    ["]A"] = "@parameter.inner",
  },
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

vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("treesitter_textobjects", { clear = true }),
  callback = function(ev)
    attach(ev.buf)
  end,
})

vim.tbl_map(attach, vim.api.nvim_list_bufs())

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
