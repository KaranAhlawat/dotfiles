vim.opt.guicursor="i:block"

-- Vscode dark options
vim.g.vscode_style = "dark"
vim.g.vscode_italic_comment = 1

-- Set the colorscheme and lualine options
vim.cmd [[ colorscheme nord ]]

vim.g.tokyonight_style = "night"
vim.g.tokyonight_lualine_bold = true
vim.g.tokyonight_italic_keywords = false
vim.g.tokyonight_hide_inactive_statusline = true

require('lualine').setup {
   options = {
     -- theme = 'nord',
     section_separators = { left = "", right = "" },
     component_separators = { left = "", right = "" }
   },
   sections = {
     lualine_x = { 'encoding', 'filetype' }
   }
}

vim.o.laststatus = 2

-- Chadtree config
local chadtree_settings = {
  ["theme.icon_colour_set"] = "none",
  ["theme.icon_glyph_set"] = "devicons"
}

vim.api.nvim_set_var("chadtree_settings", chadtree_settings)

-- General config
vim.opt.number = true;

vim.opt.expandtab = true;
vim.opt_local.expandtab = true;

vim.opt.shiftwidth = 2;
vim.opt_local.shiftwidth = 2;

vim.opt.softtabstop = 2;
vim.opt_local.softtabstop = 2;

-- clipboard

vim.opt.clipboard = "unnamedplus"

-- Word wrap maybe
vim.opt.wrap = true;
vim.opt_local.wrap = true;

vim.opt.linebreak = true;
vim.opt_local.linebreak = true;

-- incremental live completion
vim.opt.inccommand = 'nosplit'

-- don't save when switcing buffers
vim.opt.hidden = true

-- enable mouse mode
vim.opt.mouse = 'a'

-- case insensitive search

vim.opt.ignorecase = true
vim.opt.smartcase = true

-- completopt set
vim.opt.completeopt = { "menu", "menuone", "noinsert" }
vim.g.completion_matching_strategy_list = { 'exact', 'substring', 'fuzzy' }
vim.opt.shortmess:append "c"
vim.opt_global.shortmess:remove "F"

-- signcolunm width in lsp mode

vim.opt.signcolumn = 'number'

-- Set splitting options

vim.opt.splitbelow = true
vim.opt.splitright = true

-- set whichkey timeout

vim.opt.timeout = true
vim.g.timeoutlen = 20

vim.g.markdown_fenced_languages = {
  "ts=typescript"
  }

-------------------------------- Treesitter --------------------------

require('nvim-treesitter.configs').setup {
  ensure_installed = { "lua", "c", "cpp", "rust", "elm", "java", "javascript", "typescript", "graphql", "elixir", "go", "scala" },
  highlight = { enable = true },
  incremental_selection = { enable = true },
  indent = { enable = true },
  textobjects = { enable = true }
}

--------------------------------- LSP --------------------------------

local nvim_lsp = require 'lspconfig'
local keymap = vim.api.nvim_set_keymap

local on_attach = function(_, bufnr)
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap = true, silent = true }

    keymap("n", "<leader>gd", ":lua vim.lsp.buf.definition()<CR>", opts)
    keymap("n", "<leader>gi", ":lua vim.lsp.buf.implementation()<CR>", opts)
    keymap("n", "<leader>gsh", ":lua vim.lsp.buf.signature_help()<CR>", opts)
    keymap("n", "<leader>grr", ":lua vim.lsp.buf.references()<CR>", opts)
    keymap("n", "<leader>grn", ":lua vim.lsp.buf.rename()<CR>", opts)
    keymap("n", "<leader>gh", ":lua vim.lsp.buf.hover()<CR>", opts)
    keymap("n", "<leader>ca", ":lua vim.lsp.buf.code_action()<CR>", opts)
    keymap("n", "<leader>gsd", ":lua vim.lsp.diagnostic.show_line_diagnostics(); vim.lsp.util.show_line_diagnostics()<CR>", opts)
    keymap("n", "<leader>gn", ":lua vim.lsp.diagnostic.goto_next()<CR>", opts)
    keymap("n", "<leader>gc", ":lua vim.lsp.buf.declaration()<CR>", opts)
    keymap("n", "<leader>gf", ":lua vim.lsp.buf.formatting()<CR>", opts)
  end

local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

local servers = { 'clangd', 'rust_analyzer', 'pyright', 'graphql', 'prismals', 'gopls' }
for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup {
      on_attach = on_attach,
      capabilities = capabilities,
      flags = {
        debounce_text_changes = 150,
      }
    }
end

-- setup elixir server
local utils = require 'lspconfig/util'

nvim_lsp.elixirls.setup {
  on_attach = on_attach,
  capabilities = capabilities,
  flags = {
    debounce_text_changes = 150
    },
  cmd = { "/home/karan/.elixir-ls/language_server.sh" },
  root_dir = utils.root_pattern("mix.exs", ".git", ".projectile")
}

nvim_lsp.tsserver.setup {
  on_attach = on_attach,
  capabilities = capabilities,
  flags = {
    debounce_text_changes = 150
  },
  single_file_support = true
}

-- setup denols

nvim_lsp.denols.setup {
  on_attach = on_attach,
  capabilities = capabilities,
  flags = {
    debounce_text_changes = 150
  },
  root_dir = utils.root_pattern("deps.ts")
}

-- Setup sumneko lua language server
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')

nvim_lsp.sumneko_lua.setup {
  cmd = { 'lua-language-server' },
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = {
        -- Tell the server of your lua version
        version = 'LuaJIT',
        -- Setup the lua path
        path = runtime_path
      },
      diagnostics = {
        globals = { 'vim' }
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file('', true),
        maxPreload = 2000,
        preloadFileSize = 1000,
      },
      telemetry = {
        enable = false,
      },
    },
  },
}

-- Coq nvim
-- vim.g.coq_settings = {
--   ["auto_start"] = 'shut-up',
--   ["xdg"] = true,
--   ["keymap.pre_select"] = true
-- }

-- LSP kind : icons in completion menu

local lspkind = require 'lspkind'
lspkind.init()

-- Nvim CMP setup

local cmp = require 'cmp'

cmp.setup {
  mapping = {
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Insert,
      select = true
    },

    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end
  },

  sources = {
    { name = "nvim_lua" },
    { name = "nvim_lsp" },
    { name = "luasnip" },
    { name = "path" },
    { name = "buffer", keyword_length = 5  },
  },

  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end
  },

  formatting = {
    format = lspkind.cmp_format {
      with_text = true,
      menu = {
        buffer = "[buf]",
        nvim_lsp = "[LSP]",
        nvim_lua = "[api]",
        path = "[path]",
        luasnip = "[snip]",
      }
    }
  },

  experimental = {
    native_menu = false,
    ghost_text = true
  }
}


-- Change lsp icons to actual icons
local signs = { Error = " ", Warn = " ", Hint = "", Info = "", other = "﫠" }

for sign, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. sign
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = '' })
  end

----------------------------- TELESCOPE ---------------------------------

require('telescope').setup {
  defaults = {
    file_ignore_patterns = { "node_modules" }
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case"
    }
  }
}

---------------------------- NVIM NOITFY ------------------------------------
require('notify').setup {
  stages = "fade",
  on_open = nil,
  on_close = nil,
  render = "default",
  timeout = 2500,
  background_colour = "Normal",
  minimum_width = 50,
  icons = {
    ERROR = "",
    WARN = "",
    INFO = "",
    DEBUG = "",
    TRACE = "✎",
  },
}
vim.notify = require('notify')

require('which-key').setup {}

require('nvim-tree').setup {}

vim.cmd [[
  autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif
]]
