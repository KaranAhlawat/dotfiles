local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
  execute "packadd packer.nvim"
end

local packer_ok, packer = pcall(require, "packer")
if not packer_ok then
  return
end

packer.init {
  profile = { enabled = true },
  git = { clone_timeout = 300 },
  display = {
    open_fn = function ()
      return require("packer.util").float { border = "single" }
    end
  }
}

return require('packer').startup(function(use)
	-- Packer can manage itself
	use 'wbthomason/packer.nvim'

	-- Colorschemes
	use 'folke/tokyonight.nvim'
        use 'TarunDaCoder/citylights.nvim'
        use 'morhetz/gruvbox'
        use {
          'tjdevries/gruvbuddy.nvim',
          requires = { 'tjdevries/colorbuddy.vim' }
      }

	-- Treesitter
	use 'nvim-treesitter/nvim-treesitter'
        use 'nvim-treesitter/nvim-treesitter-textobjects'
        use 'pantharshit00/vim-prisma'

	-- LSP
	use 'neovim/nvim-lspconfig'

        -- Snippets
        use 'L3MON4D3/LuaSnip'

	-- Lexima
	use 'cohama/lexima.vim'

	-- Lualine
	use {
	    'hoob3rt/lualine.nvim',
	    requires = {'kyazdani42/nvim-web-devicons', opt = true}
	}

        -- nvim tree plugin
        use {
          'nvim-telescope/telescope.nvim',
           requires = { 'nvim-lua/plenary.nvim' }
        }

        use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

        -- vim kitty integration
        use 'fladson/vim-kitty'

        -- comments
        use 'tpope/vim-commentary'

        -- nvim-cmp
        use 'hrsh7th/nvim-cmp'
        use 'hrsh7th/cmp-buffer' use 'hrsh7th/cmp-path'
        use 'hrsh7th/cmp-nvim-lua'
        use 'hrsh7th/cmp-nvim-lsp'
        use 'saadparwaiz1/cmp_luasnip'

        -- chad tree & coq_nvim
        use 'ms-jpq/chadtree'
        -- use 'ms-jpq/coq_nvim'
        -- use 'ms-jpq/coq.artifacts'
        
        -- Nvimtree
        use { 'kyazdani42/nvim-tree.lua',
          config = function()
            require'nvim-tree'.setup{
              auto_close = true,
            }
          end
        }

        -- Pictograms for lsp
        use 'onsails/lspkind-nvim'

        -- Indentaion (even on blank lines)
        use 'lukas-reineke/indent-blankline.nvim'

        -- which key
        -- Lua
        use {
          "folke/which-key.nvim",
          config = function()
            require("which-key").setup {
            }
          end
        }
end)

