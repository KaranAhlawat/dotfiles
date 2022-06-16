local keymap = vim.api.nvim_set_keymap

keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Geenral keymaps
keymap("n", "Y", "y$", { noremap = true, silent = true })
keymap("n", "<leader><TAB>", ":bn<CR>", { noremap = true, silent = true })
keymap("n", "<leader>bd", ":bd<CR>", { noremap = true, silent = true })

-- Chadtree keybinds
keymap("n", "<leader>v", ":NvimTreeToggle<CR>", { noremap = true, silent = true })

--Remap for dealing with word wrap
keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

-- Telescope keybindings

local opts = { noremap = true, silent = true }

local telebuiltin = "<cmd>lua require('telescope.builtin')."
local telethemes = "(require('telescope.themes').get_dropdown({}))"

keymap("n", "<leader>ff", telebuiltin.."find_files"..telethemes.."<CR>", opts)
keymap("n", "<leader>fg", telebuiltin.."live_grep"..telethemes.."<CR>", opts)
keymap("n", "<leader>fb", telebuiltin.."buffers"..telethemes.."<CR>", opts)
keymap("n", "<leader>fh", telebuiltin.."help_tags"..telethemes.."<CR>", opts)
keymap("n", "<leader>fo", telebuiltin.."oldfiles"..telethemes.."<CR>", opts)
