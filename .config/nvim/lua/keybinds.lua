local keymap = vim.keymap.set
local opts = { silent = true }

keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- Clear highlights
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
