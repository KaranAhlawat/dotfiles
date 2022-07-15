local keymap = vim.keymap.set
local opts = { silent = true }

keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

opts = { silent = true, noremap = true }

-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Clear highlights
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)

-- Telescope
keymap("n", "<leader>ff", ":Telescope find_files<CR>", opts)
keymap("n", "<leader>ft", ":Telescope live_grep<CR>", opts)
keymap("n", "<leader>fp", ":Telescope projects<CR>", opts)
keymap("n", "<leader>fb", ":Telescope buffers<CR>", opts)

-- Comment
keymap("n", "<leader>/", "<cmd>lua require('Comment.api').toggle_current_linewise()<CR>", opts)
keymap("x", "<leader>/", '<ESC><CMD>lua require("Comment.api").toggle_linewise_op(vim.fn.visualmode())<CR>')

-- Barbar
keymap("n", "<leader>bj", "<cmd>BufferPrevious<CR>", opts)
keymap("n", "<leader>bk", "<cmd>BufferNext<CR>", opts)

-- Goto buffer in position...
keymap("n", "<leader>b1", "<cmd>BufferGoto 1<CR>", opts)
keymap("n", "<leader>b2", "<cmd>BufferGoto 2<CR>", opts)
keymap("n", "<leader>b3", "<cmd>BufferGoto 3<CR>", opts)
keymap("n", "<leader>b4", "<cmd>BufferGoto 4<CR>", opts)
keymap("n", "<leader>b5", "<cmd>BufferGoto 5<CR>", opts)
keymap("n", "<leader>b6", "<cmd>BufferGoto 6<CR>", opts)
keymap("n", "<leader>b7", "<cmd>BufferGoto 7<CR>", opts)
keymap("n", "<leader>b8", "<cmd>BufferGoto 8<CR>", opts)
keymap("n", "<leader>b9", "<cmd>BufferGoto 9<CR>", opts)
keymap("n", "<leader>b0", "<cmd>BufferLast<CR>", opts)

-- Close buffer
keymap("n", "<leader>bd", "<cmd>BufferClose<CR>", opts)
