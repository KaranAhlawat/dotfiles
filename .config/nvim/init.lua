local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

require("lazy").setup({
  {
    "Mofiqul/adwaita.nvim",
    lazy = false,
    priority = 1000,
    
    -- configure and set on startup
    config = function()
        vim.g.adwaita_darker = false
        vim.g.adwaita_disable_cursorline = true -- to disable cursorline
        vim.g.adwaita_transparent = true        -- makes the background transparent
        vim.cmd.colorscheme 'adwaita'
    end
  }
})
