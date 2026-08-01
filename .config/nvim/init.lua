-- Leader keys must be set BEFORE lazy.nvim loads (lazy uses them for its own UI)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\" -- vimtex uses \ll, \lv, etc.

-- Bootstrap lazy.nvim (plugin manager)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
	local out = vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"--branch=stable",
		"https://github.com/folke/lazy.nvim.git",
		lazypath,
	})
	if vim.v.shell_error ~= 0 then
		error("Failed to clone lazy.nvim:\n" .. out)
	end
end
vim.opt.rtp:prepend(lazypath)

-- Load core config (order matters: options first, then keymaps, then autocmds)
require("config.options")
require("config.keymaps")
require("config.autocmds")

-- Load all plugin specs from lua/plugins/*.lua
require("lazy").setup("plugins", {
	checker = { enabled = true, notify = false }, -- silent update checks
	change_detection = { notify = false },
})
