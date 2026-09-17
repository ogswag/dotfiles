local bufnr = vim.api.nvim_get_current_buf()

vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.spell = true
vim.opt_local.spelllang = "en_us,ru_yo"
vim.opt_local.textwidth = 0

vim.b[bufnr].undo_ftplugin = (vim.b[bufnr].undo_ftplugin or "")
	.. " | setlocal wrap< linebreak< spell< spelllang< textwidth<"
