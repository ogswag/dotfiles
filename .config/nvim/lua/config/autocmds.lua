local function augroup(name)
	return vim.api.nvim_create_augroup(name, { clear = true })
end
local autocmd = vim.api.nvim_create_autocmd

-- Trim trailing whitespace on save
-- Skip markdown (trailing spaces are meaningful: they produce <br>)
autocmd("BufWritePre", {
	group = augroup("TrimTrailingWhitespace"),
	callback = function()
		if vim.bo.filetype ~= "markdown" then
			-- winsaveview/winrestview restores topline too, not just the cursor, so a
			-- scrolled buffer does not jump on save. keeppatterns stops the trim regex
			-- from becoming your last search-history entry.
			local view = vim.fn.winsaveview()
			vim.cmd([[keeppatterns %s/\s\+$//e]])
			vim.fn.winrestview(view)
		end
	end,
})

-- Flash yanked region (visual feedback for y)
autocmd("TextYankPost", {
	group = augroup("YankHighlight"),
	callback = function()
		vim.hl.on_yank({ higroup = "Visual", timeout = 200 })
	end,
})

-- Equalize splits on terminal resize
autocmd("VimResized", {
	group = augroup("ResizeSplits"),
	callback = function()
		vim.cmd("wincmd =")
	end,
})

-- Window-local 'autochdir': cd each window into its file's directory.
-- Only real file buffers (buftype == "") trigger it, so entering terminals,
-- help or quickfix never moves the cwd. Scope is "window" (:lcd), so the tab
-- cwd stays the project root (set with <leader>cr).
autocmd("BufEnter", {
	group = augroup("AutoLcd"),
	callback = function(ev)
		local name = vim.api.nvim_buf_get_name(ev.buf)
		if vim.bo[ev.buf].buftype ~= "" or name == "" then
			return
		end
		local dir = vim.fs.dirname(name)
		if dir ~= vim.fn.getcwd(0) and vim.fn.isdirectory(dir) == 1 then
			vim.fn.chdir(dir, "window")
		end
	end,
})

-- Return to last cursor position when reopening a file
autocmd("BufReadPost", {
	group = augroup("LastCursorPos"),
	callback = function()
		local mark = vim.api.nvim_buf_get_mark(0, '"')
		local lcount = vim.api.nvim_buf_line_count(0)
		if mark[1] > 0 and mark[1] <= lcount then
			pcall(vim.api.nvim_win_set_cursor, 0, mark)
		end
	end,
})

-- Gleam: 2-space indentation (official Gleam style guide)
autocmd("FileType", {
	group = augroup("GleamIndent"),
	pattern = "gleam",
	callback = function()
		vim.opt_local.shiftwidth = 2
		vim.opt_local.tabstop = 2
		vim.opt_local.softtabstop = 2
	end,
})

-- LaTeX: sensible editing defaults.
-- NOTE: filetype `context` is deliberately NOT handled here; ConTeXt buffers
-- are set up in after/ftplugin/context.lua with their compile/view mappings.
autocmd("FileType", {
	group = augroup("LaTeXSettings"),
	pattern = { "tex", "latex", "plaintex" },
	callback = function()
		vim.opt_local.wrap = true
		vim.opt_local.spell = true
		vim.opt_local.spelllang = "en_us,ru_yo"
		-- Don't trim LaTeX trailing whitespace at the file level;
		-- trailing whitespace in tex sources is usually harmless and
		-- latexindent will clean it up on format anyway.
	end,
})

-- Ensure '#' comments for hash-style config files so mini.comment (gc/gcc)
-- works. Plain `conf` filetype and ghostty's config don't always set this.
autocmd("FileType", {
	group = augroup("HashCommentString"),
	pattern = { "conf", "config", "gitconfig", "ghostty", "tmux", "fish" },
	callback = function()
		vim.bo.commentstring = "# %s"
	end,
})

-- ghostty's config file has no extension, so pin its filetype + commentstring.
autocmd({ "BufRead", "BufNewFile" }, {
	group = augroup("GhosttyConfig"),
	pattern = { "*/ghostty/config", "*/ghostty/themes/*" },
	callback = function()
		vim.bo.filetype = "conf"
		vim.bo.commentstring = "# %s"
	end,
})

-- auto create missing directories
vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*",
	callback = function()
		local dir = vim.fn.expand("<afile>:p:h")
		if dir ~= "" and vim.fn.isdirectory(dir) == 0 then
			vim.fn.mkdir(dir, "p")
		end
	end,
})

-- Stop auto comment-leader insertion and auto-wrapping everywhere
autocmd({ "BufWinEnter", "FileType", "OptionSet" }, {
	group = augroup("FormatOptions"),
	callback = function()
		vim.opt_local.formatoptions:remove({ "t", "l", "c", "r", "o", "q" })
	end,
})
