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
			local pos = vim.api.nvim_win_get_cursor(0)
			vim.cmd([[%s/\s\+$//e]])
			vim.api.nvim_win_set_cursor(0, pos) -- restore cursor after substitution
		end
	end,
})

-- Flash yanked region (visual feedback for y)
autocmd("TextYankPost", {
	group = augroup("YankHighlight"),
	callback = function()
		vim.highlight.on_yank({ higroup = "Visual", timeout = 200 })
	end,
})

-- Equalize splits on terminal resize
autocmd("VimResized", {
	group = augroup("ResizeSplits"),
	callback = function()
		vim.cmd("wincmd =")
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

-- LaTeX: sensible editing defaults
autocmd("FileType", {
	group = augroup("LaTeXSettings"),
	pattern = { "tex", "latex", "plaintex" },
	callback = function()
		vim.opt_local.wrap = true
		vim.opt_local.spell = true
		vim.opt_local.spelllang = "en_us,ru_yo"
		vim.opt_local.textwidth = 100
		vim.opt_local.colorcolumn = "100"
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
