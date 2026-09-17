local map = vim.keymap.set

-- Window navigation (Ctrl+hjkl, VS Code style)
map("n", "<C-h>", "<C-w>h", { desc = "Window left" })
map("n", "<C-j>", "<C-w>j", { desc = "Window down" })
map("n", "<C-k>", "<C-w>k", { desc = "Window up" })
map("n", "<C-l>", "<C-w>l", { desc = "Window right" })

-- Window resize with arrow keys
map("n", "<C-Up>", ":resize -2<CR>", { silent = true, desc = "Shrink height" })
map("n", "<C-Down>", ":resize +2<CR>", { silent = true, desc = "Grow height" })
map("n", "<C-Left>", ":vertical resize -2<CR>", { silent = true, desc = "Shrink width" })
map("n", "<C-Right>", ":vertical resize +2<CR>", { silent = true, desc = "Grow width" })

-- Clear search highlight on Escape
map("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Stay in Visual mode and preserve counts (e.g. `3>` shifts three times).
local function shift_visual(key)
	local count = vim.v.count1
	if vim.fn.mode():match("^[sS\19]$") then
		vim.cmd("normal! " .. vim.keycode("<C-g>"))
	end
	vim.cmd("normal! " .. count .. key)
	vim.cmd("normal! gv")
end

map("v", "<", function()
	shift_visual("<")
end, { desc = "Outdent" })
map("v", ">", function()
	shift_visual(">")
end, { desc = "Indent" })

-- Navigate wrapped lines naturally (useful in LaTeX)
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- Buffer navigation
map("n", "<S-h>", "<cmd>bprevious<CR>", { desc = "Prev buffer" })
map("n", "<S-l>", "<cmd>bnext<CR>", { desc = "Next buffer" })
map("n", "<leader>bd", "<cmd>bdelete<CR>", { desc = "Delete buffer" })

-- Save with Ctrl+S (VS Code muscle memory)
map({ "n", "i", "v" }, "<C-s>", "<Esc><cmd>w<CR>", { desc = "Save" })

-- Plugin managers
map("n", "<leader>L", "<cmd>Lazy<CR>", { desc = "Lazy (plugins)" })
map("n", "<leader>M", "<cmd>Mason<CR>", { desc = "Mason (LSPs)" })

-- Diagnostics (LSP errors/warnings)
map("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Diagnostic float" })

-- Set this tab's project root (tab cwd). Windows still auto-:lcd to their
-- file's dir (AutoLcd in lua/config/autocmds.lua) on the next buffer enter.
map("n", "<leader>cr", function()
	local root = vim.fs.root(0, { ".git", "build.zig", "gleam.toml", ".luarc.json", "Makefile" })
	if not root then
		return vim.notify("No project root found", vim.log.levels.WARN)
	end
	vim.cmd.tcd(vim.fn.fnameescape(root))
	vim.notify("tcd -> " .. vim.fn.fnamemodify(root, ":~"))
end, { desc = "cd to project root (tab)" })
map("n", "[d", function()
	vim.diagnostic.jump({ count = -1, float = true })
end, { desc = "Prev diagnostic" })
map("n", "]d", function()
	vim.diagnostic.jump({ count = 1, float = true })
end, { desc = "Next diagnostic" })

-- Center search results
map("n", "n", "nzz")
map("n", "N", "Nzz")

-- Insert-mode line home/end.
-- NOTE: blink.cmp's super-tab preset binds <C-e> to "cancel"; it is explicitly
-- disabled in lua/plugins/completion.lua so these win even with the menu open.
map("i", "<C-a>", "<C-o>I", { silent = true })
map("i", "<C-e>", "<C-o>A", { silent = true })

-- Insert-mode movement.
-- NOTE: <C-k> is likewise freed from blink (signature help moved to <C-g>) and
-- from the LSP on_attach block in lua/plugins/lsp.lua.
map("i", "<C-k>", "<Up>", { silent = true })
map("i", "<C-j>", "<Down>", { silent = true })
map("i", "<C-h>", "<Left>", { silent = true })
map("i", "<C-l>", "<Right>", { silent = true })

vim.cmd([[
	cnoreabbrev <expr> e
		\ getcmdtype() ==# ':' && getcmdline() ==# 'e'
		\ ? 'tabedit'
		\ : 'e'

	cnoreabbrev <expr> edit
		\ getcmdtype() ==# ':' && getcmdline() ==# 'edit'
		\ ? 'tabedit'
		\ : 'edit'
]])
