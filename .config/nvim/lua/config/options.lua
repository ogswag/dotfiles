local opt = vim.opt

-- Line numbers
opt.number = true
opt.relativenumber = true

-- Indentation (global default; per-filetype overrides in autocmds.lua)
opt.expandtab = true
opt.shiftwidth = 4 -- Zig standard; Gleam overridden to 2 in autocmds
opt.tabstop = 4
opt.softtabstop = 4
opt.smartindent = true

-- Search
opt.ignorecase = true
opt.smartcase = true -- case-sensitive when query has uppercase
opt.hlsearch = true

-- Appearance
opt.termguicolors = true
opt.cursorline = true
opt.mouse = "a" -- enables mouse clicks everywhere, including the fold column
opt.foldcolumn = "1" -- shows ▶/▼ markers to the left of line numbers; click to fold/unfold
opt.signcolumn = "yes" -- always show; avoids layout jitter on LSP load
-- opt.colorcolumn = "100"
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.wrap = false
opt.list = true
opt.listchars = { tab = "› ", trail = "·", nbsp = "␣" }
opt.fillchars = "eob: ,fold: ,foldopen:,foldsep: ,foldinner: ,foldclose:"
opt.autochdir = true
opt.showmode = false -- lualine shows mode; don't duplicate in cmdline

-- Splits
opt.splitright = true
opt.splitbelow = true

-- Performance
opt.updatetime = 200 -- faster CursorHold (used by diagnostics, gitsigns)
opt.timeoutlen = 300 -- faster which-key popup

-- File handling
opt.undofile = true -- persistent undo across sessions
opt.swapfile = false
opt.backup = false

-- Clipboard: use macOS system clipboard for all y/p operations
opt.clipboard = "unnamedplus"

-- Folding via treesitter (folds disabled by default; use za/zR/zM)
opt.foldmethod = "expr"
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
opt.foldenable = true -- was false
opt.foldlevel = 99

-- Language
vim.opt.langmap =
	"ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯЖ;ABCDEFGHIJKLMNOPQRSTUVWXYZ:,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz"
-- -- switch between layouts using Ctrl+^ in insert mode
-- vim.opt.keymap = "russian-jcukenwin"
-- vim.opt.iminsert = 0 -- Start with English input
-- vim.opt.imsearch = 0 -- Start search with English
