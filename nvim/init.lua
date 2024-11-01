local global = vim.g
local o = vim.opt
local map = vim.api.nvim_set_keymap

o.belloff = 'all'
o.shortmess = 'Ic'

vim.g.mapleader = ","

map("n", "N", "Nzz", {})
map("n", "n", "nzz", {})

map("i", "<C-h>", "<Left>", {})
map("i", "<C-j>", "<Down>", {})
map("i", "<C-k>", "<Up>", {})
map("i", "<C-l>", "<Right>", {})

map("i", "<C-a>", "<C-o>I", {})
map("i", "<C-e>", "<C-o>A", {})

map("i", "<C-v>", "<C-o>v", {})
map("i", "<C-S-v>", "<C-o>V", {})

map("n", ">>", ">>4l", {})
map("n", "<<", "<<4h", {})
map("i", "<C-,>", "<C-c>l<<i", {})
map("i", "<C-.>", "<C-c>l>>i", {})

map("i", "<C-s>", "<C-o>:w<CR>", {})

o.compatible = false
o.fileformats = "unix,mac"
o.encoding = "UTF-8"
o.fileencoding = "UTF-8"
o.fileencodings = "UTF-8"

o.backspace = "indent,eol,start"

o.autoread = true
o.autochdir = true

o.confirm = true

o.swapfile = false

o.wrap = false
o.linebreak = true

o.laststatus = 2
o.ruler = true
o.display = "lastline"

o.clipboard = "unnamedplus,unnamed"

o.number = true

o.termguicolors = true
o.syntax = "on"

o.hlsearch = true
o.smartcase = true
o.ignorecase = true
o.incsearch = true

o.wildmenu = true
o.wildoptions = "pum,fuzzy"
o.pumheight = 20
o.wildignore = "*.o,*.obj,*.bak,*.exe,*.swp,tags"

o.autoindent = true

o.cursorline = true
o.cursorlineopt = "number"

o.colorcolumn = "110"
o.textwidth = 110

o.expandtab = true
o.shiftwidth = 4
o.tabstop = 4

o.list = true
o.listchars = "tab:› ,nbsp:␣,trail:·,extends:…,precedes:…"
o.showbreak = "↪"

o.mouse = "a"
o.title = true
o.hidden = true
o.ttimeoutlen = 0

o.showcmd = true
o.showmatch = true

o.inccommand = "split"
o.splitright = true
o.splitbelow = true

o.virtualedit = "block"

o.errorbells = false
o.visualbell = false

o.undolevels = 1000
o.undofile = true

require("config.lazy")
