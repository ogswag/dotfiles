local global = vim.g
local o = vim.opt
local map = vim.api.nvim_set_keymap

vim.g.mapleader = ","

map("n", "N", "Nzz", {})
map("n", "n", "nzz", {})

map("i", "<C-h>", "<Left>", {})
map("i", "<C-j>", "<Down>", {})
map("i", "<C-k>", "<Up>", {})
map("i", "<C-l>", "<Right>", {})

map("i", "<C-a>", "<esc>I", {})
map("i", "<C-e>", "<esc>A", {})

map("i", "<C-w>", "<esc>bi", {})
map("i", "<C-e>", "<esc>lwi", {})
map("i", "<C-S-w>", "<esc>Bi", {})
map("i", "<C-S-e>", "<esc>lWi", {})
map("i", "<C-p>", "<esc>l}{{ji", {})
map("i", "<C-S-p>", "<esc>}ji", {})

map("i", "<C-v>", "<esc>v", {})
map("i", "<C-S-v>", "<esc>V", {})

map("i", "<C-,>", "<esc><<hi", {})
map("i", "<C-.>", "<esc>>>3li", {})

map("i", "<C-x>", "<esc>ldiwi", {})

map("i", "<C-s>", "<esc>:w<CR>li", {})

map("i", "<C-z>", "<esc>:undo<CR>li", {})
map("i", "<C-S-z>", "<esc>:redo<CR>li", {})

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
o.showbreak = "↳↳"

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
-- o.wildmode = "list:longest,full"

o.autoindent = true

o.cursorline = true
o.cursorlineopt = "number"

o.colorcolumn = "80"
o.textwidth = 80

o.expandtab = true
o.shiftwidth = 2
o.tabstop = 2

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

o.guifont = "PragmataPro Liga:h16"

require("config.lazy")
