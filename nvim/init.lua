--  ██▒   █▓ ██▓ ███▄ ▄███▓ ██▀███   ▄████▄
-- ▓██░   █▒▓██▒▓██▒▀█▀ ██▒▓██ ▒ ██▒▒██▀ ▀█
--  ▓██  █▒░▒██▒▓██    ▓██░▓██ ░▄█ ▒▒▓█    ▄
--   ▒██ █░░░██░▒██    ▒██ ▒██▀▀█▄  ▒▓▓▄ ▄██▒
--    ▒▀█░  ░██░▒██▒   ░██▒░██▓ ▒██▒▒ ▓███▀ ░
--    ░ ▐░  ░▓  ░ ▒░   ░  ░░ ▒▓ ░▒▓░░ ░▒ ▒  ░
--    ░ ░░   ▒ ░░  ░   ░  ░  ░▒ ░ ▒░  ░  ▒
--      ░░   ▒ ░░      ░     ░░   ░ ░
--       ░   ░         ░      ░     ░ ░
--      ░                           ░

-- ---------- GENERAL SETTINGS  ----------
vim.cmd('filetype plugin indent on')

vim.opt.ttimeout = true
vim.opt.ttimeoutlen = 25

vim.opt.hidden = true
vim.opt.confirm = true

vim.opt.scrolloff = 0

vim.opt.langmenu = 'en_US.UTF-8'
vim.cmd('language messages en_US.UTF-8')

vim.opt.compatible = false
vim.opt.fileformats = 'unix,mac'
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'
vim.opt.fileencodings = 'utf-8'
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.title = true
vim.opt.swapfile = false
vim.opt.spell = false
vim.opt.spelllang = 'en_gb,ru_yo'

vim.opt.wrap = false
vim.opt.breakindent = true
vim.opt.breakindentopt = 'sbr,list:-1'
vim.opt.linebreak = true
vim.opt.joinspaces = false
vim.opt.list = true
vim.opt.listchars = {tab = '› ', nbsp = '␣', trail = '·', extends = '…', precedes = '…'}
vim.opt.showbreak = '↪'
vim.opt.fillchars = {fold = ' ', vert = '│'}
vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 3
vim.opt.startofline = false
vim.opt.virtualedit = 'block'
vim.opt.display = 'lastline'
vim.opt.laststatus = 2
vim.opt.ruler = true
vim.opt.belloff = 'all'
vim.opt.shortmess:append('Ic')

vim.opt.colorcolumn = '110'
vim.opt.textwidth = 110

vim.cmd('syntax on')
vim.opt.background = 'dark'
vim.cmd([[
augroup colorscheme_change
    au!
    au ColorScheme habamax hi Normal ctermbg=NONE guibg=NONE
    au ColorScheme habamax hi Comment ctermfg=95 guifg=NONE
    au ColorScheme habamax hi SpellBad cterm=underline ctermfg=124 ctermbg=NONE guifg=#af0000 gui=underline guibg=NONE
    au ColorScheme lunaperche hi Normal ctermbg=NONE guibg=NONE
    au ColorScheme sorbet hi Normal ctermbg=NONE guibg=NONE
    au ColorScheme wildcharm hi Normal ctermbg=NONE guibg=NONE
    au ColorScheme zellner hi Normal ctermbg=NONE guibg=NONE
augroup END
]])

vim.cmd('colorscheme lunaperche')
vim.opt.showmatch = true

if vim.fn.has('gui_running') == 1 then
    vim.opt.guifont = 'Input Mono:h13'
    vim.opt.guioptions:remove('r')
    vim.opt.guioptions:remove('l')
    vim.opt.guioptions:remove('L')
end

vim.opt.cursorline = true
vim.opt.cursorlineopt = 'number'
vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Centre search results on screen
vim.keymap.set('n', 'n', 'nzz', {noremap = true})
vim.keymap.set('n', 'N', 'Nzz', {noremap = true})

vim.keymap.set('n', 'j', 'gj', {noremap = true})
vim.keymap.set('n', 'k', 'gk', {noremap = true})

vim.keymap.set('n', '<C-q>', '<End>a;<C-c>j', {noremap = true, silent = true})
vim.keymap.set('i', '<C-q>', '<End>;<Down>', {noremap = true, silent = true})

vim.keymap.set('i', '<C-a>', '<C-o>I', {noremap = true, silent = true})
vim.keymap.set('i', '<C-e>', '<C-o>A', {noremap = true, silent = true})

vim.keymap.set('i', '<C-k>', '<up>', {noremap = true, silent = true})
vim.keymap.set('i', '<C-j>', '<down>', {noremap = true, silent = true})
vim.keymap.set('i', '<C-h>', '<left>', {noremap = true, silent = true})
vim.keymap.set('i', '<C-l>', '<right>', {noremap = true, silent = true})

vim.opt.autoindent = true
vim.opt.shiftwidth = 4
vim.opt.softtabstop = -1
vim.opt.expandtab = true

vim.cmd([[
autocmd BufNewFile,BufRead,BufWinEnter,FileType,OptionSet * set formatoptions-=t formatoptions-=l formatoptions-=c formatoptions-=r formatoptions-=o formatoptions-=q
autocmd BufNewFile,BufRead,BufWinEnter,FileType,OptionSet * setlocal formatoptions-=t formatoptions-=l formatoptions-=c formatoptions-=r formatoptions-=o formatoptions-=q
]])

vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.smarttab = true

vim.opt.autochdir = true
vim.opt.autoread = true

vim.opt.backspace = 'indent,eol,start'

vim.opt.mouse = 'a'

vim.opt.wildmenu = true
vim.opt.wildoptions = 'pum,fuzzy'
vim.opt.pumheight = 20
vim.opt.wildignore = '*.o,*.obj,*.bak,*.exe,*.swp,tags,*.out'

vim.opt.undolevels = 1000
-- vim.opt.undofile = true
vim.opt.viminfo = "'200,<500,s32"
