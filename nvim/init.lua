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
vim.cmd("filetype plugin indent on")

vim.opt.ttimeout = true
vim.opt.ttimeoutlen = 25

vim.opt.hidden = true
vim.opt.confirm = true

vim.opt.scrolloff = 0

vim.opt.langmenu = "en_US.UTF-8"
vim.cmd("language messages en_US.UTF-8")

vim.opt.compatible = false
vim.opt.fileformats = "unix,mac"
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"
vim.opt.fileencodings = "utf-8"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.title = true
vim.opt.swapfile = false
vim.opt.spell = false
vim.opt.spelllang = "en_us,ru_yo"

vim.opt.wrap = false
vim.opt.breakindent = true
vim.opt.breakindentopt = "sbr,list:-1"
vim.opt.linebreak = true
vim.opt.joinspaces = false
vim.opt.list = true
vim.opt.listchars = { tab = "› ", nbsp = "␣", trail = "·", extends = "…", precedes = "…" }
vim.opt.showbreak = "↪"
vim.opt.fillchars = { fold = " ", vert = "│" }
vim.opt.sidescroll = 1
vim.opt.sidescrolloff = 3
vim.opt.startofline = false
vim.opt.virtualedit = "block"
vim.opt.display = "lastline"
vim.opt.laststatus = 2
vim.opt.ruler = true
vim.opt.belloff = "all"
vim.opt.shortmess:append("Ic")

vim.opt.colorcolumn = "110"
vim.opt.textwidth = 110

vim.cmd("syntax on")
vim.cmd([[
augroup colorscheme_change
    au!
    au ColorScheme default hi Normal ctermbg=NONE guibg=NONE
    au ColorScheme habamax.nvim hi Normal ctermbg=NONE guibg=NONE
augroup END
]])
-- Enable transparent background if supported by colorscheme
-- vim.api.nvim_create_autocmd("ColorScheme", {
--   pattern = "*",
--   callback = function()
--     pcall(vim.cmd, "highlight Normal guibg=none ctermbg=none")
--     pcall(vim.cmd, "highlight NonText guibg=none ctermbg=none")
--     pcall(vim.cmd, "highlight LineNr guibg=none ctermbg=none")
--     pcall(vim.cmd, "highlight SignColumn guibg=none ctermbg=none")
--   end,
-- })

vim.opt.showmatch = true

if vim.fn.has("gui_running") == 1 then
    vim.opt.guifont = "Input Mono:h13"
    vim.opt.guioptions:remove("r")
    vim.opt.guioptions:remove("l")
    vim.opt.guioptions:remove("L")
end

vim.opt.cursorline = true
vim.opt.cursorlineopt = "number"
vim.opt.number = true

vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Centre search results on screen
vim.keymap.set("n", "n", "nzz", { noremap = true })
vim.keymap.set("n", "N", "Nzz", { noremap = true })

vim.keymap.set("n", "j", "gj", { noremap = true })
vim.keymap.set("n", "k", "gk", { noremap = true })

vim.keymap.set("n", "<C-q>", "<End>a;<C-c>j", { noremap = true, silent = true })
vim.keymap.set("i", "<C-q>", "<End>;<Down>", { noremap = true, silent = true })

vim.keymap.set("i", "<C-a>", "<C-o>I", { noremap = true, silent = true })
vim.keymap.set("i", "<C-e>", "<C-o>A", { noremap = true, silent = true })

vim.keymap.set("i", "<C-k>", "<up>", { noremap = true, silent = true })
vim.keymap.set("i", "<C-j>", "<down>", { noremap = true, silent = true })
vim.keymap.set("i", "<C-h>", "<left>", { noremap = true, silent = true })
vim.keymap.set("i", "<C-l>", "<right>", { noremap = true, silent = true })

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

vim.opt.backspace = "indent,eol,start"

vim.opt.mouse = "a"

vim.opt.wildmenu = true
vim.opt.wildoptions = "pum,fuzzy"
vim.opt.pumheight = 20
vim.opt.wildignore = "*.o,*.obj,*.bak,*.exe,*.swp,tags,*.out"

vim.opt.undolevels = 1000
vim.opt.undofile = true
vim.opt.viminfo = "'200,<500,s32"

-- Set up lazy.nvim plugin manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- Set up plugins
require("lazy").setup({
    -- Colorschemes with both light and dark variants
    {
        "khoido2003/classic_monokai.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            -- Default configuration
            require("classic_monokai").setup()
        end,
    },

    {
        "miikanissi/modus-themes.nvim",
        priority = 1000,
        config = function()
            require("modus-themes").setup({
                variant = "tinted",
            })
        end,
    },

    -- Auto dark/light mode detection
    {
        "f-person/auto-dark-mode.nvim",
        config = function()
            local auto_dark_mode = require("auto-dark-mode")

            auto_dark_mode.setup({
                update_interval = 1000, -- ms
                set_dark_mode = function()
                    vim.api.nvim_set_option("background", "dark")
                    -- Choose your preferred dark colorscheme here
                    vim.cmd.colorscheme("default") -- alternatives: tokyonight, onedark, gruvbox-material
                end,
                set_light_mode = function()
                    vim.api.nvim_set_option("background", "light")
                    -- Choose your preferred light colorscheme here
                    vim.cmd.colorscheme("modus_operandi") -- alternatives: tokyonight-day, gruvbox-material-light
                end,
            })

            auto_dark_mode.init()
        end,
    },

    -- Quality of life plugins
    { "tpope/vim-sleuth" }, -- Detect tabstop and shiftwidth automatically
    { "tpope/vim-commentary" }, -- Easy commenting
    { "tpope/vim-surround" }, -- Surround text objects
    { "windwp/nvim-autopairs", config = true }, -- Auto pair brackets, quotes, etc.
    {
        "chentoast/marks.nvim", -- A better user experience for viewing and interacting with Vim marks.,
        event = "VeryLazy",
        config = function()
            require("marks").setup({
                -- whether to map keybinds or not. default true
                default_mappings = true,
                -- which builtin marks to show. default {}
                builtin_marks = { ".", "<", ">", "^" },
                -- whether movements cycle back to the beginning/end of buffer. default true
                cyclic = true,
                -- whether the shada file is updated after modifying uppercase marks. default false
                force_write_shada = false,
                -- how often (in ms) to redraw signs/recompute mark positions.
                -- higher values will have better performance but may cause visual lag,
                -- while lower values may cause performance penalties. default 150.
                refresh_interval = 250,
                -- sign priorities for each type of mark - builtin marks, uppercase marks, lowercase
                -- marks, and bookmarks.
                -- can be either a table with all/none of the keys, or a single number, in which case
                -- the priority applies to all marks.
                -- default 10.
                sign_priority = { lower = 10, upper = 15, builtin = 8, bookmark = 20 },
                -- disables mark tracking for specific filetypes. default {}
                excluded_filetypes = {},
                -- disables mark tracking for specific buftypes. default {}
                excluded_buftypes = {},
                -- marks.nvim allows you to configure up to 10 bookmark groups, each with its own
                -- sign/virttext. Bookmarks can be used to group together positions and quickly move
                -- across multiple buffers. default sign is '!@#$%^&*()' (from 0 to 9), and
                -- default virt_text is "".
                bookmark_0 = {
                    sign = "⚑",
                    virt_text = "hello world",
                    -- explicitly prompt for a virtual line annotation when setting a bookmark from this group.
                    -- defaults to false.
                    annotate = false,
                },
                mappings = {},
            })
        end,
    },

    -- File navigation
    {
        "nvim-telescope/telescope.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
    },

    -- Formatter
    {
        "stevearc/conform.nvim",
        opts = {
            formatters_by_ft = {
                cpp = { "clang_format" },
                c = { "clang_format" },
                lua = { "stylua" },
                python = { "ruff_format" },
                javascript = { "prettier" },
                shell = { "shfmt" },
                bash = { "shfmt" },
            },
            formatters = {
                clang_format = {
                    prepend_args = {
                        "--style=file", -- Respect .clang-format in project
                        "--fallback-style=Google", -- Default if no .clang-format exists
                    },
                },
                stylua = {
                    prepend_args = { "--indent-type", "Spaces" }, -- Use spaces instead of tabs
                },
            },
        },
    },
})

-- Key mappings
vim.g.mapleader = ","
vim.g.maplocalleader = ","

vim.keymap.set("n", "<leader>fo", function()
    require("conform").format({ async = true, lsp_fallback = true })
end, { desc = "Format file" })

vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
vim.keymap.set("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })
vim.keymap.set("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Find buffers" })
vim.keymap.set("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { desc = "Help tags" })

-- Auto commands
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function()
        vim.highlight.on_yank({ timeout = 200 })
    end,
})
