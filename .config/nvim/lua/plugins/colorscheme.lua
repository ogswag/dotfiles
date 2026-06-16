return {
	{
		"lunacookies/vim-colors-xcode",
		config = function()
			vim.o.background = "dark"
			vim.cmd.colorscheme("xcodedark")
		end,
	},
	{
		"ogswag/valve-olive.nvim",
		lazy = false,
		priority = 1000,
		opts = {
			darker_bg = true,
			transparent = false,
			italic_comments = true,
			bold_functions = true,
		},
		config = function(_, opts)
			require("valve-olive").setup(opts)
		end,
	},
	{
		"dchinmay2/alabaster.nvim",
		lazy = false,
		priority = 1000,
	},
	{ "oonamo/ef-themes.nvim", lazy = false, priority = 1000 },
	{
		"ogswag/envy.nvim", -- or your fork/path
		lazy = false,
		priority = 1000,
		opts = {}, -- see Configuration
		config = function(_, opts)
			require("envy").setup(opts)
		end,
	},
	{ "NLKNguyen/papercolor-theme", lazy = false, priority = 1000 },
	{ "jonathanfilip/vim-lucius", lazy = false, priority = 1000 },
	{
		"zenbones-theme/zenbones.nvim",
		lazy = false,
		-- you can set set configuration options here
		config = function()
			vim.g.zenbones_compat = 1
		end,
	},
	{
		"miikanissi/modus-themes.nvim",
		priority = 1000,
		config = function()
			require("modus-themes").setup({
				variants = {
					modus_operandi = "tinted", -- Set variant for `modus_operandi` style
					modus_vivendi = "default", -- Set variant for `modus_vivendi` style
				},
			})
		end,
	},
	{
		"forest-nvim/sequoia.nvim",
		lazy = false,
		priority = 1000,
	},
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		-- config = function()
		-- 	vim.cmd([[colorscheme tokyonight]])
		-- end,
		opts = {},
	},
	{
		"kungfusheep/mfd.nvim",
		lazy = false,
	},
	{
		"IroncladDev/osmium",
		lazy = false,
		config = function()
			require("osmium").setup({
				integrations = {
					gitsigns = true,
					telescope = true,
				},
				transparent_bg = false,
				show_end_of_buffer = false,
			})
		end,
	},
	{
		"wtfox/jellybeans.nvim",
		lazy = false,
		priority = 1000,
	},
}
