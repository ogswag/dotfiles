return {
	{
		"ogswag/valve-olive.nvim",
		lazy = false,
		priority = 1000,
		opts = {
			darker_bg = false,
			transparent = false,
			italic_comments = true,
			bold_functions = true,
		},
		config = function(_, opts)
			require("valve-olive").setup(opts)
			vim.cmd.colorscheme("valve-olive")
		end,
	},
	{
		"dchinmay2/alabaster.nvim",
		lazy = false,
		priority = 1000,
	},
	{ "oonamo/ef-themes.nvim", lazy = false, priority = 1000 },
	{ "ogswag/vim-envy", lazy = false, priority = 1000 },
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
	{
		"f-person/auto-dark-mode.nvim",
		opts = {
			update_interval = 3000,
			set_dark_mode = function()
				vim.o.background = "dark"
				vim.cmd.colorscheme("valve-olive")
			end,
			set_light_mode = function()
				vim.o.background = "light"
				vim.cmd.colorscheme("zenbones")
			end,
		},
	},
}
