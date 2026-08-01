return {
	{ "lunacookies/vim-colors-xcode", lazy = false },
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
		"ogswag/envy.nvim",
		lazy = false,
		priority = 1000,
		config = function(_, opts)
			require("envy").setup(opts)
		end,
	},
	{ "NLKNguyen/papercolor-theme", lazy = false, priority = 1000 },
	{ "jonathanfilip/vim-lucius", lazy = false, priority = 1000 },
	{ "forest-nvim/sequoia.nvim", lazy = false, priority = 1000 },
	{ "folke/tokyonight.nvim", lazy = false, priority = 1000 },
	{ "kungfusheep/mfd.nvim", lazy = false },
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
		"f-person/auto-dark-mode.nvim",
		opts = {
			set_dark_mode = function()
				vim.api.nvim_set_option_value("background", "dark", {})
				vim.cmd.colorscheme("habamax")
				require("lualine").setup({ options = { theme = "codedark" } })
			end,
			set_light_mode = function()
				vim.api.nvim_set_option_value("background", "light", {})
				vim.cmd.colorscheme("envy")
				require("lualine").setup({ options = { theme = "envy" } })
			end,
			update_interval = 3000,
			fallback = "dark",
		},
	},
}
