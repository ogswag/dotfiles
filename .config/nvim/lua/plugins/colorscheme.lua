return {
	{ "ogswag/vim-envy", lazy = false, priority = 1000 },
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
				vim.cmd.colorscheme("darkblue")
			end,
			set_light_mode = function()
				vim.o.background = "light"
				vim.cmd.colorscheme("envy")
			end,
		},
	},
}
