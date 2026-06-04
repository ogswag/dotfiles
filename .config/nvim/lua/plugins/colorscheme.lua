return {
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
			vim.cmd([[colors osmium]])
		end,
	},
	{
		"wtfox/jellybeans.nvim",
		lazy = false,
		priority = 1000,
	},
}
