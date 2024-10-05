return {
	"echasnovski/mini.nvim",
	config = function()
		require("mini.ai").setup()
		require("mini.align").setup()
		require("mini.icons").setup()
		require("mini.operators").setup()
		require("mini.surround").setup()
	end,
}
