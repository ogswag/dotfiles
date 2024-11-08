return {
	"nvim-treesitter/nvim-treesitter",
	"nvim-treesitter/playground",
	event = { "BufReadPre", "BufNewFile" },
	build = ":TSUpdate",
	dependencies = {
		"windwp/nvim-ts-autotag",
	},
	config = function()
		local treesitter = require("nvim-treesitter.configs")

		treesitter.setup({
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			ensure_installed = {
				"bash",
				"c",
				"cmake",
				"cpp",
				"css",
				"dockerfile",
				"doxygen",
				"gitignore",
				"html",
				"javascript",
				"json",
				"latex",
				"llvm",
				"lua",
				"make",
				"markdown",
				"markdown_inline",
				"nasm",
				"objc",
				"python",
				"rust",
				"swift",
				"toml",
				"tsx",
				"typescript",
				"vim",
				"yaml",
			},
		})
	end,
}
