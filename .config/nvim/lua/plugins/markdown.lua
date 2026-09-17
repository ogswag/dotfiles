return {
	{
		"MeanderingProgrammer/render-markdown.nvim",
		ft = "markdown",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"echasnovski/mini.icons",
		},
		opts = {
			heading = {
				sign = false,
				border = true,
				width = "full",
			},
			code = {
				sign = false,
				width = "full",
			},
			-- Needs `utftex` (libtexprintf) or `latex2text` (pylatexenc). Missing
			-- converters are skipped; $...$ stays as source.
			latex = {
				enabled = true,
				converter = { "utftex", "latex2text" },
			},
			completions = {
				lsp = { enabled = true },
			},
		},
	},
}
