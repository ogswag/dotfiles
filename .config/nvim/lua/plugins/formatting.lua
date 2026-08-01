return {
	{
		"stevearc/conform.nvim",
		event = "BufWritePre",
		cmd = "ConformInfo",
		keys = {
			{
				"<leader>cf",
				function()
					require("conform").format({ async = true, lsp_fallback = true })
				end,
				desc = "Format file",
			},
		},
		opts = {
			formatters_by_ft = {
				lua = { "stylua" },
				-- zigfmt is bundled with the Zig compiler; no separate install needed
				zig = { "zigfmt" },
				-- Gleam: no stdin-based formatter available in conform.
				-- Falls through to lsp_fallback, which calls gleam LSP's textDocument/formatting.
				gleam = {},
				-- latexindent ships with MacTeX; for BasicTeX: sudo tlmgr install latexindent
				tex = { "latexindent" },
			},

			-- Format on save (respects per-buffer opt-out via :lua vim.b.disable_autoformat=true)
			format_on_save = function(bufnr)
				if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
					return -- return nil = skip formatting
				end
				return {
					timeout_ms = 1000,
					lsp_fallback = true, -- used for gleam and any ft without a formatter
				}
			end,
		},
	},
}
