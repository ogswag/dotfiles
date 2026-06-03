-- vimtex: the standard LaTeX plugin for Neovim.
-- Provides compilation, PDF synctex, better motions, folding, and more.
return {
	{
		"lervag/vimtex",
		lazy = false, -- don't lazy-load; vimtex needs early filetype detection
		init = function()
			-- PDF viewer: Skim (macOS, free, synctex support)
			-- https://skim-app.sourceforge.io
			vim.g.vimtex_view_method = "skim"
			vim.g.vimtex_view_skim_sync = 1 -- jump to position in Skim after compile
			vim.g.vimtex_view_skim_activate = 1 -- bring Skim to front

			-- Compiler: latexmk (comes with MacTeX)
			vim.g.vimtex_compiler_method = "latexmk"
			vim.g.vimtex_compiler_latexmk = {
				options = {
					"-pdf",
					"-shell-escape", -- needed for minted, tikz-externalize, etc.
					"-verbose",
					"-file-line-error",
					"-synctex=1",
					"-interaction=nonstopmode",
				},
			}

			-- Let treesitter handle syntax highlighting; disable vimtex's own
			vim.g.vimtex_syntax_enabled = 0

			-- Suppress cosmetic warnings from quickfix
			vim.g.vimtex_quickfix_ignore_filters = {
				"Underfull",
				"Overfull",
				"specifier changed to",
				"Token not allowed",
			}
		end,
		-- vimtex keymaps use <localleader> (= \) by default, e.g.:
		--   \ll   compile (toggle continuous)
		--   \lv   view PDF
		--   \lc   clean aux files
		--   \le   open error list
		--   \lt   table of contents sidebar
		--   \li   info about current file
	},
}
