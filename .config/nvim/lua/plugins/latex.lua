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

			vim.g.vimtex_view_skim_reading_bar = 1
			-- Compiler: latexmk (comes with MacTeX)
			vim.g.vimtex_compiler_method = "latexmk"
			vim.g.vimtex_compiler_latexmk = {
				options = {
					"-pdflua",
					"-shell-escape", -- needed for minted, tikz-externalize, etc.
					"-verbose",
					"-file-line-error",
					"-synctex=1",
					"-interaction=nonstopmode",
					"-f",
					"-auxdir=build",
				},
			}

			-- Let treesitter handle syntax highlighting; disable vimtex's own.
			-- NOTE: this was set long before it was true -- nvim-treesitter was on its
			-- `main` branch with an opts table it never read, so no `latex` parser was
			-- installed and .tex fell back to Neovim's bundled regex syntax/tex.vim.
			-- lua/plugins/treesitter.lua now installs the parser and starts it.
			-- Trade-off to know about: with vimtex syntax off, vimtex's own
			-- syntax-based math-zone detection (vimtex#syntax#in_mathzone) returns
			-- nothing; flip this to 1 if you ever wire math snippets for LaTeX.
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
