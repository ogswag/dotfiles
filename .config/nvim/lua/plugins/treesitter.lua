-- nvim-treesitter is on its `main` branch (the v1.0 rewrite). That branch's
-- setup() accepts exactly one key:
--
--   ---@class TSConfig
--   ---@field install_dir string
--
-- so the old `ensure_installed` / `highlight` / `indent` / `incremental_selection`
-- opts table was being merged in and silently never read -- which is why no
-- parsers were installed and treesitter folding never worked. Parsers now get
-- installed explicitly, and highlighting is started per-buffer.
local ensure = {
	"lua",
	"luadoc",
	"zig",
	"gleam",
	"latex",
	"bibtex",
	"markdown",
	"markdown_inline",
	"html",
	"bash",
	"json",
	"toml",
	"yaml",
	"vim",
	"vimdoc",
	"query",
}

return {
	{
		"nvim-treesitter/nvim-treesitter",
		branch = "main",
		build = ":TSUpdate",
		event = { "BufReadPre", "BufNewFile" },
		cmd = { "TSInstall", "TSUpdate", "TSLog" },
		config = function()
			require("nvim-treesitter").setup({}) -- install_dir only; nothing else is read

			local ts_config = require("nvim-treesitter.config")

			-- There is no `ensure_installed` on this branch; install what's missing.
			local installed = ts_config.get_installed("parsers")
			local missing = vim.tbl_filter(function(lang)
				return not vim.tbl_contains(installed, lang)
			end, ensure)
			if #missing > 0 then
				require("nvim-treesitter").install(missing)
			end

			-- Highlighting is no longer an option -- it's vim.treesitter.start()
			-- per buffer. Same for indent, which is an indentexpr now.
			vim.api.nvim_create_autocmd("FileType", {
				group = vim.api.nvim_create_augroup("TreesitterStart", { clear = true }),
				callback = function(ev)
					local lang = vim.treesitter.language.get_lang(vim.bo[ev.buf].filetype)
					if not lang then
						return
					end
					if not vim.tbl_contains(ts_config.get_installed("parsers"), lang) then
						return
					end

					-- Skip very large files; the regex syntax fallback is cheaper.
					local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(ev.buf))
					if ok and stats and stats.size > 100 * 1024 then
						return
					end

					if pcall(vim.treesitter.start, ev.buf, lang) then
						vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
					end
				end,
			})
		end,
	},
}
