-- nvim-treesitter v1.0 removed the `configs` sub-module.
-- Use `main = "nvim-treesitter"` so lazy calls require("nvim-treesitter").setup(opts).
return {
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		event = { "BufReadPre", "BufNewFile" },
		main = "nvim-treesitter", -- ← the fix: no more require("nvim-treesitter.configs")
		opts = {
			ensure_installed = {
				"lua",
				"luadoc",
				"zig",
				"gleam",
				"latex",
				"bibtex",
				"markdown",
				"markdown_inline",
				"bash",
				"json",
				"toml",
				"yaml",
				"vim",
				"vimdoc",
			},
			auto_install = true,
			highlight = {
				enable = true,
				-- Disable on very large files to avoid lag
				disable = function(_, buf)
					local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
					return ok and stats and stats.size > 100 * 1024
				end,
			},
			indent = { enable = true },
			-- Ctrl+Space to expand selection node by node
			incremental_selection = {
				enable = true,
				keymaps = {
					init_selection = "<C-space>",
					node_incremental = "<C-space>",
					scope_incremental = false,
					node_decremental = "<BS>",
				},
			},
		},
	},
}
