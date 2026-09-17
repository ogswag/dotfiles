-- mini.nvim modules (each pulled from echasnovski's per-module mirror so lazy
-- can load/dedupe them independently). See https://nvim-mini.org/mini.nvim
return {
	-- Icons provider. Replaces nvim-web-devicons: faster, better maintained.
	-- mock_nvim_web_devicons() makes plugins that `require("nvim-web-devicons")`
	-- (lualine, bufferline, telescope) transparently use mini.icons.
	{
		"echasnovski/mini.icons",
		lazy = false, -- needed early by statusline/tabline at startup
		opts = {},
		config = function(_, opts)
			require("mini.icons").setup(opts)
			MiniIcons.mock_nvim_web_devicons()
		end,
	},

	-- Auto-close/auto-pair brackets, quotes, etc. Replaces nvim-autopairs.
	{
		"echasnovski/mini.pairs",
		event = "InsertEnter",
		opts = {},
	},

	-- gc / gcc to toggle comments. Replaces Comment.nvim.
	-- Resolves 'commentstring' per-line (treesitter-aware via custom hook),
	-- which is why it handles .conf / ghostty-style files that Comment.nvim
	-- left uncommented. Filetype commentstrings are set in config/autocmds.lua.
	{
		"echasnovski/mini.comment",
		event = { "BufReadPre", "BufNewFile" },
		opts = {},
	},

	-- NOTE: the snippet engine is LuaSnip, not mini.snippets -- see
	-- lua/plugins/completion.lua. mini.snippets cannot auto-expand (its own docs:
	-- "'LuaSnip' can autoexpand snippets, while this module always requires" a
	-- keypress), which rules out the regex math abbreviations used for ConTeXt.

	{
		"echasnovski/mini.bufremove",
		lazy = false,
		version = false, -- use main branch; mini's tagged releases lag
		config = function()
			require("mini.bufremove").setup()
		end,
	},
}
