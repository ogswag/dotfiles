-- mini.nvim modules (each pulled from echasnovski's per-module mirror so lazy
-- can load/dedupe them independently). See https://nvim-mini.org/mini.nvim
return {
	-- Icons provider. Replaces nvim-web-devicons: faster, better maintained.
	-- mock_nvim_web_devicons() makes plugins that `require("nvim-web-devicons")`
	-- (lualine, neo-tree, bufferline, telescope) transparently use mini.icons.
	{
		"echasnovski/mini.icons",
		lazy = false, -- needed early by statusline/tabline/tree at startup
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

	-- Snippet engine. Wired into blink.cmp via snippets.preset = "mini_snippets"
	-- (see lua/plugins/completion.lua), so snippets show up in the completion
	-- popup alongside LSP items.
	{
		"echasnovski/mini.snippets",
		dependencies = {
			-- Community-maintained prewritten snippets (VSCode-style JSON).
			-- mini.snippets reads them off the runtimepath via gen_loader.from_lang().
			"rafamadriz/friendly-snippets",
		},
		event = "InsertEnter",
		config = function()
			local snippets = require("mini.snippets")
			snippets.setup({
				snippets = {
					-- Your own global snippets, if you create the file (optional).
					snippets.gen_loader.from_file("~/.config/nvim/snippets/global.json"),
					-- Per-language snippets from friendly-snippets + ~/.config/nvim/snippets/<lang>.
					snippets.gen_loader.from_lang(),
				},
			})
		end,
	},
	{
		"echasnovski/mini.bufremove",
		lazy = false,
		version = false, -- use main branch; mini's tagged releases lag
		config = function()
			require("mini.bufremove").setup()
		end,
	},
}
