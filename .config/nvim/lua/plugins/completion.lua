-- blink.cmp: completion engine written in Rust -- faster than nvim-cmp.
-- Ships pre-built binaries on GitHub releases so you don't need Rust installed.
return {
	-- Snippet engine. LuaSnip rather than mini.snippets because only LuaSnip can
	-- auto-expand on a regex match as you type, with no Tab press.
	{
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		build = "make install_jsregexp",
		dependencies = { "rafamadriz/friendly-snippets" },
		event = "InsertEnter",
		config = function()
			local ls = require("luasnip")
			ls.setup({
				enable_autosnippets = true,
				-- Autosnippets need to re-evaluate as the buffer changes, not just
				-- on InsertLeave.
				update_events = { "TextChanged", "TextChangedI" },
				delete_check_events = "TextChanged",
			})

			-- friendly-snippets (VSCode-style JSON), lazily per filetype.
			require("luasnip.loaders.from_vscode").lazy_load()
			-- Our own Lua snippets: ~/.config/nvim/snippets/<ft>.lua
			require("luasnip.loaders.from_lua").lazy_load({
				paths = vim.fn.stdpath("config") .. "/snippets",
			})

			-- Cycle choice nodes. NOT <C-l>: that's already <Right> in insert mode
			-- (see lua/config/keymaps.lua).
			vim.keymap.set({ "i", "s" }, "<C-u>", function()
				if ls.choice_active() then
					ls.change_choice(1)
				end
			end, { desc = "LuaSnip: next choice" })
		end,
	},

	{
		"saghen/blink.cmp",
		version = "1.*", -- pin to stable v1.x; check docs if upgrading breaks things
		dependencies = { "L3MON4D3/LuaSnip" },
		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			snippets = { preset = "luasnip" },

			keymap = {
				preset = "super-tab", -- Tab accepts/jumps; C-n/C-p navigates

				-- The super-tab preset claims <C-k> (signature) and <C-e> (cancel),
				-- which collide with the insert-mode readline cluster in
				-- lua/config/keymaps.lua (<C-k> = Up, <C-e> = end-of-line). Blink
				-- treats an empty table as "disabled", so those keys stay ours.
				["<C-k>"] = {},
				["<C-e>"] = {},
				["<C-g>"] = { "show_signature", "hide_signature", "fallback" },
				["<C-q>"] = { "cancel", "fallback" },
			},

			appearance = {
				nerd_font_variant = "mono",
			},

			completion = {
				-- Auto-pop documentation window (like VS Code's hover widget)
				documentation = {
					auto_show = true,
					auto_show_delay_ms = 150,
					window = { border = "rounded" },
				},
				-- Ghost text: shows top completion inline in grey (like Copilot but for LSP)
				ghost_text = { enabled = true },
				-- Automatically insert brackets after function completions
				accept = {
					auto_brackets = { enabled = true },
				},
				menu = {
					border = "rounded",
					draw = {
						-- Syntax-highlight items in the menu using treesitter
						treesitter = { "lsp" },
					},
				},
			},

			sources = {
				-- Order matters: LSP -> path -> snippets -> buffer words.
				-- `buffer` was missing here despite the comment claiming otherwise.
				default = { "lsp", "path", "snippets", "buffer" },

				per_filetype = {
					tex = { "snippets", "lsp", "buffer", "path" },
					plaintex = { "snippets", "lsp", "buffer", "path" },
					markdown = { "snippets", "lsp", "buffer", "path" },
				},
			},

			-- Rust-based fuzzy matcher (blink's default and fastest option)
			fuzzy = { implementation = "prefer_rust_with_warning" },
		},
		opts_extend = { "sources.default" },
	},
}
