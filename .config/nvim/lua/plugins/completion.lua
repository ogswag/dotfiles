-- blink.cmp: completion engine written in Rust — faster than nvim-cmp.
-- Ships pre-built binaries on GitHub releases so you don't need Rust installed.
return {
	{
		"saghen/blink.cmp",
		version = "1.*", -- pin to stable v1.x; check docs if upgrading breaks things
		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			keymap = {
				preset = "super-tab", -- Tab accepts/jumps; C-n/C-p navigates
				["<CR>"] = { "accept", "fallback" }, -- Enter also accepts (VS Code feel)
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
				-- Order matters: LSP → path → snippets → buffer words
				default = { "lsp", "path", "snippets" },
			},

			-- Rust-based fuzzy matcher (blink's default and fastest option)
			fuzzy = { implementation = "prefer_rust_with_warning" },
		},
		opts_extend = { "sources.default" },
	},
}
