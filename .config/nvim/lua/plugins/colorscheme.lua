-- Only the ACTIVE colorscheme loads at startup. Everything else is lazy.
--
-- Previously all ten were `lazy = false, priority = 1000`, so every one of them
-- was sourced on every launch while only mfd-lumon was ever applied. lazy.nvim
-- hooks ColorSchemePre and searches lazy plugins' colors/ directories, so
-- `:colorscheme tokyonight` (or Telescope's colorscheme picker) still loads the
-- right plugin on demand -- including running its config/setup() first.
return {
	-- Active theme. Eager + highest priority so it paints before the UI draws.
	{
		"kungfusheep/mfd.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			require("mfd").setup({
				accessibility_contrast = 4, -- 0 (default) to 10 (max WCAG compliance)
				no_italic = true, -- disable italic highlighting (default: false)
				-- bright_comments = true, -- legacy: equivalent to accessibility_contrast = 4
			})
			-- vim.cmd.colorscheme("mfd-lumon")
		end,
	},

	-- Available on demand via :colorscheme <name>
	{ "panjakub/cobaltnext.nvim", name = "cobaltnext", lazy = false },
	{ "lunacookies/vim-colors-xcode", lazy = false },
	{
		"ogswag/valve-olive.nvim",
		lazy = false,
		opts = {
			darker_bg = true,
			transparent = false,
			italic_comments = true,
			bold_functions = true,
		},
		config = function(_, opts)
			require("valve-olive").setup(opts)
		end,
	},
	{
		"ogswag/envy.nvim",
		lazy = false,
		config = function(_, opts)
			require("envy").setup(opts)
		end,
	},
	{ "NLKNguyen/papercolor-theme", lazy = false },
	{ "jonathanfilip/vim-lucius", lazy = false },
	{ "forest-nvim/sequoia.nvim", lazy = false },
	{ "folke/tokyonight.nvim", lazy = false },
	{
		dir = "~/.config/nvim/lua/plugins/harlequin.nvim",
		name = "harlequin",
		lazy = false,
		priority = 1000,
	},
	{
		"IroncladDev/osmium",
		lazy = false,
		config = function()
			require("osmium").setup({
				integrations = {
					-- gitsigns is currently disabled (lua/plugins/git.lua is fully
					-- commented out); harmless, but this flag does nothing until
					-- it's re-enabled.
					gitsigns = true,
					telescope = true,
				},
				transparent_bg = false,
				show_end_of_buffer = false,
			})
		end,
	},
	{
		"f-person/auto-dark-mode.nvim",
		opts = {
			set_dark_mode = function()
				vim.api.nvim_set_option_value("background", "dark", {})
				vim.cmd.colorscheme("harlequin")
				require("lualine").setup({ options = { theme = "harlequin" } })
			end,
			set_light_mode = function()
				vim.api.nvim_set_option_value("background", "light", {})
				-- vim.g.green_grass_mode = "soft" -- "soft" or "vivid"
				vim.cmd.colorscheme("PaperColor")
				require("lualine").setup({ options = { theme = "auto" } })
			end,
			update_interval = 3000,
			fallback = "dark",
		},
	},
}
