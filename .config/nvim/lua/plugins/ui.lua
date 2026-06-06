return {
	-- Statusline
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "echasnovski/mini.icons" },
		event = "VimEnter",
		opts = {
			options = {
				theme = "auto",
				globalstatus = true, -- one statusline for all splits (cleaner)
				component_separators = "",
				section_separators = { left = "", right = "" },
			},
			sections = {
				lualine_a = { "mode" },
				lualine_b = { "branch", "diff", "diagnostics" },
				lualine_c = { { "filename", path = 1 } }, -- show relative path
				lualine_x = { "filetype" },
				lualine_y = { "progress" },
				lualine_z = { "location" },
			},
		},
	},

	-- Indent guides
	{
		"lukas-reineke/indent-blankline.nvim",
		event = { "BufReadPre", "BufNewFile" },
		main = "ibl",
		opts = {
			-- Plain vertical lines at every indent level; no scope highlighting.
			-- (mini.nvim has no full indent-guides module — only mini.indentscope,
			-- which animates the current scope only. indent-blankline is the tool
			-- built for simple guides at all levels.)
			indent = { char = "│" },
			scope = { enabled = false },
		},
	},

	-- Auto-pairs and comments now come from mini.nvim (see lua/plugins/mini.lua).

	-- Show pending keybindings after leader delay
	{
		"folke/which-key.nvim",
		event = "VimEnter",
		opts = {
			delay = 500,
			spec = {
				{ "<leader>c", group = "code" },
				{ "<leader>f", group = "find/files" },
				{ "<leader>g", group = "git" },
				{ "<leader>b", group = "buffer" },
			},
		},
	},

	-- tabs
	{
		"akinsho/bufferline.nvim",
		version = "*",
		dependencies = { "echasnovski/mini.icons" },
		event = "VimEnter",
		keys = {
			{ "<leader>bp", "<cmd>BufferLinePick<CR>", desc = "Pick buffer" },
			{ "<leader>bx", "<cmd>BufferLinePickClose<CR>", desc = "Pick buffer to close" },
		},
		opts = {
			options = {
				mode = "buffers",
				close_command = function(n)
					require("mini.bufremove").delete(n, false)
				end,
				right_mouse_command = function(n)
					require("mini.bufremove").delete(n, false)
				end,
				left_mouse_command = "buffer %d", -- left-click tab switches to it
				indicator = { style = "icon", icon = "▎" },
				separator_style = "thin",
				show_buffer_close_icons = true,
				show_close_icon = false,
				-- Shift neo-tree offset so tabs don't overlap the file explorer
				offsets = {
					{
						filetype = "neo-tree",
						text = "Explorer",
						highlight = "Directory",
						separator = true,
					},
				},
			},
		},
	},

	-- better folding
	{
		"kevinhwang91/nvim-ufo",
		dependencies = { "kevinhwang91/promise-async" },
		event = "BufReadPost",
		keys = {
			{
				"zR",
				function()
					require("ufo").openAllFolds()
				end,
				desc = "Open all folds",
			},
			{
				"zM",
				function()
					require("ufo").closeAllFolds()
				end,
				desc = "Close all folds",
			},
		},
		opts = {
			-- Use treesitter first, fall back to indent-based detection
			provider_selector = function()
				return { "treesitter", "indent" }
			end,
		},
	},
}
