return {
	-- Statusline
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
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
			indent = { char = "│" },
			scope = { enabled = true },
		},
	},

	-- Auto-close brackets, quotes, etc.
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {
			check_ts = true, -- use treesitter for smarter pairing (avoids pairing in comments/strings)
		},
	},

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

	-- gc / gcc to toggle comments
	{
		"numToStr/Comment.nvim",
		event = { "BufReadPre", "BufNewFile" },
		opts = {},
	},

	-- tabs
	{
		"akinsho/bufferline.nvim",
		version = "*",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		event = "VimEnter",
		keys = {
			{ "<leader>bp", "<cmd>BufferLinePick<CR>", desc = "Pick buffer" },
			{ "<leader>bx", "<cmd>BufferLinePickClose<CR>", desc = "Pick buffer to close" },
		},
		opts = {
			options = {
				mode = "buffers",
				close_command = "bdelete! %d",
				right_mouse_command = "bdelete! %d", -- right-click tab closes it
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
