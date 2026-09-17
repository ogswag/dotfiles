return {
	{
		"bullets-vim/bullets.vim",
		init = function()
			-- Keep Bullets' useful defaults without letting it replace Visual < and >.
			vim.g.bullets_set_mappings = 0
			vim.g.bullets_custom_mappings = {
				{ "imap", "<cr>", "<Plug>(bullets-newline)" },
				{ "inoremap", "<C-cr>", "<cr>" },
				{ "nmap", "o", "<Plug>(bullets-newline)" },
				{ "vmap", "gN", "<Plug>(bullets-renumber)" },
				{ "nmap", "gN", "<Plug>(bullets-renumber)" },
				{ "nmap", "<leader>x", "<Plug>(bullets-toggle-checkbox)" },
				{ "imap", "<C-t>", "<Plug>(bullets-demote)" },
				{ "nmap", ">>", "<Plug>(bullets-demote)" },
				{ "imap", "<C-d>", "<Plug>(bullets-promote)" },
				{ "nmap", "<<", "<Plug>(bullets-promote)" },
			}
		end,
	},
	{
		"folke/zen-mode.nvim",
		cmd = "ZenMode",
		keys = {
			{ "<leader>z", "<cmd>ZenMode<CR>", desc = "Zen mode" },
		},
		opts = {
			window = {
				-- backdrop = 0.95, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
				-- -- height and width can be:
				-- -- * an absolute number of cells when > 1
				-- -- * a percentage of the width / height of the editor when <= 1
				-- -- * a function that returns the width or the height
				-- width = 120, -- width of the Zen window
				-- height = 1, -- height of the Zen window
				-- -- by default, no options are changed for the Zen window
				-- -- uncomment any of the options below, or add other vim.wo options you want to apply
				options = {
					signcolumn = "no", -- disable signcolumn
					number = true, -- disable number column
					relativenumber = false, -- disable relative numbers
					cursorline = false, -- disable cursorline
					cursorcolumn = false, -- disable cursor column
					foldcolumn = "0", -- disable fold column
					list = false, -- disable whitespace characters
				},
			},
		},
	},

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
				lualine_x = {
					-- ConTeXt compile/watch status. package.loaded check means this
					-- never pulls in lua/context/compile.lua outside ConTeXt work.
					{
						function()
							local compile = package.loaded["context.compile"]
							if not compile then
								return ""
							end
							if compile.is_running() then
								return "󰑓 typesetting"
							end
							if compile.is_watching() then
								return "󰈈 watch"
							end
							return ""
						end,
					},
					"filetype",
				},
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
				{ "<leader>r", group = "rename" },
				-- vimtex (LaTeX) and lua/context/compile.lua (ConTeXt) share the
				-- same <localleader>l* layout on purpose.
				{ "<localleader>l", group = "TeX / ConTeXt" },
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
