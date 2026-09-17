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
				{ "imap", "<C-,>", "<Plug>(bullets-demote)" },
				{ "imap", "<C-.>", "<Plug>(bullets-promote)" },
				-- { "nmap", ">>", "<Plug>(bullets-demote)" },
				-- { "nmap", "<<", "<Plug>(bullets-promote)" },
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
				{ "<leader>r", group = "rename" },
				{ "<localleader>l", group = "LaTeX" },
			},
		},
	},

	-- tabs
	{
		"nanozuki/tabby.nvim",
		lazy = false,
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},

		config = function()
			vim.o.showtabline = 2

			-- Preserve real tab layouts + Tabby tab names in sessions.
			vim.opt.sessionoptions:append("tabpages")
			vim.opt.sessionoptions:append("globals")

			require("tabby").setup({
				preset = "tab_only",

				option = {
					nerdfont = true,

					-- If you use lualine, you can optionally inherit its theme:
					-- lualine_theme = "auto",

					tab_name = {
						-- Fallback name when you haven't manually renamed a tab.
						name_fallback = function(tabid)
							return "Tab " .. tabid
						end,
					},
				},
			})
		end,

		keys = {
			-- Create / close tabs
			{ "<leader>tn", "<cmd>tabnew<CR>", desc = "New tab" },
			{ "<leader>tc", "<cmd>tabclose<CR>", desc = "Close tab" },
			{ "<leader>to", "<cmd>tabonly<CR>", desc = "Close other tabs" },

			-- Navigate tabs
			{ "<leader>tl", "<cmd>tabnext<CR>", desc = "Next tab" },
			{ "<leader>th", "<cmd>tabprevious<CR>", desc = "Previous tab" },

			-- Move tabs
			{ "<leader>tL", "<cmd>+tabmove<CR>", desc = "Move tab right" },
			{ "<leader>tH", "<cmd>-tabmove<CR>", desc = "Move tab left" },

			-- Tabby features
			{ "<leader>tr", "<cmd>Tabby rename_tab<CR>", desc = "Rename tab" },
			{ "<leader>tj", "<cmd>Tabby jump_to_tab<CR>", desc = "Jump to tab" },
			{ "<leader>tw", "<cmd>Tabby pick_window<CR>", desc = "Pick window" },
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
