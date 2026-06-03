return {
	{
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v3.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		},
		cmd = "Neotree",
		keys = {
			{ "<leader>e", "<cmd>Neotree toggle<CR>", desc = "File explorer" },
			{ "<leader>E", "<cmd>Neotree reveal<CR>", desc = "Reveal file" },
		},
		opts = {
			close_if_last_window = true,
			popup_border_style = "rounded",
			filesystem = {
				filtered_items = {
					visible = false,
					hide_dotfiles = false,
					hide_gitignored = true,
					hide_by_name = { ".DS_Store", ".zig-cache" },
				},
				follow_current_file = { enabled = true },
				use_libuv_file_watcher = true, -- auto-refresh without :e
			},
			window = {
				position = "left",
				width = 32,
				mappings = {
					-- Unmap space so it doesn't conflict with our <leader>
					["<space>"] = "none",
				},
			},
			default_component_configs = {
				git_status = {
					symbols = {
						added = "✚",
						modified = "",
						deleted = "✖",
						renamed = "➜",
						untracked = "★",
						ignored = "◌",
						unstaged = "✗",
						staged = "✓",
						conflict = "",
					},
				},
			},
		},
	},
}
