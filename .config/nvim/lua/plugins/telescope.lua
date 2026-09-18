return {
	{
		"nvim-telescope/telescope.nvim",
		branch = "master",

		dependencies = {
			"nvim-lua/plenary.nvim",

			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
				cond = function()
					return vim.fn.executable("make") == 1
				end,
			},

			"nvim-telescope/telescope-file-browser.nvim",
		},

		cmd = "Telescope",

		keys = {
			-- Navigable filesystem browser.
			{
				"<leader>e",
				"<cmd>Telescope file_browser path=%:p:h select_buffer=true<CR>",
				desc = "Browse files",
			},

			-- Flat fuzzy file search from the tab/project root, not the
			-- window-local lcd (AutoLcd), so sibling dirs stay reachable.
			{
				"<leader>ff",
				function()
					local root = vim.fs.root(0, { ".git", "build.zig", "gleam.toml", ".luarc.json", "Makefile" })
					require("telescope.builtin").find_files({
						cwd = root or vim.fn.getcwd(-1, 0),
					})
				end,
				desc = "Find files",
			},

			{ "<leader>fr", "<cmd>Telescope oldfiles<CR>", desc = "Recent files" },
			{ "<leader>g", "<cmd>Telescope live_grep<CR>", desc = "Live grep" },
			{ "<leader>b", "<cmd>Telescope buffers<CR>", desc = "Buffers" },
			{ "<leader>h", "<cmd>Telescope help_tags<CR>", desc = "Help tags" },
			{ "<leader>fd", "<cmd>Telescope diagnostics<CR>", desc = "Diagnostics" },

			{
				"<leader>/",
				"<cmd>Telescope current_buffer_fuzzy_find<CR>",
				desc = "Search in buffer",
			},

			{
				"<leader>o",
				function()
					require("outline").open()
				end,
				desc = "Outline",
			},
		},

		config = function()
			local telescope = require("telescope")
			local actions = require("telescope.actions")
			local action_state = require("telescope.actions.state")
			local fb_actions = require("telescope._extensions.file_browser.actions")

			-------------------------------------------------------------------
			-- File-browser selection
			--
			-- Directory + <CR> → enter directory
			-- File      + <CR> → open in a real Neovim tabpage
			-------------------------------------------------------------------

			-- Resolve to an absolute path before tabedit. Relative paths plus
			-- window-local lcd otherwise create empty buffers in the wrong dir.
			local function select_tab(prompt_bufnr)
				local entry = action_state.get_selected_entry()
				if entry then
					local p = entry.path or entry.filename
					if p and p ~= "" then
						if p:sub(1, 1) ~= "/" then
							local picker = action_state.get_current_picker(prompt_bufnr)
							p = vim.fs.joinpath(picker.cwd or vim.uv.cwd(), p)
						end
						local abs = vim.fn.fnamemodify(p, ":p")
						entry.path = abs
						entry.filename = abs
					end
				end
				actions.select_tab(prompt_bufnr)
			end

			local function file_browser_select(prompt_bufnr)
				local entry = action_state.get_selected_entry()

				if not entry then
					return
				end

				-- telescope-file-browser entries expose a plenary Path.
				--
				-- For directories, retain the browser's normal selection
				-- behavior so <CR> descends into them.
				if entry.Path and entry.Path:is_dir() then
					actions.select_default(prompt_bufnr)
					return
				end

				-- Files open in a real Neovim tabpage.
				select_tab(prompt_bufnr)
			end

			telescope.setup({
				defaults = {
					prompt_prefix = " ",
					selection_caret = " ",
					entry_prefix = " ",

					path_display = { "smart" },

					sorting_strategy = "ascending",

					layout_config = {
						horizontal = {
							prompt_position = "top",
							preview_width = 0.55,
						},
					},

					mappings = {
						i = {
							["<C-j>"] = actions.move_selection_next,
							["<C-k>"] = actions.move_selection_previous,

							["<C-q>"] = actions.send_to_qflist + actions.open_qflist,

							["<Esc>"] = actions.close,
						},
					},
				},

				-----------------------------------------------------------------
				-- Built-in pickers
				-----------------------------------------------------------------

				pickers = {
					find_files = {
						hidden = true,

						file_ignore_patterns = {
							"^%.git/",
							"^node_modules/",
							"^%.zig%-cache/",
							"^_minted",
						},

						-- Open selected files in real Neovim tabpages.
						mappings = {
							i = {
								["<CR>"] = select_tab,
							},

							n = {
								["<CR>"] = select_tab,
							},
						},
					},
					oldfiles = {
						-- Open selected files in real Neovim tabpages.
						mappings = {
							i = {
								["<CR>"] = select_tab,
							},

							n = {
								["<CR>"] = select_tab,
							},
						},
					},
				},

				-----------------------------------------------------------------
				-- Extensions
				-----------------------------------------------------------------

				extensions = {
					file_browser = {
						-- Folder-browser searches start from wherever we've
						-- navigated instead of jumping back to the original cwd.
						cwd_to_path = true,

						-- Group directories together.
						grouped = true,

						-- Show dotfiles.
						hidden = {
							file_browser = true,
							folder_browser = true,
						},

						-- Honour .gitignore.
						respect_gitignore = true,

						-- Use telescope-file-browser in place of netrw.
						hijack_netrw = true,

						mappings = {
							i = {
								-- Vertico-directory-like backspace:
								--
								-- prompt non-empty → delete character
								-- prompt empty     → parent directory
								["<BS>"] = fb_actions.backspace,

								-- Explicitly go to parent directory.
								["<C-g>"] = fb_actions.goto_parent_dir,

								-- Directory → descend
								-- File      → new tabpage
								["<CR>"] = file_browser_select,
							},

							n = {
								["<CR>"] = file_browser_select,

								-- Convenient normal-mode parent directory.
								["-"] = fb_actions.goto_parent_dir,
							},
						},
					},
				},
			})

			-------------------------------------------------------------------
			-- Extensions
			-------------------------------------------------------------------

			pcall(telescope.load_extension, "fzf")
			pcall(telescope.load_extension, "file_browser")
		end,
	},
}
