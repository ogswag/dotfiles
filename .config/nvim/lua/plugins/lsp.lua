return {
	-- Mason: GUI installer for LSP servers, linters, formatters
	{
		"williamboman/mason.nvim",
		cmd = "Mason",
		build = ":MasonUpdate",
		opts = {},
	},

	-- Bridges mason ↔ lspconfig: auto-installs the servers listed below
	{
		"williamboman/mason-lspconfig.nvim",
		dependencies = { "williamboman/mason.nvim" },
		opts = {
			ensure_installed = {
				"zls", -- Zig Language Server
				"texlab", -- LaTeX
				"lua_ls", -- Lua (for editing this config)
				-- gleam is NOT in Mason; must be installed via `brew install gleam`
			},
			automatic_installation = false,
		},
	},

	-- The LSP engine
	{
		"neovim/nvim-lspconfig",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			"saghen/blink.cmp", -- must load first so we can get its capabilities
		},
		config = function()
			-- ── Shared on_attach ─────────────────────────────────────────────────
			-- Keymaps attached per-buffer when any LSP connects
			local on_attach = function(_, bufnr)
				local function map(keys, func, desc)
					vim.keymap.set("n", keys, func, { buffer = bufnr, desc = "LSP: " .. desc })
				end
				map("gd", vim.lsp.buf.definition, "Go to definition")
				map("gD", vim.lsp.buf.declaration, "Go to declaration")
				map("gr", vim.lsp.buf.references, "References")
				map("gi", vim.lsp.buf.implementation, "Go to implementation")
				map("gt", vim.lsp.buf.type_definition, "Go to type definition")
				map("K", vim.lsp.buf.hover, "Hover docs")
				map("<leader>ca", vim.lsp.buf.code_action, "Code action")
				map("<leader>rn", vim.lsp.buf.rename, "Rename symbol")
				map("<leader>ds", vim.lsp.buf.document_symbol, "Document symbols")

				-- Signature help in insert mode (shows argument types while typing a call)
				vim.keymap.set(
					"i",
					"<C-k>",
					vim.lsp.buf.signature_help,
					{ buffer = bufnr, desc = "LSP: Signature help" }
				)
			end

			-- ── Global defaults (applied to every server) ────────────────────────
			-- nvim-lspconfig registers cmd/filetypes/root_markers for all known servers;
			-- we just layer on_attach + capabilities on top.
			vim.lsp.config("*", {
				on_attach = on_attach,
				capabilities = require("blink.cmp").get_lsp_capabilities(),
			})

			-- ── Per-server settings ───────────────────────────────────────────────
			-- (only override what differs from lspconfig's bundled defaults)

			vim.lsp.config("zls", {
				settings = {
					zls = {
						enable_snippets = true,
						enable_ast_check_diagnostics = true,
						enable_autofix = true,
						semantic_tokens = "partial",
					},
				},
			})

			-- gleam: no extra settings needed; `gleam lsp` is in PATH via `brew install gleam`

			vim.lsp.config("texlab", {
				settings = {
					texlab = {
						-- Let vimtex drive compilation; texlab just provides diagnostics + search
						build = {
							executable = "latexmk",
							args = {
								"-pdf",
								"-interaction=nonstopmode",
								"-synctex=1",
								"%f",
							},
							onSave = false,
							forwardSearchAfter = false,
						},
						chktex = { onOpenAndSave = true, onEdit = false },
						-- Forward search: jump from .tex cursor position to PDF page in Skim
						forwardSearch = {
							executable = "displayline", -- Skim's CLI tool
							args = { "-revert", "%l", "%p", "%f" },
						},
						diagnostics = {
							ignoredPatterns = {
								"^Underfull",
								"^Overfull",
								"specifier changed",
							},
						},
					},
				},
			})

			vim.lsp.config("lua_ls", {
				-- Without a root marker, lua_ls walks up to ~ and then refuses to load.
				-- .luarc.json (created below) is the strongest anchor; .git is a fallback.
				root_markers = { ".luarc.json", ".luarc.jsonc", ".git" },
				settings = {
					Lua = {
						runtime = { version = "LuaJIT" },
						diagnostics = { globals = { "vim" } },
						workspace = {
							checkThirdParty = false,
							-- Only add VIMRUNTIME; the "${3rd}/luv/library" path can trigger
							-- the home-dir scan bug on some setups, so drop it here.
							library = { vim.env.VIMRUNTIME },
						},
						telemetry = { enable = false },
						hint = { enable = true },
					},
				},
			})

			-- ── Activate servers ─────────────────────────────────────────────────
			vim.lsp.enable({ "zls", "gleam", "texlab", "lua_ls" })

			-- ── Diagnostic display ───────────────────────────────────────────────
			vim.diagnostic.config({
				virtual_text = {
					prefix = "●",
					source = "if_many", -- only show source when multiple LSPs are active
				},
				float = { border = "rounded", source = true },
				signs = true,
				underline = true,
				update_in_insert = false, -- don't flash errors while you're mid-type
				severity_sort = true,
			})
		end,
	},
}
