return {
	-- Mason: GUI installer for LSP servers, linters, formatters
	{
		"mason-org/mason.nvim",
		cmd = "Mason",
		build = ":MasonUpdate",
		opts = {},
	},

	-- Bridges mason ↔ lspconfig: auto-installs the servers listed below
	{
		"mason-org/mason-lspconfig.nvim",
		dependencies = { "mason-org/mason.nvim" },
		opts = {
			ensure_installed = {
				"zls", -- Zig Language Server
				"lua_ls", -- Lua (for editing this config)
				"texlab", -- LaTeX (ConTeXt is handled by digestif, below)
				-- gleam is NOT in Mason; must be installed via `brew install gleam`
				-- digestif is NOT installed here: mason's luarocks installer needs
				-- `luarocks` on PATH anyway, and its registry pin lags the release
				-- that reads ConTeXt's interface files at runtime. Install with:
				--   brew install luarocks && luarocks install --local digestif
			},
			-- NOTE: `automatic_installation` was a mason-lspconfig v1 key and is
			-- silently ignored by v2 (installed here: v2.2.0). v2's equivalent is
			-- `automatic_enable`, which defaults to true and would vim.lsp.enable()
			-- every mason-installed server behind our backs. Off, so the explicit
			-- vim.lsp.enable() call below is the single source of truth.
			automatic_enable = false,
		},
	},

	-- The LSP engine
	{
		"neovim/nvim-lspconfig",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"mason-org/mason.nvim",
			"mason-org/mason-lspconfig.nvim",
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

				-- NOTE: no insert-mode <C-k> here. It used to map signature help,
				-- which (being buffer-local) beat the global <C-k> = <Up> from
				-- lua/config/keymaps.lua in every LSP buffer. blink.cmp already
				-- provides signature help -- rebound to <C-g> in completion.lua.
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

			-- digestif: language server for TeX "and friends", and the only one
			-- that understands ConTeXt. It reads ConTeXt's own interface files at
			-- runtime, so it tracks the installed distribution (including
			-- third-party modules) rather than a bundled snapshot.
			--
			-- nvim-lspconfig ships lsp/digestif.lua with
			--   filetypes = { "tex", "plaintex", "context" }
			-- which would attach it to LaTeX buffers alongside texlab and give you
			-- every completion twice. Narrow it to the filetypes texlab can't do.
			--
			-- The `cmd` is resolved rather than left as bare "digestif": TeX Live
			-- ships its own digestif wrapper in /Library/TeX/texbin that is earlier
			-- on PATH and, on this machine, fails with "Can't find 'digestif.zip'
			-- archive". Prefer the luarocks build when it's there.
			local digestif = vim.fn.expand("~/.luarocks/bin/digestif")
			if vim.fn.executable(digestif) == 0 then
				digestif = "digestif"
			end

			-- digestif finds ConTeXt's interface definition (context-en.xml) by
			-- indexing the texmf trees that `kpsewhich` reports -- which are TeX
			-- Live's. Your ConTeXt is a standalone LMTX install outside that tree,
			-- so out of the box digestif attaches, finds no ConTeXt data, and
			-- answers every completion request with nil. DIGESTIF_TEXMF fixes that,
			-- but it REPLACES the search list, so TeX Live's dirs are appended too.
			local function digestif_texmf()
				local dirs = {}
				local ctx_bin = vim.fn.exepath("context")
				if ctx_bin ~= "" then
					-- .../tex/texmf-osx-arm64/bin/context -> .../tex/texmf-context
					local tex_root = vim.fn.fnamemodify(vim.fn.resolve(ctx_bin), ":h:h:h")
					local ctx_tree = tex_root .. "/texmf-context"
					if vim.uv.fs_stat(ctx_tree) then
						table.insert(dirs, ctx_tree)
					end
				end
				local kpse = vim.system({ "kpsewhich", "-var-brace-value=TEXMF" }, { text = true }):wait()
				if kpse.code == 0 then
					for dir in vim.trim(kpse.stdout):gmatch("[^:]+") do
						table.insert(dirs, (dir:gsub("^!!", "")))
					end
				end
				return table.concat(dirs, ":")
			end

			vim.lsp.config("digestif", {
				-- `cmd` as a function runs only when the client actually starts,
				-- i.e. in a ConTeXt buffer. As a plain table + cmd_env, the
				-- kpsewhich lookup above (~50 ms) would run at lspconfig load --
				-- the first file open of EVERY session, Zig and Lua included.
				cmd = function(dispatchers, config)
					return vim.lsp.rpc.start({ digestif }, dispatchers, {
						cwd = config.root_dir,
						env = { DIGESTIF_TEXMF = digestif_texmf() },
					})
				end,
				filetypes = { "context" },
				root_markers = { ".git", "*.mkxl", "*.mkiv" },
			})

			-- ── Activate servers ─────────────────────────────────────────────────
			-- Explicit, because mason-lspconfig's automatic_enable is off above.
			local servers = { "zls", "gleam", "texlab", "lua_ls" }
			if vim.fn.executable(digestif) == 1 then
				table.insert(servers, "digestif")
			end
			vim.lsp.enable(servers)

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
