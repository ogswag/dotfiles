--- DWIM outline picker: headings in prose, symbols in code.
---
--- Markdown / org / LaTeX jump via a Telescope list of headings
--- (consult-outline). Everything else uses LSP document symbols, then
--- treesitter (consult-imenu). Bound to <leader>o.

local M = {}

local LATEX_FT = { tex = true, latex = true, plaintex = true }

local LATEX_CMDS = {
	{ "subparagraph", 6 },
	{ "subsubsection", 4 },
	{ "subsection", 3 },
	{ "paragraph", 5 },
	{ "chapter", 1 },
	{ "section", 2 },
	{ "part", 0 },
}

local function indent_text(level, title)
	return string.rep("  ", level) .. title
end

local function buf_lines(bufnr)
	return vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
end

local function buf_path(bufnr)
	return vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":p")
end

local function collect_atx(bufnr, marker)
	local path = buf_path(bufnr)
	local entries = {}
	local pat = "^(" .. marker .. "+)%s+(.*)$"
	for lnum, line in ipairs(buf_lines(bufnr)) do
		local marks, title = line:match(pat)
		if marks then
			local level = #marks - 1
			entries[#entries + 1] = {
				filename = path,
				lnum = lnum,
				level = level,
				text = indent_text(level, title),
			}
		end
	end
	return entries
end

local function collect_latex_regex(bufnr)
	local path = buf_path(bufnr)
	local entries = {}
	for lnum, line in ipairs(buf_lines(bufnr)) do
		for _, spec in ipairs(LATEX_CMDS) do
			local cmd, level = spec[1], spec[2]
			local rest = line:match("^%s*\\" .. cmd .. "%*%s*(.*)$") or line:match("^%s*\\" .. cmd .. "([%s{%[].*)$")
			if rest then
				local title = rest:match("{(.-)}") or rest:match("%[(.-)%]") or cmd
				entries[#entries + 1] = {
					filename = path,
					lnum = lnum,
					level = level,
					text = indent_text(level, title),
				}
				break
			end
		end
	end
	return entries
end

local function collect_vimtex()
	if vim.fn.exists("*vimtex#toc#get_entries") ~= 1 then
		return nil
	end
	local ok, raw = pcall(vim.fn["vimtex#toc#get_entries"])
	if not ok or type(raw) ~= "table" or #raw == 0 then
		return nil
	end
	local entries = {}
	for _, e in ipairs(raw) do
		if e.type == "content" and e.title and e.line then
			local level = e.level or 0
			entries[#entries + 1] = {
				filename = e.file or buf_path(0),
				lnum = e.line,
				level = level,
				text = indent_text(level, e.title),
			}
		end
	end
	if #entries == 0 then
		return nil
	end
	return entries
end

local function closest_index(entries)
	local cur = vim.api.nvim_win_get_cursor(0)[1]
	local path = buf_path(0)
	local idx = 1
	for i, e in ipairs(entries) do
		local file = vim.fn.fnamemodify(e.filename or "", ":p")
		if file == path and e.lnum <= cur then
			idx = i
		end
	end
	return idx
end

local function pick(entries, title)
	local ok_tele, pickers = pcall(require, "telescope.pickers")
	if not ok_tele then
		vim.fn.setqflist({}, " ", { title = title, items = entries })
		vim.cmd("botright copen")
		return
	end

	local finders = require("telescope.finders")
	local conf = require("telescope.config").values

	pickers
		.new({}, {
			prompt_title = title,
			default_selection_index = closest_index(entries),
			finder = finders.new_table({
				results = entries,
				entry_maker = function(e)
					return {
						value = e,
						display = e.text,
						ordinal = e.text,
						filename = e.filename,
						lnum = e.lnum,
					}
				end,
			}),
			sorter = conf.generic_sorter({}),
			previewer = conf.qflist_previewer({}),
		})
		:find()
end

local function has_document_symbols(bufnr)
	for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
		if client:supports_method("textDocument/documentSymbol") then
			return true
		end
	end
	return false
end

function M.open()
	local bufnr = vim.api.nvim_get_current_buf()
	local ft = vim.bo[bufnr].filetype

	local entries
	if LATEX_FT[ft] then
		entries = collect_vimtex() or collect_latex_regex(bufnr)
	elseif ft == "markdown" then
		entries = collect_atx(bufnr, "#")
	elseif ft == "org" then
		entries = collect_atx(bufnr, "%*")
	end

	if entries and #entries > 0 then
		pick(entries, "Outline")
		return
	end

	local builtin = require("telescope.builtin")
	if has_document_symbols(bufnr) then
		builtin.lsp_document_symbols()
		return
	end

	local ok = pcall(builtin.treesitter)
	if not ok then
		vim.notify("no outline", vim.log.levels.INFO, { title = "Outline" })
	end
end

return M
