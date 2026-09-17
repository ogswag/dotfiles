-- green-grass.lua
--
-- Green Grass
--
-- Modes:
--
--   vivid
--     Full original palette. Bright, colorful syntax highlighting.
--
--   soft
--     Slightly reduced syntax color variety, inspired by the philosophy
--     of MFD-style themes. Still colorful, but identifiers, properties,
--     operators, modules, etc. stop competing for attention.
--
-- Configure BEFORE loading:
--
--   vim.g.green_grass_mode = "soft"
--   vim.cmd.colorscheme("green-grass")
--
-- Default: "vivid"

vim.cmd("highlight clear")

if vim.fn.exists("syntax_on") then
	vim.cmd("syntax reset")
end

vim.o.termguicolors = true
vim.o.background = "dark"

vim.g.colors_name = "green-grass"

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

local mode = vim.g.green_grass_mode or "vivid"

if mode ~= "vivid" and mode ~= "soft" then
	mode = "vivid"
end

------------------------------------------------------------------------
-- Base palette
------------------------------------------------------------------------

local c = {
	bg = "#23653C",
	fg = "#FFFFFF",
	cursor = "#FFA460",

	selection = "#FFFACD",
	select_fg = "#000000",

	tab_bg = "#4C805F",
	tab_fg = "#A6CCB3",

	-- ANSI palette
	black = "#000000",
	gray = "#767676",

	red = "#FF6B60",
	red_bright = "#FFB6B0",

	green = "#0BE377",
	green_hi = "#6FE8AC",

	yellow = "#FFFFB6",
	yellow_hi = "#FFFFCB",

	blue = "#96CAFD",
	blue_hi = "#B5DCFE",

	magenta = "#FF73FD",
	magenta_hi = "#FF9CFE",

	cyan = "#0DCDCD",
	cyan_hi = "#14FFFF",

	white_dim = "#DDDDDD",
	white = "#FFFFFF",

	-- Derived green UI shades
	bg_alt = "#2D7048",
	bg_alt2 = "#377853",
	bg_float = "#2A6B44",
	bg_dark = "#1C5733",

	border = "#72A383",

	muted = "#A6CCB3",
	muted2 = "#83AD91",

	line = "#2B6E45",
	line_hi = "#33794E",

	-- Diagnostic backgrounds
	error_bg = "#7B453E",
	warn_bg = "#77703B",
	info_bg = "#386B74",
	hint_bg = "#317060",

	-- Diff backgrounds
	diff_add = "#286F45",
	diff_change = "#316D62",
	diff_delete = "#70453E",
	diff_text = "#467E72",
}

------------------------------------------------------------------------
-- Semantic syntax palettes
--
-- Everything below the UI layer uses these roles instead of referring
-- directly to ANSI colors.
------------------------------------------------------------------------

local syntax = {}

if mode == "soft" then
	--------------------------------------------------------------------
	-- SOFT
	--
	-- Deliberately retains:
	--
	--   salmon  -> structural keywords
	--   yellow  -> functions
	--   green   -> strings
	--   cyan    -> types
	--   orange  -> genuinely special syntax
	--
	-- But collapses:
	--
	--   variables / properties / fields -> foreground
	--   operators                       -> dim foreground
	--   modules                         -> dim foreground
	--   preprocessor                    -> dim foreground
	--   magenta literals                -> pale blue
	--
	-- This removes the "rainbow every token" effect without making the
	-- theme monochrome.
	--------------------------------------------------------------------

	syntax = {
		text = c.fg,
		text_dim = c.white_dim,

		comment = c.muted,

		string = c.green_hi,
		regexp = c.green_hi,

		number = c.blue_hi,
		constant = c.blue_hi,
		boolean = c.blue_hi,

		identifier = c.fg,
		variable = c.fg,
		parameter = c.white_dim,
		property = c.fg,
		field = c.fg,

		func = c.yellow,
		func_builtin = c.yellow_hi,

		keyword = c.red_bright,

		operator = c.white_dim,

		type = c.cyan_hi,
		type_builtin = c.cyan_hi,

		preproc = c.white_dim,
		module = c.white_dim,

		special = c.cursor,

		tag = c.green_hi,
		tag_attribute = c.yellow,

		punctuation = c.white_dim,
	}
else
	--------------------------------------------------------------------
	-- VIVID
	--
	-- Essentially the original syntax palette.
	--------------------------------------------------------------------

	syntax = {
		text = c.fg,
		text_dim = c.white_dim,

		comment = c.muted,

		string = c.green_hi,
		regexp = c.cyan_hi,

		number = c.magenta_hi,
		constant = c.magenta_hi,
		boolean = c.magenta,

		identifier = c.blue,
		variable = c.fg,
		parameter = c.blue,
		property = c.blue_hi,
		field = c.blue_hi,

		func = c.yellow,
		func_builtin = c.yellow_hi,

		keyword = c.red_bright,

		operator = c.cyan_hi,

		type = c.cyan_hi,
		type_builtin = c.cyan,

		preproc = c.blue_hi,
		module = c.blue_hi,

		special = c.cursor,

		tag = c.green,
		tag_attribute = c.yellow,

		punctuation = c.white_dim,
	}
end

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

local function hi(group, opts)
	vim.api.nvim_set_hl(0, group, opts)
end

local function link(group, target)
	hi(group, { link = target })
end

------------------------------------------------------------------------
-- Editor
------------------------------------------------------------------------

hi("Normal", {
	fg = c.fg,
	bg = c.bg,
})

hi("NormalNC", {
	fg = c.white_dim,
	bg = c.bg,
})

hi("NormalFloat", {
	fg = c.fg,
	bg = c.bg_float,
})

hi("FloatBorder", {
	fg = c.green_hi,
	bg = c.bg_float,
})

hi("FloatTitle", {
	fg = c.yellow,
	bg = c.bg_float,
	bold = true,
})

for _, group in ipairs({
	"Cursor",
	"lCursor",
	"CursorIM",
	"TermCursor",
}) do
	hi(group, {
		fg = c.black,
		bg = c.cursor,
	})
end

hi("CursorLine", {
	bg = c.line,
})

hi("CursorColumn", {
	bg = c.line,
})

hi("ColorColumn", {
	bg = c.bg_dark,
})

hi("LineNr", {
	fg = c.muted2,
	bg = c.bg,
})

hi("CursorLineNr", {
	fg = c.yellow,
	bg = c.line,
	bold = true,
})

hi("SignColumn", {
	fg = c.muted,
	bg = c.bg,
})

hi("FoldColumn", {
	fg = c.muted,
	bg = c.bg,
})

hi("Folded", {
	fg = c.green_hi,
	bg = c.bg_dark,
	italic = true,
})

------------------------------------------------------------------------
-- Selection / searching
------------------------------------------------------------------------

hi("Visual", {
	fg = c.select_fg,
	bg = c.selection,
})

hi("VisualNOS", {
	fg = c.select_fg,
	bg = c.selection,
})

hi("Search", {
	fg = c.black,
	bg = c.yellow,
})

hi("IncSearch", {
	fg = c.black,
	bg = c.cursor,
	bold = true,
})

hi("CurSearch", {
	fg = c.black,
	bg = c.cursor,
	bold = true,
})

hi("Substitute", {
	fg = c.black,
	bg = c.red_bright,
})

hi("MatchParen", {
	fg = c.yellow,
	bg = c.bg_alt2,
	bold = true,
	underline = true,
})

------------------------------------------------------------------------
-- Invisible characters
------------------------------------------------------------------------

hi("Whitespace", {
	fg = "#639173",
})

hi("NonText", {
	fg = "#639173",
})

hi("SpecialKey", {
	fg = c.muted2,
})

hi("EndOfBuffer", {
	fg = c.bg,
	bg = c.bg,
})

------------------------------------------------------------------------
-- Windows / splits
------------------------------------------------------------------------

hi("WinSeparator", {
	fg = c.border,
	bg = c.bg,
})

hi("VertSplit", {
	fg = c.border,
	bg = c.bg,
})

hi("WinBar", {
	fg = c.yellow,
	bg = c.bg,
	bold = true,
})

hi("WinBarNC", {
	fg = c.muted,
	bg = c.bg,
})

------------------------------------------------------------------------
-- Popup menu
------------------------------------------------------------------------

hi("Pmenu", {
	fg = c.fg,
	bg = c.tab_bg,
})

hi("PmenuSel", {
	fg = c.black,
	bg = c.green_hi,
	bold = true,
})

hi("PmenuSbar", {
	bg = c.bg_alt,
})

hi("PmenuThumb", {
	bg = c.muted,
})

hi("PmenuMatch", {
	fg = c.yellow,
	bg = c.tab_bg,
	bold = true,
})

hi("PmenuMatchSel", {
	fg = c.black,
	bg = c.green_hi,
	bold = true,
})

hi("WildMenu", {
	fg = c.black,
	bg = c.yellow,
	bold = true,
})

------------------------------------------------------------------------
-- General UI
------------------------------------------------------------------------

hi("Title", {
	fg = c.yellow,
	bold = true,
})

hi("Directory", {
	fg = c.blue,
	bold = true,
})

hi("Question", {
	fg = c.green,
	bold = true,
})

hi("MoreMsg", {
	fg = c.green_hi,
})

hi("ModeMsg", {
	fg = c.yellow,
	bold = true,
})

hi("ErrorMsg", {
	fg = c.red_bright,
	bold = true,
})

hi("WarningMsg", {
	fg = c.yellow,
	bold = true,
})

------------------------------------------------------------------------
-- Traditional Vim syntax
------------------------------------------------------------------------

hi("Comment", {
	fg = syntax.comment,
})

hi("Constant", {
	fg = syntax.constant,
})

hi("String", {
	fg = syntax.string,
})

hi("Character", {
	fg = syntax.string,
})

hi("Number", {
	fg = syntax.number,
})

hi("Boolean", {
	fg = syntax.boolean,
	bold = true,
})

hi("Float", {
	fg = syntax.number,
})

hi("Identifier", {
	fg = syntax.identifier,
})

hi("Function", {
	fg = syntax.func,
	bold = true,
})

hi("Statement", {
	fg = syntax.keyword,
	bold = true,
})

hi("Conditional", {
	fg = syntax.keyword,
	bold = true,
})

hi("Repeat", {
	fg = syntax.keyword,
})

hi("Label", {
	fg = mode == "soft" and syntax.text_dim or c.yellow,
})

hi("Operator", {
	fg = syntax.operator,
})

hi("Keyword", {
	fg = syntax.keyword,
	bold = true,
})

hi("Exception", {
	fg = syntax.keyword,
})

hi("PreProc", {
	fg = syntax.preproc,
})

hi("Include", {
	fg = syntax.preproc,
})

hi("Define", {
	fg = syntax.preproc,
})

hi("Macro", {
	fg = syntax.preproc,
})

hi("PreCondit", {
	fg = syntax.preproc,
})

hi("Type", {
	fg = syntax.type,
	bold = mode == "vivid",
})

hi("StorageClass", {
	fg = syntax.type,
})

hi("Structure", {
	fg = syntax.type,
})

hi("Typedef", {
	fg = syntax.type,
})

hi("Special", {
	fg = syntax.special,
})

hi("SpecialChar", {
	fg = syntax.special,
})

hi("Tag", {
	fg = syntax.tag,
})

hi("Delimiter", {
	fg = syntax.punctuation,
})

hi("SpecialComment", {
	fg = syntax.comment,
})

hi("Debug", {
	fg = c.red,
})

hi("Underlined", {
	fg = c.blue_hi,
	underline = true,
})

hi("Ignore", {
	fg = c.gray,
})

hi("Error", {
	fg = c.white,
	bg = c.error_bg,
})

hi("Todo", {
	fg = c.black,
	bg = c.yellow,
	bold = true,
})

------------------------------------------------------------------------
-- Treesitter: comments
------------------------------------------------------------------------

hi("@comment", {
	fg = syntax.comment,
})

hi("@comment.documentation", {
	fg = mode == "soft" and syntax.comment or c.green_hi,
})

hi("@comment.todo", {
	fg = c.black,
	bg = c.yellow,
	bold = true,
})

hi("@comment.error", {
	fg = c.black,
	bg = c.red_bright,
	bold = true,
})

hi("@comment.warning", {
	fg = c.black,
	bg = c.yellow,
	bold = true,
})

hi("@comment.note", {
	fg = c.black,
	bg = c.blue_hi,
	bold = true,
})

------------------------------------------------------------------------
-- Treesitter: strings
------------------------------------------------------------------------

hi("@string", {
	fg = syntax.string,
})

hi("@string.documentation", {
	fg = syntax.string,
})

hi("@string.escape", {
	fg = syntax.special,
	bold = true,
})

hi("@string.regexp", {
	fg = syntax.regexp,
})

hi("@string.special", {
	fg = syntax.special,
})

hi("@string.special.symbol", {
	fg = syntax.special,
})

hi("@string.special.path", {
	fg = syntax.string,
})

hi("@string.special.url", {
	fg = c.blue_hi,
	underline = true,
})

hi("@character", {
	fg = syntax.string,
})

hi("@character.special", {
	fg = syntax.special,
})

------------------------------------------------------------------------
-- Treesitter: literals
------------------------------------------------------------------------

hi("@number", {
	fg = syntax.number,
})

hi("@number.float", {
	fg = syntax.number,
})

hi("@boolean", {
	fg = syntax.boolean,
	bold = true,
})

hi("@constant", {
	fg = syntax.constant,
})

hi("@constant.builtin", {
	fg = syntax.constant,
	bold = true,
})

hi("@constant.macro", {
	fg = syntax.constant,
})

------------------------------------------------------------------------
-- Treesitter: variables
------------------------------------------------------------------------

hi("@variable", {
	fg = syntax.variable,
})

hi("@variable.builtin", {
	fg = mode == "soft" and syntax.text_dim or c.blue_hi,
})

hi("@variable.parameter", {
	fg = syntax.parameter,
})

hi("@variable.parameter.builtin", {
	fg = mode == "soft" and syntax.parameter or c.blue_hi,
	italic = true,
})

hi("@variable.member", {
	fg = syntax.property,
})

hi("@property", {
	fg = syntax.property,
})

hi("@field", {
	fg = syntax.field,
})

------------------------------------------------------------------------
-- Treesitter: functions
------------------------------------------------------------------------

hi("@function", {
	fg = syntax.func,
})

hi("@function.builtin", {
	fg = syntax.func_builtin,
})

hi("@function.call", {
	fg = syntax.func,
})

hi("@function.macro", {
	fg = syntax.func_builtin,
})

hi("@function.method", {
	fg = syntax.func,
})

hi("@function.method.call", {
	fg = syntax.func,
})

hi("@constructor", {
	fg = syntax.type,
})

------------------------------------------------------------------------
-- Treesitter: keywords
------------------------------------------------------------------------

for _, group in ipairs({
	"@keyword",
	"@keyword.coroutine",
	"@keyword.function",
	"@keyword.modifier",
	"@keyword.repeat",
	"@keyword.exception",
	"@keyword.conditional",
}) do
	hi(group, {
		fg = syntax.keyword,
	})
end

hi("@keyword", {
	fg = syntax.keyword,
	bold = true,
})

hi("@keyword.return", {
	fg = syntax.keyword,
	bold = true,
})

hi("@keyword.debug", {
	fg = c.red,
})

hi("@keyword.operator", {
	fg = mode == "soft" and syntax.operator or syntax.keyword,
})

hi("@keyword.import", {
	fg = mode == "soft" and syntax.keyword or syntax.preproc,
})

hi("@keyword.type", {
	fg = syntax.type,
})

hi("@keyword.conditional.ternary", {
	fg = syntax.operator,
})

------------------------------------------------------------------------
-- Treesitter: operators
------------------------------------------------------------------------

hi("@operator", {
	fg = syntax.operator,
})

------------------------------------------------------------------------
-- Treesitter: types
------------------------------------------------------------------------

hi("@type", {
	fg = syntax.type,
})

hi("@type.builtin", {
	fg = syntax.type_builtin,
	bold = mode == "vivid",
})

hi("@type.definition", {
	fg = syntax.type,
})

hi("@type.qualifier", {
	fg = syntax.keyword,
})

------------------------------------------------------------------------
-- Treesitter: attributes / modules / labels
------------------------------------------------------------------------

hi("@attribute", {
	fg = syntax.special,
})

hi("@attribute.builtin", {
	fg = syntax.special,
})

hi("@module", {
	fg = syntax.module,
})

hi("@module.builtin", {
	fg = syntax.module,
	italic = mode == "soft",
})

hi("@label", {
	fg = mode == "soft" and syntax.text_dim or c.yellow,
})

------------------------------------------------------------------------
-- Treesitter: tags
------------------------------------------------------------------------

hi("@tag", {
	fg = syntax.tag,
})

hi("@tag.builtin", {
	fg = syntax.tag,
})

hi("@tag.attribute", {
	fg = syntax.tag_attribute,
})

hi("@tag.delimiter", {
	fg = syntax.punctuation,
})

------------------------------------------------------------------------
-- Treesitter: punctuation
------------------------------------------------------------------------

hi("@punctuation.delimiter", {
	fg = syntax.punctuation,
})

hi("@punctuation.bracket", {
	fg = syntax.punctuation,
})

hi("@punctuation.special", {
	fg = syntax.special,
})

------------------------------------------------------------------------
-- Treesitter: markup
------------------------------------------------------------------------

hi("@markup.heading", {
	fg = c.yellow,
	bold = true,
})

hi("@markup.heading.1", {
	fg = c.yellow,
	bold = true,
})

hi("@markup.heading.2", {
	fg = c.green_hi,
	bold = true,
})

hi("@markup.heading.3", {
	fg = mode == "soft" and c.white or c.blue_hi,
	bold = true,
})

hi("@markup.heading.4", {
	fg = mode == "soft" and c.muted or c.magenta_hi,
	bold = true,
})

hi("@markup.heading.5", {
	fg = mode == "soft" and c.white_dim or c.cyan_hi,
	bold = true,
})

hi("@markup.heading.6", {
	fg = c.muted,
	bold = true,
})

hi("@markup.strong", {
	bold = true,
})

hi("@markup.italic", {
	italic = true,
})

hi("@markup.strikethrough", {
	strikethrough = true,
})

hi("@markup.underline", {
	underline = true,
})

hi("@markup.quote", {
	fg = c.green_hi,
	italic = true,
})

hi("@markup.math", {
	fg = mode == "soft" and syntax.type or c.cyan_hi,
})

hi("@markup.link", {
	fg = c.blue_hi,
})

hi("@markup.link.label", {
	fg = c.yellow,
})

hi("@markup.link.url", {
	fg = c.blue,
	underline = true,
})

hi("@markup.raw", {
	fg = syntax.string,
})

hi("@markup.raw.block", {
	fg = syntax.string,
})

hi("@markup.list", {
	fg = syntax.special,
})

hi("@markup.list.checked", {
	fg = c.green,
})

hi("@markup.list.unchecked", {
	fg = c.muted,
})

------------------------------------------------------------------------
-- LSP semantic tokens
--
-- Explicit links stop semantic-token highlighting from reintroducing
-- unrelated colors on top of Treesitter.
------------------------------------------------------------------------

link("@lsp.type.comment", "@comment")
link("@lsp.type.string", "@string")
link("@lsp.type.number", "@number")
link("@lsp.type.regexp", "@string.regexp")
link("@lsp.type.operator", "@operator")

link("@lsp.type.variable", "@variable")
link("@lsp.type.parameter", "@variable.parameter")
link("@lsp.type.property", "@property")

link("@lsp.type.function", "@function")
link("@lsp.type.method", "@function.method")

link("@lsp.type.class", "@type")
link("@lsp.type.interface", "@type")
link("@lsp.type.struct", "@type")
link("@lsp.type.type", "@type")
link("@lsp.type.typeParameter", "@type")

link("@lsp.type.namespace", "@module")
link("@lsp.type.enum", "@type")
link("@lsp.type.enumMember", "@constant")

link("@lsp.type.decorator", "@attribute")
link("@lsp.type.macro", "@function.macro")

------------------------------------------------------------------------
-- Diagnostics
--
-- Intentionally NOT muted by soft mode.
-- Diagnostics should remain obvious.
------------------------------------------------------------------------

hi("DiagnosticError", {
	fg = c.red_bright,
})

hi("DiagnosticWarn", {
	fg = c.yellow,
})

hi("DiagnosticInfo", {
	fg = c.blue_hi,
})

hi("DiagnosticHint", {
	fg = c.cyan_hi,
})

hi("DiagnosticOk", {
	fg = c.green_hi,
})

hi("DiagnosticVirtualTextError", {
	fg = c.red_bright,
	bg = c.error_bg,
})

hi("DiagnosticVirtualTextWarn", {
	fg = c.yellow,
	bg = c.warn_bg,
})

hi("DiagnosticVirtualTextInfo", {
	fg = c.blue_hi,
	bg = c.info_bg,
})

hi("DiagnosticVirtualTextHint", {
	fg = c.cyan_hi,
	bg = c.hint_bg,
})

hi("DiagnosticVirtualTextOk", {
	fg = c.green_hi,
	bg = c.diff_add,
})

hi("DiagnosticUnderlineError", {
	undercurl = true,
	sp = c.red,
})

hi("DiagnosticUnderlineWarn", {
	undercurl = true,
	sp = c.yellow,
})

hi("DiagnosticUnderlineInfo", {
	undercurl = true,
	sp = c.blue,
})

hi("DiagnosticUnderlineHint", {
	undercurl = true,
	sp = c.cyan,
})

hi("DiagnosticUnderlineOk", {
	undercurl = true,
	sp = c.green,
})

hi("DiagnosticFloatingError", {
	fg = c.red_bright,
})

hi("DiagnosticFloatingWarn", {
	fg = c.yellow,
})

hi("DiagnosticFloatingInfo", {
	fg = c.blue_hi,
})

hi("DiagnosticFloatingHint", {
	fg = c.cyan_hi,
})

------------------------------------------------------------------------
-- LSP UI
------------------------------------------------------------------------

hi("LspReferenceText", {
	bg = c.bg_alt2,
})

hi("LspReferenceRead", {
	bg = c.bg_alt2,
})

hi("LspReferenceWrite", {
	bg = c.bg_alt2,
	bold = true,
})

hi("LspInlayHint", {
	fg = c.muted,
	bg = c.bg_alt,
	italic = true,
})

hi("LspCodeLens", {
	fg = c.muted,
})

hi("LspCodeLensSeparator", {
	fg = c.muted2,
})

hi("LspSignatureActiveParameter", {
	fg = c.yellow,
	bold = true,
})

------------------------------------------------------------------------
-- Diff
------------------------------------------------------------------------

hi("DiffAdd", {
	fg = c.green_hi,
	bg = c.diff_add,
})

hi("DiffChange", {
	fg = c.cyan_hi,
	bg = c.diff_change,
})

hi("DiffDelete", {
	fg = c.red_bright,
	bg = c.diff_delete,
})

hi("DiffText", {
	fg = c.white,
	bg = c.diff_text,
	bold = true,
})

hi("Added", {
	fg = c.green_hi,
})

hi("Changed", {
	fg = c.blue_hi,
})

hi("Removed", {
	fg = c.red_bright,
})

------------------------------------------------------------------------
-- Status line
------------------------------------------------------------------------

hi("StatusLine", {
	fg = c.white,
	bg = c.tab_bg,
	bold = true,
})

hi("StatusLineNC", {
	fg = c.tab_fg,
	bg = c.bg_dark,
})

------------------------------------------------------------------------
-- Native tab line
--
-- Exact Kitty relationship:
--
-- active:
--   white on #23653C
--
-- inactive:
--   #A6CCB3 on #4C805F
------------------------------------------------------------------------

hi("TabLineSel", {
	fg = c.white,
	bg = c.bg,
	bold = true,
})

hi("TabLine", {
	fg = c.tab_fg,
	bg = c.tab_bg,
})

hi("TabLineFill", {
	bg = c.tab_bg,
})

------------------------------------------------------------------------
-- Telescope
------------------------------------------------------------------------

hi("TelescopeNormal", {
	fg = c.fg,
	bg = c.bg_float,
})

hi("TelescopeBorder", {
	fg = c.green_hi,
	bg = c.bg_float,
})

hi("TelescopePromptNormal", {
	fg = c.fg,
	bg = c.bg_alt,
})

hi("TelescopePromptBorder", {
	fg = c.green_hi,
	bg = c.bg_alt,
})

hi("TelescopePromptTitle", {
	fg = c.black,
	bg = c.green_hi,
	bold = true,
})

hi("TelescopePreviewTitle", {
	fg = c.black,
	bg = c.blue,
	bold = true,
})

hi("TelescopeResultsTitle", {
	fg = c.black,
	bg = c.yellow,
	bold = true,
})

hi("TelescopeSelection", {
	fg = c.white,
	bg = c.tab_bg,
	bold = true,
})

hi("TelescopeSelectionCaret", {
	fg = c.cursor,
	bg = c.tab_bg,
	bold = true,
})

hi("TelescopeMatching", {
	fg = c.yellow,
	bold = true,
})

hi("TelescopePromptPrefix", {
	fg = c.cursor,
})

------------------------------------------------------------------------
-- Neo-tree
------------------------------------------------------------------------

hi("NeoTreeNormal", {
	fg = c.fg,
	bg = c.bg_dark,
})

hi("NeoTreeNormalNC", {
	fg = c.white_dim,
	bg = c.bg_dark,
})

hi("NeoTreeEndOfBuffer", {
	fg = c.bg_dark,
	bg = c.bg_dark,
})

hi("NeoTreeDirectoryName", {
	fg = c.blue_hi,
})

hi("NeoTreeDirectoryIcon", {
	fg = c.blue,
})

hi("NeoTreeRootName", {
	fg = c.yellow,
	bold = true,
})

hi("NeoTreeFileName", {
	fg = c.fg,
})

hi("NeoTreeFileNameOpened", {
	fg = c.cursor,
	bold = true,
})

hi("NeoTreeGitAdded", {
	fg = c.green_hi,
})

hi("NeoTreeGitModified", {
	fg = c.yellow,
})

hi("NeoTreeGitDeleted", {
	fg = c.red_bright,
})

hi("NeoTreeGitConflict", {
	fg = c.magenta_hi,
})

hi("NeoTreeGitUntracked", {
	fg = c.green,
})

hi("NeoTreeIndentMarker", {
	fg = c.muted2,
})

hi("NeoTreeWinSeparator", {
	fg = c.border,
	bg = c.bg_dark,
})

hi("NeoTreeCursorLine", {
	bg = c.bg_alt,
})

------------------------------------------------------------------------
-- GitSigns
------------------------------------------------------------------------

hi("GitSignsAdd", {
	fg = c.green_hi,
})

hi("GitSignsChange", {
	fg = c.blue_hi,
})

hi("GitSignsDelete", {
	fg = c.red_bright,
})

hi("GitSignsCurrentLineBlame", {
	fg = c.muted,
	italic = true,
})

------------------------------------------------------------------------
-- Which-key
------------------------------------------------------------------------

hi("WhichKey", {
	fg = c.yellow,
	bold = true,
})

hi("WhichKeyGroup", {
	fg = c.blue_hi,
})

hi("WhichKeyDesc", {
	fg = c.green_hi,
})

hi("WhichKeySeparator", {
	fg = c.muted,
})

hi("WhichKeyValue", {
	fg = c.muted,
})

------------------------------------------------------------------------
-- Lazy.nvim
------------------------------------------------------------------------

hi("LazyNormal", {
	fg = c.fg,
	bg = c.bg_float,
})

hi("LazyButton", {
	fg = c.fg,
	bg = c.tab_bg,
})

hi("LazyButtonActive", {
	fg = c.black,
	bg = c.green_hi,
	bold = true,
})

hi("LazyH1", {
	fg = c.black,
	bg = c.yellow,
	bold = true,
})

hi("LazyH2", {
	fg = c.yellow,
	bold = true,
})

hi("LazySpecial", {
	fg = c.cursor,
})

------------------------------------------------------------------------
-- Mason
------------------------------------------------------------------------

hi("MasonHeader", {
	fg = c.black,
	bg = c.green_hi,
	bold = true,
})

hi("MasonHeaderSecondary", {
	fg = c.black,
	bg = c.blue,
	bold = true,
})

hi("MasonHighlight", {
	fg = c.green_hi,
})

hi("MasonHighlightBlock", {
	fg = c.black,
	bg = c.green_hi,
})

hi("MasonHighlightBlockBold", {
	fg = c.black,
	bg = c.green_hi,
	bold = true,
})

hi("MasonMuted", {
	fg = c.muted,
})

hi("MasonMutedBlock", {
	fg = c.black,
	bg = c.muted,
})

------------------------------------------------------------------------
-- Indent guides
------------------------------------------------------------------------

hi("IblIndent", {
	fg = "#4D7F5D",
})

hi("IblScope", {
	fg = c.green_hi,
})

hi("IndentBlanklineChar", {
	fg = "#4D7F5D",
})

hi("IndentBlanklineContextChar", {
	fg = c.green_hi,
})

------------------------------------------------------------------------
-- nvim-cmp
------------------------------------------------------------------------

hi("CmpItemAbbr", {
	fg = c.white_dim,
})

hi("CmpItemAbbrDeprecated", {
	fg = c.gray,
	strikethrough = true,
})

hi("CmpItemAbbrMatch", {
	fg = c.yellow,
	bold = true,
})

hi("CmpItemAbbrMatchFuzzy", {
	fg = c.yellow,
	bold = true,
})

hi("CmpItemMenu", {
	fg = c.muted,
})

hi("CmpItemKindFunction", {
	fg = syntax.func,
})

hi("CmpItemKindMethod", {
	fg = syntax.func,
})

hi("CmpItemKindConstructor", {
	fg = syntax.type,
})

hi("CmpItemKindVariable", {
	fg = mode == "soft" and syntax.text_dim or c.blue,
})

hi("CmpItemKindField", {
	fg = mode == "soft" and syntax.text_dim or c.blue_hi,
})

hi("CmpItemKindProperty", {
	fg = mode == "soft" and syntax.text_dim or c.blue_hi,
})

hi("CmpItemKindKeyword", {
	fg = syntax.keyword,
})

hi("CmpItemKindClass", {
	fg = syntax.type,
})

hi("CmpItemKindInterface", {
	fg = syntax.type,
})

hi("CmpItemKindStruct", {
	fg = syntax.type,
})

hi("CmpItemKindModule", {
	fg = syntax.module,
})

hi("CmpItemKindText", {
	fg = syntax.string,
})

hi("CmpItemKindConstant", {
	fg = syntax.constant,
})

hi("CmpItemKindEnum", {
	fg = syntax.type,
})

hi("CmpItemKindEnumMember", {
	fg = syntax.constant,
})

hi("CmpItemKindSnippet", {
	fg = syntax.special,
})

hi("CmpItemKindColor", {
	fg = c.magenta_hi,
})

hi("CmpItemKindFile", {
	fg = c.blue_hi,
})

hi("CmpItemKindFolder", {
	fg = c.blue,
})

------------------------------------------------------------------------
-- blink.cmp
------------------------------------------------------------------------

hi("BlinkCmpMenu", {
	fg = c.fg,
	bg = c.tab_bg,
})

hi("BlinkCmpMenuSelection", {
	fg = c.black,
	bg = c.green_hi,
	bold = true,
})

hi("BlinkCmpLabel", {
	fg = c.white_dim,
})

hi("BlinkCmpLabelMatch", {
	fg = c.yellow,
	bold = true,
})

hi("BlinkCmpLabelDeprecated", {
	fg = c.gray,
	strikethrough = true,
})

hi("BlinkCmpKindFunction", {
	fg = syntax.func,
})

hi("BlinkCmpKindMethod", {
	fg = syntax.func,
})

hi("BlinkCmpKindVariable", {
	fg = mode == "soft" and syntax.text_dim or c.blue,
})

hi("BlinkCmpKindProperty", {
	fg = mode == "soft" and syntax.text_dim or c.blue_hi,
})

hi("BlinkCmpKindKeyword", {
	fg = syntax.keyword,
})

hi("BlinkCmpKindClass", {
	fg = syntax.type,
})

hi("BlinkCmpKindInterface", {
	fg = syntax.type,
})

hi("BlinkCmpKindModule", {
	fg = syntax.module,
})

hi("BlinkCmpKindText", {
	fg = syntax.string,
})

------------------------------------------------------------------------
-- Tabby
------------------------------------------------------------------------

hi("TabbyCurrent", {
	fg = c.white,
	bg = c.bg,
	bold = true,
})

hi("TabbyVisible", {
	fg = c.tab_fg,
	bg = c.tab_bg,
})

hi("TabbyHidden", {
	fg = c.tab_fg,
	bg = c.tab_bg,
})

hi("TabbyFill", {
	bg = c.tab_bg,
})

------------------------------------------------------------------------
-- Terminal ANSI colors
--
-- Exact Kitty ANSI palette. Soft mode DOES NOT touch these.
------------------------------------------------------------------------

vim.g.terminal_color_0 = "#000000"
vim.g.terminal_color_1 = "#FF6B60"
vim.g.terminal_color_2 = "#0BE377"
vim.g.terminal_color_3 = "#FFFFB6"
vim.g.terminal_color_4 = "#96CAFD"
vim.g.terminal_color_5 = "#FF73FD"
vim.g.terminal_color_6 = "#0DCDCD"
vim.g.terminal_color_7 = "#DDDDDD"

vim.g.terminal_color_8 = "#767676"
vim.g.terminal_color_9 = "#FFB6B0"
vim.g.terminal_color_10 = "#6FE8AC"
vim.g.terminal_color_11 = "#FFFFCB"
vim.g.terminal_color_12 = "#B5DCFE"
vim.g.terminal_color_13 = "#FF9CFE"
vim.g.terminal_color_14 = "#14FFFF"
vim.g.terminal_color_15 = "#FFFFFF"
