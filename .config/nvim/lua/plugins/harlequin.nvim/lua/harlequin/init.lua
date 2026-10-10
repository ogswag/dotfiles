-- Harlequin highlight engine.
-- Palettes: "harlequin" (warm charcoal) and "harlequin-black" (OLED).

local M = {}

local palettes = {
  harlequin = {
    text      = { '#F8F8F2', 15  },
    bg        = { '#1C1B1A', 234 },
    white     = { '#FFFFFF', 15  },
    black     = { '#000000', 0   },
    grey0     = { '#BEBEBE', 250 },
    grey1     = { '#808080', 244 },
    grey2     = { '#696969', 242 },
    grey3     = { '#545454', 240 },
    grey4     = { '#343434', 236 },
    grey5     = { '#080808', 232 },
    cerise    = { '#FF0033', 197 },
    lime      = { '#AEEE00', 154 },
    gold      = { '#FFB829', 214 },
    brick     = { '#CB4154', 167 },
    lilac     = { '#AE81FF', 141 },
    frost     = { '#2C89C7', 68  },
    sunny     = { '#FFFC7F', 228 },
    mordant   = { '#AE0C00', 124 },
    auburn    = { '#7C0A02', 88  },
    deep_blue = { '#0D3A6E', 24  },
    moss      = { '#004225', 22  },
  },
  ['harlequin-black'] = {
    text      = { '#F8F8F2', 15  },
    bg        = { '#000000', 0   }, -- OLED
    white     = { '#FFFFFF', 15  },
    black     = { '#000000', 0   },
    grey0     = { '#BEBEBE', 250 },
    grey1     = { '#808080', 244 },
    grey2     = { '#5C5C5C', 241 }, -- slightly dimmer chrome on true black
    grey3     = { '#3A3A3A', 237 },
    grey4     = { '#161616', 233 }, -- visual / folds: lifted just enough
    grey5     = { '#0A0A0A', 232 }, -- cursorline / floats / panels
    cerise    = { '#FF0033', 197 },
    lime      = { '#AEEE00', 154 },
    gold      = { '#FFB829', 214 },
    brick     = { '#CB4154', 167 },
    lilac     = { '#AE81FF', 141 },
    frost     = { '#2C89C7', 68  },
    sunny     = { '#FFFC7F', 228 },
    mordant   = { '#AE0C00', 124 },
    auburn    = { '#5A0701', 52  }, -- darker diff-delete wash
    deep_blue = { '#071E3A', 17  },
    moss      = { '#002214', 22  },
  },
}

function M.palette(name)
  return palettes[name] or palettes.harlequin
end

function M.lualine(name)
  local P = M.palette(name)
  return {
    normal = {
      a = { fg = P.black[1], bg = P.gold[1], gui = 'bold' },
      b = { fg = P.text[1], bg = P.grey4[1] },
      c = { fg = P.grey0[1], bg = P.grey5[1] },
    },
    insert = {
      a = { fg = P.black[1], bg = P.lime[1], gui = 'bold' },
      b = { fg = P.text[1], bg = P.grey4[1] },
      c = { fg = P.grey0[1], bg = P.grey5[1] },
    },
    visual = {
      a = { fg = P.black[1], bg = P.lilac[1], gui = 'bold' },
      b = { fg = P.text[1], bg = P.grey4[1] },
      c = { fg = P.grey0[1], bg = P.grey5[1] },
    },
    replace = {
      a = { fg = P.white[1], bg = P.cerise[1], gui = 'bold' },
      b = { fg = P.text[1], bg = P.grey4[1] },
      c = { fg = P.grey0[1], bg = P.grey5[1] },
    },
    command = {
      a = { fg = P.black[1], bg = P.frost[1], gui = 'bold' },
      b = { fg = P.text[1], bg = P.grey4[1] },
      c = { fg = P.grey0[1], bg = P.grey5[1] },
    },
    terminal = {
      a = { fg = P.black[1], bg = P.sunny[1], gui = 'bold' },
      b = { fg = P.text[1], bg = P.grey4[1] },
      c = { fg = P.grey0[1], bg = P.grey5[1] },
    },
    inactive = {
      a = { fg = P.grey1[1], bg = P.grey5[1] },
      b = { fg = P.grey1[1], bg = P.grey5[1] },
      c = { fg = P.grey2[1], bg = P.grey5[1] },
    },
  }
end

function M.load(name)
  name = name or 'harlequin'
  local P = M.palette(name)

  vim.cmd.highlight('clear')
  if vim.fn.exists('syntax_on') == 1 then
    vim.cmd.syntax('reset')
  end
  vim.o.background = 'dark'
  vim.g.colors_name = name

  local function hi(name, spec)
    local val = {}
    if spec.link then
      vim.api.nvim_set_hl(0, name, { link = spec.link, default = spec.default })
      return
    end
    if spec.fg then
      val.fg = spec.fg[1]
      val.ctermfg = spec.fg[2]
    end
    if spec.bg then
      val.bg = spec.bg[1]
      val.ctermbg = spec.bg[2]
    end
    if spec.sp then
      val.sp = spec.sp[1]
    end
    for _, key in ipairs({
      'bold', 'italic', 'underline', 'undercurl', 'underdotted',
      'underdashed', 'strikethrough', 'reverse', 'nocombine',
      'standout', 'altfont',
    }) do
      if spec[key] then
        val[key] = true
      end
    end
    vim.api.nvim_set_hl(0, name, val)
  end
  
  -- ---------------------------------------------------------------------------
  -- Editor UI
  -- ---------------------------------------------------------------------------
  hi('Normal',       { fg = P.text, bg = P.bg })
  hi('NormalNC',     { fg = P.text, bg = P.bg })
  hi('NormalFloat',  { fg = P.text, bg = P.grey5 })
  hi('FloatBorder',  { fg = P.grey1, bg = P.grey5 })
  hi('FloatTitle',   { fg = P.gold, bg = P.grey5, bold = true })
  hi('FloatFooter',  { fg = P.grey2, bg = P.grey5 })
  hi('FloatShadow',  { bg = P.black })
  hi('FloatShadowThrough', { bg = P.black })
  
  hi('NormalSB',     { fg = P.text, bg = P.grey5 })
  hi('MsgArea',      { fg = P.text, bg = P.bg })
  hi('MsgSeparator', { fg = P.grey3, bg = P.bg })
  
  hi('LineNr',       { fg = P.grey2 })
  hi('LineNrAbove',  { fg = P.grey2 })
  hi('LineNrBelow',  { fg = P.grey2 })
  hi('CursorLineNr', { fg = P.gold, bold = true })
  hi('CursorLine',   { bg = P.grey5 })
  hi('CursorColumn', { bg = P.grey5 })
  hi('ColorColumn',  { bg = P.grey5 })
  hi('SignColumn',   { fg = P.grey2, bg = P.grey5 })
  hi('CursorLineSign', { fg = P.grey2, bg = P.grey5 })
  hi('CursorLineFold', { fg = P.white, bg = P.grey4 })
  hi('FoldColumn',   { fg = P.white, bg = P.grey4 })
  hi('Folded',       { fg = P.white, bg = P.grey4 })
  hi('NonText',      { fg = P.grey2 })
  hi('EndOfBuffer',  { fg = P.grey2 })
  hi('Whitespace',   { fg = P.grey3 })
  hi('SpecialKey',   { fg = P.lilac, bold = true })
  hi('Conceal',      { fg = P.grey2 })
  
  hi('Cursor',       { fg = P.grey5, bg = P.white })
  hi('lCursor',      { fg = P.grey5, bg = P.white })
  hi('vCursor',      { fg = P.grey5, bg = P.white })
  hi('iCursor',      { fg = P.grey5, bg = P.white })
  hi('TermCursor',   { fg = P.grey5, bg = P.white })
  hi('TermCursorNC', { fg = P.grey5, bg = P.grey1 })
  
  hi('Visual',       { bg = P.grey4 })
  hi('VisualNOS',    { bg = P.grey4 })
  hi('Search',       { fg = P.black, bg = P.frost })
  hi('IncSearch',    { fg = P.black, bg = P.sunny })
  hi('CurSearch',    { fg = P.black, bg = P.gold })
  hi('Substitute',   { fg = P.black, bg = P.cerise })
  hi('MatchParen',   { fg = P.black, bg = P.gold, bold = true })
  
  hi('StatusLine',   { fg = P.white, bg = P.black, bold = true })
  hi('StatusLineNC', { fg = P.grey1, bg = P.grey5, bold = true })
  hi('StatusLineTerm',   { fg = P.white, bg = P.black, bold = true })
  hi('StatusLineTermNC', { fg = P.grey1, bg = P.grey5 })
  hi('WinSeparator', { fg = P.grey1, bg = P.grey5, bold = true })
  hi('VertSplit',    { fg = P.grey1, bg = P.grey5, bold = true })
  hi('WinBar',       { fg = P.grey0, bg = P.bg, bold = true })
  hi('WinBarNC',     { fg = P.grey2, bg = P.bg })
  hi('TabLine',      { fg = P.grey0, bg = P.grey5 })
  hi('TabLineSel',   { fg = P.black, bg = P.gold, bold = true })
  hi('TabLineFill',  { fg = P.grey2, bg = P.black })
  
  hi('Pmenu',        { fg = P.frost, bg = P.grey5 })
  hi('PmenuSel',     { fg = P.gold, bg = P.black, bold = true })
  hi('PmenuKind',    { fg = P.lilac, bg = P.grey5 })
  hi('PmenuKindSel', { fg = P.lilac, bg = P.black, bold = true })
  hi('PmenuExtra',   { fg = P.grey2, bg = P.grey5 })
  hi('PmenuExtraSel',{ fg = P.grey1, bg = P.black })
  hi('PmenuSbar',    { bg = P.grey4 })
  hi('PmenuThumb',   { bg = P.grey1 })
  hi('PmenuMatch',   { fg = P.sunny, bg = P.grey5 })
  hi('PmenuMatchSel',{ fg = P.sunny, bg = P.black, bold = true })
  hi('WildMenu',     { fg = P.black, bg = P.lilac, bold = true })
  hi('ComplMatchIns',{ fg = P.grey2 })
  
  hi('Error',        { fg = P.mordant, bg = P.grey5, bold = true })
  hi('ErrorMsg',     { fg = P.mordant, bold = true })
  hi('WarningMsg',   { fg = P.mordant })
  hi('ModeMsg',      { fg = P.sunny, bold = true })
  hi('MoreMsg',      { fg = P.lime })
  hi('Question',     { fg = P.lime })
  hi('Directory',    { fg = P.lime })
  hi('Title',        { underline = true })
  hi('QuickFixLine', { bg = P.grey4, bold = true })
  hi('qfLineNr',     { fg = P.gold })
  hi('qfFileName',   { fg = P.lime })
  hi('qfText',       { fg = P.text })
  
  hi('DiffAdd',      { bg = P.moss })
  hi('DiffChange',   { bg = P.grey4 })
  hi('DiffDelete',   { fg = P.auburn, bg = P.auburn })
  hi('DiffText',     { bg = P.deep_blue, bold = true })
  hi('DiffTextAdd',  { link = 'DiffText' })
  hi('Added',        { fg = P.lime })
  hi('Changed',      { fg = P.gold })
  hi('Removed',      { fg = P.cerise })
  
  hi('SpellBad',     { sp = P.mordant, undercurl = true })
  hi('SpellCap',     { sp = P.auburn, undercurl = true })
  hi('SpellLocal',   { sp = P.auburn, undercurl = true })
  hi('SpellRare',    { sp = P.white, undercurl = true })
  
  hi('Underlined',   { underline = true })
  hi('Ignore',       { fg = P.lime })
  hi('Todo',         { fg = P.frost, bold = true })
  hi('Tag',          { bold = true })
  
  -- ---------------------------------------------------------------------------
  -- Syntax (classic groups — kept faithful to the original scheme)
  -- ---------------------------------------------------------------------------
  hi('Statement',    { fg = P.cerise, bold = true })
  hi('Keyword',      { fg = P.cerise, bold = true })
  hi('Conditional',  { fg = P.cerise, bold = true })
  hi('Repeat',       { fg = P.cerise, bold = true })
  hi('Operator',     { fg = P.cerise })
  hi('Label',        { fg = P.cerise })
  hi('Exception',    { fg = P.lime, bold = true })
  
  hi('Type',         { fg = P.brick })
  hi('StorageClass', { fg = P.cerise })
  hi('Structure',    { fg = P.cerise })
  hi('Typedef',      { fg = P.cerise, bold = true })
  
  hi('PreProc',      { fg = P.lime })
  hi('Include',      { fg = P.lime, bold = true })
  hi('Define',       { fg = P.lime })
  hi('Macro',        { fg = P.lime })
  hi('PreCondit',    { fg = P.lime, bold = true })
  hi('Debug',        { fg = P.lime, bold = true })
  
  hi('Identifier',   { fg = P.gold })
  hi('Function',     { fg = P.gold })
  
  hi('Comment',      { fg = P.frost })
  hi('CommentEmail', { fg = P.frost, underline = true })
  hi('CommentUrl',   { fg = P.frost, underline = true })
  hi('SpecialComment', { fg = P.frost, bold = true })
  
  hi('String',       { fg = P.sunny })
  hi('Character',    { fg = P.lilac, bold = true })
  hi('Number',       { fg = P.lilac, bold = true })
  hi('Boolean',      { fg = P.lilac, bold = true })
  hi('Float',        { fg = P.lilac, bold = true })
  hi('Constant',     { fg = P.lilac, bold = true })
  
  hi('Special',      { fg = P.lilac, bold = true })
  hi('SpecialChar',  { fg = P.lilac, bold = true })
  hi('Delimiter',    { fg = P.lime })
  
  -- ---------------------------------------------------------------------------
  -- Treesitter
  -- Variables stay on the default foreground so a buffer is not a wall of gold.
  -- Functions, types, and keywords keep the original Harlequin accents.
  -- ---------------------------------------------------------------------------
  hi('@variable',            { fg = P.text })
  hi('@variable.builtin',    { fg = P.brick })
  hi('@variable.parameter',  { fg = P.gold })
  hi('@variable.parameter.builtin', { fg = P.lilac })
  hi('@variable.member',     { fg = P.gold })
  
  hi('@constant',            { link = 'Constant' })
  hi('@constant.builtin',    { fg = P.lilac, bold = true })
  hi('@constant.macro',      { link = 'Macro' })
  
  hi('@module',              { fg = P.lime })
  hi('@module.builtin',      { fg = P.lime, bold = true })
  hi('@label',               { link = 'Label' })
  
  hi('@string',              { link = 'String' })
  hi('@string.documentation',{ fg = P.sunny })
  hi('@string.regexp',       { fg = P.lilac })
  hi('@string.escape',       { fg = P.lilac, bold = true })
  hi('@string.special',      { fg = P.lilac, bold = true })
  hi('@string.special.symbol', { fg = P.lilac })
  hi('@string.special.url',  { fg = P.frost, underline = true })
  hi('@string.special.path', { fg = P.lime })
  
  hi('@character',           { link = 'Character' })
  hi('@character.special',   { link = 'SpecialChar' })
  
  hi('@boolean',             { link = 'Boolean' })
  hi('@number',              { link = 'Number' })
  hi('@number.float',        { link = 'Float' })
  
  hi('@type',                { link = 'Type' })
  hi('@type.builtin',        { fg = P.brick, bold = true })
  hi('@type.definition',     { link = 'Typedef' })
  hi('@type.qualifier',      { fg = P.cerise })
  
  hi('@attribute',           { fg = P.cerise })
  hi('@attribute.builtin',   { fg = P.cerise, bold = true })
  hi('@property',            { fg = P.gold })
  
  hi('@function',            { link = 'Function' })
  hi('@function.builtin',    { fg = P.gold, bold = true })
  hi('@function.call',       { link = 'Function' })
  hi('@function.macro',      { link = 'Macro' })
  hi('@function.method',     { link = 'Function' })
  hi('@function.method.call',{ link = 'Function' })
  hi('@constructor',         { fg = P.gold })
  hi('@operator',            { link = 'Operator' })
  
  hi('@keyword',             { link = 'Keyword' })
  hi('@keyword.coroutine',   { fg = P.cerise, bold = true })
  hi('@keyword.function',    { fg = P.cerise, bold = true })
  hi('@keyword.operator',    { fg = P.cerise })
  hi('@keyword.import',      { fg = P.lime, bold = true })
  hi('@keyword.type',        { fg = P.cerise })
  hi('@keyword.modifier',    { fg = P.cerise })
  hi('@keyword.repeat',      { link = 'Repeat' })
  hi('@keyword.return',      { fg = P.cerise, bold = true })
  hi('@keyword.debug',       { link = 'Debug' })
  hi('@keyword.exception',   { link = 'Exception' })
  hi('@keyword.conditional', { link = 'Conditional' })
  hi('@keyword.conditional.ternary', { fg = P.cerise })
  hi('@keyword.directive',   { link = 'PreProc' })
  hi('@keyword.directive.define', { link = 'Define' })
  
  hi('@punctuation.delimiter', { fg = P.text })
  hi('@punctuation.bracket',   { fg = P.text })
  hi('@punctuation.special',   { fg = P.lilac, bold = true })
  
  hi('@comment',             { link = 'Comment' })
  hi('@comment.documentation', { fg = P.frost, italic = true })
  hi('@comment.error',       { fg = P.mordant, bold = true })
  hi('@comment.warning',     { fg = P.gold, bold = true })
  hi('@comment.todo',        { fg = P.frost, bold = true })
  hi('@comment.note',        { fg = P.frost, italic = true })
  
  hi('@markup',              { fg = P.text })
  hi('@markup.strong',       { bold = true })
  hi('@markup.italic',       { italic = true })
  hi('@markup.strikethrough',{ strikethrough = true })
  hi('@markup.underline',    { underline = true })
  hi('@markup.heading',      { fg = P.gold, bold = true })
  hi('@markup.heading.1',    { fg = P.cerise, bold = true })
  hi('@markup.heading.2',    { fg = P.gold, bold = true })
  hi('@markup.heading.3',    { fg = P.lime, bold = true })
  hi('@markup.heading.4',    { fg = P.frost, italic = true })
  hi('@markup.heading.5',    { fg = P.lilac, italic = true })
  hi('@markup.heading.6',    { fg = P.brick, italic = true })
  hi('@markup.quote',        { fg = P.frost, italic = true })
  hi('@markup.math',         { fg = P.lilac })
  hi('@markup.link',         { fg = P.frost })
  hi('@markup.link.label',   { fg = P.gold })
  hi('@markup.link.url',     { fg = P.frost, underline = true })
  hi('@markup.raw',          { fg = P.sunny })
  hi('@markup.raw.block',    { fg = P.sunny })
  hi('@markup.list',         { fg = P.cerise })
  hi('@markup.list.checked', { fg = P.lime })
  hi('@markup.list.unchecked', { fg = P.grey1 })
  
  hi('@diff.plus',           { fg = P.lime })
  hi('@diff.minus',          { fg = P.cerise })
  hi('@diff.delta',          { fg = P.gold })
  
  hi('@tag',                 { fg = P.gold })
  hi('@tag.builtin',         { fg = P.gold })
  hi('@tag.attribute',       { fg = P.brick })
  hi('@tag.delimiter',       { fg = P.lime })
  
  -- Legacy TS* groups (nvim-treesitter < 0.8 / some plugins)
  hi('TSVariable',           { link = '@variable' })
  hi('TSFunction',           { link = '@function' })
  hi('TSKeyword',            { link = '@keyword' })
  hi('TSString',             { link = '@string' })
  hi('TSComment',            { link = '@comment' })
  hi('TSType',               { link = '@type' })
  hi('TSConstant',           { link = '@constant' })
  hi('TSParameter',          { link = '@variable.parameter' })
  hi('TSField',              { link = '@variable.member' })
  hi('TSProperty',           { link = '@property' })
  hi('TSConstructor',        { link = '@constructor' })
  hi('TSTag',                { link = '@tag' })
  
  -- ---------------------------------------------------------------------------
  -- LSP / diagnostics / semantic tokens
  -- ---------------------------------------------------------------------------
  hi('DiagnosticError',            { fg = P.mordant })
  hi('DiagnosticWarn',             { fg = P.gold })
  hi('DiagnosticInfo',             { fg = P.frost })
  hi('DiagnosticHint',             { fg = P.frost })
  hi('DiagnosticOk',              { fg = P.lime })
  hi('DiagnosticUnnecessary',     { fg = P.grey2, undercurl = true, sp = P.frost })
  hi('DiagnosticDeprecated',      { strikethrough = true, sp = P.grey1 })
  
  hi('DiagnosticUnderlineError',  { undercurl = true, sp = P.mordant })
  hi('DiagnosticUnderlineWarn',   { undercurl = true, sp = P.gold })
  hi('DiagnosticUnderlineInfo',   { undercurl = true, sp = P.frost })
  hi('DiagnosticUnderlineHint',   { undercurl = true, sp = P.frost })
  hi('DiagnosticUnderlineOk',     { undercurl = true, sp = P.lime })
  
  hi('DiagnosticVirtualTextError', { fg = P.mordant, bg = P.grey5 })
  hi('DiagnosticVirtualTextWarn',  { fg = P.gold, bg = P.grey5 })
  hi('DiagnosticVirtualTextInfo',  { fg = P.frost, bg = P.grey5 })
  hi('DiagnosticVirtualTextHint',  { fg = P.frost, bg = P.grey5 })
  hi('DiagnosticVirtualTextOk',    { fg = P.lime, bg = P.grey5 })
  
  hi('DiagnosticFloatingError',   { fg = P.mordant, bg = P.grey5 })
  hi('DiagnosticFloatingWarn',    { fg = P.gold, bg = P.grey5 })
  hi('DiagnosticFloatingInfo',    { fg = P.frost, bg = P.grey5 })
  hi('DiagnosticFloatingHint',    { fg = P.frost, bg = P.grey5 })
  hi('DiagnosticFloatingOk',      { fg = P.lime, bg = P.grey5 })
  
  hi('DiagnosticSignError',       { fg = P.mordant, bg = P.grey5 })
  hi('DiagnosticSignWarn',        { fg = P.gold, bg = P.grey5 })
  hi('DiagnosticSignInfo',        { fg = P.frost, bg = P.grey5 })
  hi('DiagnosticSignHint',        { fg = P.frost, bg = P.grey5 })
  hi('DiagnosticSignOk',          { fg = P.lime, bg = P.grey5 })
  
  hi('LspReferenceText',          { bg = P.grey4 })
  hi('LspReferenceRead',          { bg = P.moss })
  hi('LspReferenceWrite',         { bg = P.deep_blue, bold = true })
  hi('LspReferenceTarget',        { bg = P.grey4 })
  hi('LspSignatureActiveParameter', { fg = P.gold, bold = true })
  hi('LspCodeLens',               { fg = P.grey2, italic = true })
  hi('LspCodeLensSeparator',      { fg = P.grey3 })
  hi('LspInlayHint',              { fg = P.grey2, bg = P.grey5, italic = true })
  
  hi('@lsp.type.class',           { link = '@type' })
  hi('@lsp.type.comment',         { link = '@comment' })
  hi('@lsp.type.decorator',       { link = '@attribute' })
  hi('@lsp.type.enum',            { link = '@type' })
  hi('@lsp.type.enumMember',      { link = '@constant' })
  hi('@lsp.type.event',           { fg = P.gold })
  hi('@lsp.type.function',        { link = '@function' })
  hi('@lsp.type.interface',       { fg = P.brick, italic = true })
  hi('@lsp.type.keyword',         { link = '@keyword' })
  hi('@lsp.type.macro',           { link = '@function.macro' })
  hi('@lsp.type.method',          { link = '@function.method' })
  hi('@lsp.type.modifier',        { link = '@keyword.modifier' })
  hi('@lsp.type.namespace',       { link = '@module' })
  hi('@lsp.type.number',          { link = '@number' })
  hi('@lsp.type.operator',        { link = '@operator' })
  hi('@lsp.type.parameter',       { link = '@variable.parameter' })
  hi('@lsp.type.property',        { link = '@property' })
  hi('@lsp.type.regexp',          { link = '@string.regexp' })
  hi('@lsp.type.string',          { link = '@string' })
  hi('@lsp.type.struct',          { link = '@type' })
  hi('@lsp.type.type',            { link = '@type' })
  hi('@lsp.type.typeParameter',   { fg = P.brick, italic = true })
  hi('@lsp.type.variable',        { link = '@variable' })
  hi('@lsp.mod.deprecated',       { strikethrough = true })
  hi('@lsp.typemod.function.defaultLibrary', { link = '@function.builtin' })
  hi('@lsp.typemod.variable.defaultLibrary', { link = '@variable.builtin' })
  hi('@lsp.typemod.variable.global', { fg = P.lilac })
  
  -- nvim-lspconfig / mason use these for UI chrome
  hi('LspInfoBorder',             { link = 'FloatBorder' })
  hi('LspInfoTitle',              { link = 'FloatTitle' })
  hi('LspInfoList',               { fg = P.gold })
  hi('LspInfoTip',                { link = 'Comment' })
  hi('LspInfoFiletype',           { fg = P.brick })
  
  -- ---------------------------------------------------------------------------
  -- Language leftovers from the original scheme
  -- ---------------------------------------------------------------------------
  hi('pythonDecorator',      { fg = P.cerise })
  hi('pythonException',      { fg = P.lime, bold = true })
  hi('pythonExceptions',     { fg = P.lime })
  
  hi('rubyModule',           { fg = P.lime })
  hi('rubyModuleNameTag',    { fg = P.text })
  hi('rubyPseudoVariable',   { fg = P.text })
  hi('rubyClass',            { fg = P.cerise })
  hi('rubyClassNameTag',     { fg = P.gold })
  hi('rubyDefine',           { fg = P.cerise })
  hi('rubyConstant',         { fg = P.text })
  hi('rubyStringDelimiter',  { fg = P.sunny })
  hi('rubyInterpolation',    { fg = P.lilac })
  hi('rubyInterpolationDelimiter', { fg = P.lilac })
  
  hi('javaScriptNull',       { fg = P.lilac, bold = true })
  hi('javaScriptNumber',     { fg = P.lilac, bold = true })
  hi('javaScriptFunction',   { fg = P.cerise })
  hi('javaScriptOperator',   { fg = P.cerise, bold = true })
  hi('javaScriptBraces',     { fg = P.text })
  hi('javaScriptIdentifier', { fg = P.brick })
  hi('javaScriptMember',     { fg = P.gold })
  hi('javaScriptType',       { fg = P.gold })
  hi('javaScript',           { fg = P.text })
  
  hi('clojureDefine',        { fg = P.cerise })
  hi('clojureSpecial',       { fg = P.cerise })
  hi('clojureCond',          { fg = P.cerise })
  hi('clojureParen0',        { fg = P.text })
  hi('clojureMacro',         { fg = P.lime, bold = true })
  hi('clojureDispatch',      { fg = P.lilac, bold = true })
  hi('clojureError',         { fg = P.black, bg = P.mordant, bold = true })
  
  hi('scalaClassName',       { fg = P.gold })
  hi('scalaConstructor',     { fg = P.text })
  
  hi('vimCommentTitle',      { fg = P.frost, bold = true })
  hi('vimParenSep',          { fg = P.text })
  hi('vimSep',               { fg = P.text })
  hi('vimOper',              { fg = P.text })
  
  hi('xmlProcessingDelim',   { fg = P.brick })
  hi('xmlNamespace',         { fg = P.gold })
  hi('xmlTag',               { fg = P.gold })
  hi('xmlTagName',           { fg = P.gold })
  hi('xmlEndTag',            { fg = P.gold })
  hi('xmlAttrib',            { fg = P.brick })
  hi('xmlAttribPunct',       { fg = P.brick })
  hi('xmlEntity',            { fg = P.lilac, bold = true })
  hi('xmlEntityPunct',       { fg = P.lilac })
  
  hi('htmlTagName',          { fg = P.gold })
  hi('htmlTag',              { fg = P.gold })
  hi('htmlTagN',             { fg = P.gold })
  hi('htmlEndTag',           { fg = P.gold })
  hi('htmlEvent',            { fg = P.brick })
  hi('htmlEventDQ',          { fg = P.lime })
  hi('htmlH1',               { bold = true })
  hi('htmlH2',               { bold = true })
  hi('htmlH3',               { italic = true })
  hi('htmlH4',               { italic = true })
  hi('htmlScriptTag',        { fg = P.lime })
  hi('htmlArg',              { fg = P.brick })
  hi('htmlString',           { link = 'String' })
  hi('htmlLink',             { fg = P.frost, underline = true })
  hi('htmlBold',             { bold = true })
  hi('htmlItalic',           { italic = true })
  
  hi('cssSelectorOp',        { fg = P.text })
  hi('cssSelectorOp2',       { fg = P.text })
  hi('cssBraces',            { fg = P.text })
  hi('cssPseudoClass',       { fg = P.lime })
  hi('cssValueNumber',       { fg = P.lilac })
  hi('cssValueLength',       { fg = P.lilac })
  hi('cssColor',             { fg = P.lilac })
  hi('cssImportant',         { fg = P.lime, bold = true })
  hi('cssCommonAttr',        { fg = P.lilac, bold = true })
  hi('cssRenderAttr',        { fg = P.lilac, bold = true })
  hi('cssBoxAttr',           { fg = P.lilac, bold = true })
  hi('cssUIAttr',            { fg = P.lilac, bold = true })
  hi('cssTextAttr',          { fg = P.lilac, bold = true })
  hi('cssTableAttr',         { fg = P.lilac, bold = true })
  hi('cssColorAttr',         { fg = P.lilac, bold = true })
  
  hi('markdownCode',         { fg = P.sunny })
  hi('markdownCodeBlock',    { fg = P.sunny })
  hi('markdownH1',           { fg = P.cerise, bold = true })
  hi('markdownH2',           { fg = P.gold, bold = true })
  hi('markdownH3',           { fg = P.lime, bold = true })
  hi('markdownUrl',          { fg = P.frost, underline = true })
  hi('markdownLinkText',     { fg = P.gold })
  
  -- ---------------------------------------------------------------------------
  -- Original plugin support
  -- ---------------------------------------------------------------------------
  hi('EasyMotionTarget',     { fg = P.cerise, bold = true })
  hi('EasyMotionShade',      { fg = P.grey2 })
  
  hi('CocHighlightText',     { bg = P.grey4 })
  hi('CocHighlightRead',     { bg = P.moss })
  hi('CocHighlightWrite',    { bg = P.deep_blue, bold = true })
  hi('CocErrorHighlight',    { undercurl = true, sp = P.mordant })
  hi('CocWarningHighlight',  { undercurl = true, sp = P.gold })
  hi('CocInfoHighlight',     { undercurl = true, sp = P.frost })
  hi('CocHintHighlight',     { undercurl = true, sp = P.frost })
  hi('CocUnusedHighlight',   { undercurl = true, sp = P.frost })
  hi('CocErrorSign',         { link = 'DiagnosticSignError' })
  hi('CocWarningSign',       { link = 'DiagnosticSignWarn' })
  hi('CocInfoSign',          { link = 'DiagnosticSignInfo' })
  hi('CocHintSign',          { link = 'DiagnosticSignHint' })
  hi('CocFadeOut',           { fg = P.grey2 })
  hi('CocMenuSel',           { link = 'PmenuSel' })
  hi('CocFloating',          { link = 'NormalFloat' })
  hi('CocFloatBorder',       { link = 'FloatBorder' })
  
  -- ---------------------------------------------------------------------------
  -- lazy.nvim
  -- ---------------------------------------------------------------------------
  hi('LazyNormal',           { fg = P.text, bg = P.grey5 })
  hi('LazyButton',           { fg = P.text, bg = P.grey4 })
  hi('LazyButtonActive',     { fg = P.black, bg = P.gold, bold = true })
  hi('LazyH1',               { fg = P.black, bg = P.gold, bold = true })
  hi('LazyH2',               { fg = P.gold, bold = true })
  hi('LazyComment',          { fg = P.frost })
  hi('LazyCommit',           { fg = P.lilac })
  hi('LazyCommitIssue',      { fg = P.cerise })
  hi('LazyCommitScope',      { fg = P.brick, italic = true })
  hi('LazyCommitType',       { fg = P.lime, bold = true })
  hi('LazyDimmed',           { fg = P.grey2 })
  hi('LazyDir',              { fg = P.lime })
  hi('LazyProgressDone',     { fg = P.lime, bold = true })
  hi('LazyProgressTodo',     { fg = P.grey3 })
  hi('LazyProp',             { fg = P.grey1 })
  hi('LazyReasonCmd',        { fg = P.sunny })
  hi('LazyReasonEvent',      { fg = P.gold })
  hi('LazyReasonFt',         { fg = P.brick })
  hi('LazyReasonImport',     { fg = P.lilac })
  hi('LazyReasonKeys',       { fg = P.cerise })
  hi('LazyReasonPlugin',     { fg = P.frost })
  hi('LazyReasonRequire',    { fg = P.lilac })
  hi('LazyReasonRuntime',    { fg = P.grey1 })
  hi('LazyReasonSource',     { fg = P.lime })
  hi('LazyReasonStart',      { fg = P.gold })
  hi('LazySpecial',          { fg = P.lilac })
  hi('LazyTaskError',        { fg = P.mordant })
  hi('LazyTaskOutput',       { fg = P.text })
  hi('LazyUrl',              { fg = P.frost, underline = true })
  hi('LazyValue',            { fg = P.sunny })
  hi('LazyNoCond',           { fg = P.grey2 })
  hi('LazyLocal',            { fg = P.lime })
  
  -- ---------------------------------------------------------------------------
  -- mason.nvim / mason-lspconfig.nvim
  -- ---------------------------------------------------------------------------
  hi('MasonHeader',                { fg = P.black, bg = P.gold, bold = true })
  hi('MasonHeaderSecondary',       { fg = P.black, bg = P.frost, bold = true })
  hi('MasonHighlight',             { fg = P.frost })
  hi('MasonHighlightBlock',        { fg = P.black, bg = P.frost })
  hi('MasonHighlightBlockBold',    { fg = P.black, bg = P.frost, bold = true })
  hi('MasonHighlightSecondary',    { fg = P.gold })
  hi('MasonHighlightBlockSecondary', { fg = P.black, bg = P.gold })
  hi('MasonHighlightBlockBoldSecondary', { fg = P.black, bg = P.gold, bold = true })
  hi('MasonMuted',                 { fg = P.grey1 })
  hi('MasonMutedBlock',            { fg = P.grey0, bg = P.grey4 })
  hi('MasonMutedBlockBold',        { fg = P.grey0, bg = P.grey4, bold = true })
  hi('MasonError',                 { fg = P.mordant })
  hi('MasonWarning',               { fg = P.gold })
  hi('MasonHeading',               { fg = P.gold, bold = true })
  hi('MasonLink',                  { fg = P.frost, underline = true })
  
  -- ---------------------------------------------------------------------------
  -- telescope.nvim + fzf-native + file-browser
  -- ---------------------------------------------------------------------------
  hi('TelescopeNormal',            { fg = P.text, bg = P.grey5 })
  hi('TelescopeBorder',            { fg = P.grey1, bg = P.grey5 })
  hi('TelescopePromptNormal',      { fg = P.text, bg = P.grey5 })
  hi('TelescopePromptBorder',      { fg = P.gold, bg = P.grey5 })
  hi('TelescopePromptTitle',       { fg = P.black, bg = P.gold, bold = true })
  hi('TelescopePromptPrefix',      { fg = P.cerise, bg = P.grey5 })
  hi('TelescopePromptCounter',     { fg = P.grey2 })
  hi('TelescopeResultsNormal',     { fg = P.text, bg = P.grey5 })
  hi('TelescopeResultsBorder',     { fg = P.grey1, bg = P.grey5 })
  hi('TelescopeResultsTitle',      { fg = P.black, bg = P.frost, bold = true })
  hi('TelescopeResultsComment',    { fg = P.grey2 })
  hi('TelescopeResultsIdentifier', { fg = P.gold })
  hi('TelescopeResultsFunction',   { fg = P.gold })
  hi('TelescopeResultsClass',      { fg = P.brick })
  hi('TelescopeResultsConstant',   { fg = P.lilac })
  hi('TelescopeResultsNumber',     { fg = P.lilac })
  hi('TelescopeResultsOperator',   { fg = P.cerise })
  hi('TelescopeResultsStruct',     { fg = P.brick })
  hi('TelescopeResultsVariable',   { fg = P.text })
  hi('TelescopeResultsDiffAdd',    { fg = P.lime })
  hi('TelescopeResultsDiffChange', { fg = P.gold })
  hi('TelescopeResultsDiffDelete', { fg = P.cerise })
  hi('TelescopePreviewNormal',     { fg = P.text, bg = P.grey5 })
  hi('TelescopePreviewBorder',     { fg = P.grey1, bg = P.grey5 })
  hi('TelescopePreviewTitle',      { fg = P.black, bg = P.lime, bold = true })
  hi('TelescopePreviewLine',       { bg = P.grey4 })
  hi('TelescopePreviewMatch',      { fg = P.black, bg = P.sunny })
  hi('TelescopePreviewDirectory',  { fg = P.lime })
  hi('TelescopeMatching',          { fg = P.sunny, bold = true })
  hi('TelescopeSelection',         { fg = P.gold, bg = P.black, bold = true })
  hi('TelescopeSelectionCaret',    { fg = P.cerise, bg = P.black, bold = true })
  hi('TelescopeMultiSelection',    { fg = P.lilac, bg = P.grey4 })
  hi('TelescopeMultiIcon',         { fg = P.lilac })
  hi('TelescopeTitle',             { fg = P.gold, bold = true })
  hi('TelescopePathSeparator',     { fg = P.grey3 })
  
  hi('TelescopeFileBrowserTitle',  { fg = P.black, bg = P.lime, bold = true })
  hi('TelescopePromptBorderFileBrowser', { fg = P.lime, bg = P.grey5 })
  
  -- ---------------------------------------------------------------------------
  -- blink.cmp + LuaSnip / friendly-snippets
  -- ---------------------------------------------------------------------------
  hi('BlinkCmpMenu',               { fg = P.text, bg = P.grey5 })
  hi('BlinkCmpMenuBorder',         { fg = P.grey1, bg = P.grey5 })
  hi('BlinkCmpMenuSelection',      { fg = P.gold, bg = P.black, bold = true })
  hi('BlinkCmpScrollBarThumb',     { bg = P.grey1 })
  hi('BlinkCmpScrollBarGutter',    { bg = P.grey4 })
  hi('BlinkCmpLabel',              { fg = P.text })
  hi('BlinkCmpLabelDeprecated',    { fg = P.grey2, strikethrough = true })
  hi('BlinkCmpLabelMatch',         { fg = P.sunny, bold = true })
  hi('BlinkCmpLabelDetail',        { fg = P.grey2 })
  hi('BlinkCmpLabelDescription',   { fg = P.grey2 })
  hi('BlinkCmpSource',             { fg = P.grey1 })
  hi('BlinkCmpGhostText',          { fg = P.grey3 })
  hi('BlinkCmpDoc',                { fg = P.text, bg = P.grey5 })
  hi('BlinkCmpDocBorder',          { fg = P.grey1, bg = P.grey5 })
  hi('BlinkCmpDocSeparator',       { fg = P.grey3, bg = P.grey5 })
  hi('BlinkCmpDocCursorLine',      { bg = P.grey4 })
  hi('BlinkCmpSignatureHelp',      { fg = P.text, bg = P.grey5 })
  hi('BlinkCmpSignatureHelpBorder',{ fg = P.grey1, bg = P.grey5 })
  hi('BlinkCmpSignatureHelpActiveParameter', { fg = P.gold, bold = true })
  hi('BlinkCmpKind',               { fg = P.grey0 })
  hi('BlinkCmpKindText',           { fg = P.text })
  hi('BlinkCmpKindMethod',         { fg = P.gold })
  hi('BlinkCmpKindFunction',       { fg = P.gold })
  hi('BlinkCmpKindConstructor',    { fg = P.gold })
  hi('BlinkCmpKindField',          { fg = P.gold })
  hi('BlinkCmpKindVariable',       { fg = P.text })
  hi('BlinkCmpKindClass',          { fg = P.brick })
  hi('BlinkCmpKindInterface',      { fg = P.brick })
  hi('BlinkCmpKindModule',         { fg = P.lime })
  hi('BlinkCmpKindProperty',       { fg = P.gold })
  hi('BlinkCmpKindUnit',           { fg = P.lilac })
  hi('BlinkCmpKindValue',          { fg = P.lilac })
  hi('BlinkCmpKindEnum',           { fg = P.brick })
  hi('BlinkCmpKindKeyword',        { fg = P.cerise })
  hi('BlinkCmpKindSnippet',        { fg = P.sunny })
  hi('BlinkCmpKindColor',          { fg = P.lilac })
  hi('BlinkCmpKindFile',           { fg = P.frost })
  hi('BlinkCmpKindReference',      { fg = P.frost })
  hi('BlinkCmpKindFolder',         { fg = P.lime })
  hi('BlinkCmpKindEnumMember',     { fg = P.lilac })
  hi('BlinkCmpKindConstant',       { fg = P.lilac })
  hi('BlinkCmpKindStruct',         { fg = P.brick })
  hi('BlinkCmpKindEvent',          { fg = P.gold })
  hi('BlinkCmpKindOperator',       { fg = P.cerise })
  hi('BlinkCmpKindTypeParameter',  { fg = P.brick })
  hi('BlinkCmpKindCopilot',        { fg = P.frost })
  hi('BlinkCmpKindCodeium',        { fg = P.frost })
  hi('BlinkCmpKindSupermaven',     { fg = P.frost })
  hi('BlinkCmpKindTabNine',        { fg = P.frost })
  hi('BlinkCmpKindDefault',        { fg = P.grey0 })
  
  hi('SnippetTabstop',             { bg = P.grey4 })
  hi('LuasnipChoiceNode',          { fg = P.gold, bg = P.grey4 })
  hi('LuasnipInsertNode',          { fg = P.frost, bg = P.grey4 })
  
  -- ---------------------------------------------------------------------------
  -- conform.nvim / nvim-lint  (diagnostics already cover lint; extra UI)
  -- ---------------------------------------------------------------------------
  hi('ConformDone',                { fg = P.lime })
  hi('ConformError',               { fg = P.mordant })
  hi('LintError',                  { link = 'DiagnosticError' })
  hi('LintWarn',                   { link = 'DiagnosticWarn' })
  hi('LintInfo',                   { link = 'DiagnosticInfo' })
  hi('LintHint',                   { link = 'DiagnosticHint' })
  
  -- ---------------------------------------------------------------------------
  -- which-key.nvim
  -- ---------------------------------------------------------------------------
  hi('WhichKey',                   { fg = P.sunny })
  hi('WhichKeyGroup',              { fg = P.frost })
  hi('WhichKeyDesc',               { fg = P.text })
  hi('WhichKeySeparator',          { fg = P.grey2 })
  hi('WhichKeyBorder',             { fg = P.grey1, bg = P.grey5 })
  hi('WhichKeyNormal',             { fg = P.text, bg = P.grey5 })
  hi('WhichKeyTitle',              { fg = P.gold, bg = P.grey5, bold = true })
  hi('WhichKeyValue',              { fg = P.grey1 })
  hi('WhichKeyIcon',               { fg = P.gold })
  hi('WhichKeyIconAzure',          { fg = P.frost })
  hi('WhichKeyIconBlue',           { fg = P.frost })
  hi('WhichKeyIconCyan',           { fg = P.frost })
  hi('WhichKeyIconGreen',          { fg = P.lime })
  hi('WhichKeyIconGrey',           { fg = P.grey1 })
  hi('WhichKeyIconOrange',         { fg = P.gold })
  hi('WhichKeyIconPurple',         { fg = P.lilac })
  hi('WhichKeyIconRed',            { fg = P.cerise })
  hi('WhichKeyIconYellow',         { fg = P.sunny })
  
  -- ---------------------------------------------------------------------------
  -- indent-blankline.nvim (v2 + v3)
  -- ---------------------------------------------------------------------------
  hi('IndentBlanklineChar',        { fg = P.grey3, nocombine = true })
  hi('IndentBlanklineSpaceChar',   { fg = P.grey3, nocombine = true })
  hi('IndentBlanklineContextChar', { fg = P.frost, nocombine = true })
  hi('IndentBlanklineContextStart',{ underline = true, sp = P.frost })
  hi('IblIndent',                  { fg = P.grey3, nocombine = true })
  hi('IblWhitespace',              { fg = P.grey3, nocombine = true })
  hi('IblScope',                   { fg = P.frost, nocombine = true })
  
  -- ---------------------------------------------------------------------------
  -- nvim-web-devicons / mini.icons
  -- ---------------------------------------------------------------------------
  hi('DevIconDefault',             { fg = P.grey0 })
  hi('MiniIconsAzure',             { fg = P.frost })
  hi('MiniIconsBlue',              { fg = P.frost })
  hi('MiniIconsCyan',              { fg = P.frost })
  hi('MiniIconsGreen',             { fg = P.lime })
  hi('MiniIconsGrey',              { fg = P.grey1 })
  hi('MiniIconsOrange',            { fg = P.gold })
  hi('MiniIconsPurple',            { fg = P.lilac })
  hi('MiniIconsRed',               { fg = P.cerise })
  hi('MiniIconsYellow',            { fg = P.sunny })
  
  -- mini.pairs / mini.comment / mini.bufremove have no dedicated chrome;
  -- comment plugin follows Comment / @comment already set above.
  hi('MiniHipatternsFixme',        { fg = P.white, bg = P.mordant, bold = true })
  hi('MiniHipatternsHack',         { fg = P.black, bg = P.gold, bold = true })
  hi('MiniHipatternsTodo',         { fg = P.black, bg = P.frost, bold = true })
  hi('MiniHipatternsNote',         { fg = P.black, bg = P.lime, bold = true })
  hi('MiniTrailspace',             { bg = P.auburn })
  hi('MiniCursorword',             { underline = true, sp = P.grey1 })
  hi('MiniCursorwordCurrent',      { underline = true, sp = P.grey1 })
  hi('MiniAnimateNormalFloat',     { link = 'NormalFloat' })
  hi('MiniAnimateCursor',          { reverse = true, nocombine = true })
  
  -- ---------------------------------------------------------------------------
  -- nvim-ufo / promise-async
  -- ---------------------------------------------------------------------------
  hi('UfoFoldedFg',                { fg = P.white })
  hi('UfoFoldedBg',                { bg = P.grey4 })
  hi('UfoPreviewSbar',             { bg = P.grey4 })
  hi('UfoPreviewThumb',            { bg = P.grey1 })
  hi('UfoPreviewWinBar',           { bg = P.grey4 })
  hi('UfoPreviewCursorLine',       { bg = P.grey4 })
  hi('UfoFoldedEllipsis',          { fg = P.gold, bold = true })
  hi('UfoCursorFoldedLine',        { bg = P.grey4, bold = true })
  
  -- ---------------------------------------------------------------------------
  -- zen-mode.nvim
  -- ---------------------------------------------------------------------------
  hi('ZenBg',                      { fg = P.text, bg = P.bg })
  
  -- ---------------------------------------------------------------------------
  -- vimtex
  -- ---------------------------------------------------------------------------
  hi('texStatement',               { fg = P.cerise, bold = true })
  hi('texSection',                 { fg = P.cerise, bold = true })
  hi('texBeginEnd',                { fg = P.cerise, bold = true })
  hi('texBeginEndName',            { fg = P.gold })
  hi('texDocType',                 { fg = P.brick })
  hi('texDocTypeArgs',             { fg = P.lilac })
  hi('texCmd',                     { fg = P.cerise })
  hi('texCmdClass',                { fg = P.brick })
  hi('texCmdPackage',              { fg = P.lime })
  hi('texCmdDef',                  { fg = P.cerise, bold = true })
  hi('texCmdNew',                  { fg = P.lime })
  hi('texMathZone',                { fg = P.lilac })
  hi('texMathOper',                { fg = P.cerise })
  hi('texMathDelim',               { fg = P.lime })
  hi('texMathDelimZone',           { fg = P.gold })
  hi('texComment',                 { link = 'Comment' })
  hi('texSpecialChar',             { fg = P.lilac, bold = true })
  hi('texOpt',                     { fg = P.brick })
  hi('texArg',                     { fg = P.gold })
  hi('texParm',                    { fg = P.gold })
  hi('texRefArg',                  { fg = P.sunny })
  hi('texTitleArg',                { fg = P.gold, bold = true })
  hi('texAuthorArg',               { fg = P.frost })
  hi('texFileArg',                 { fg = P.lime })
  hi('texUrlArg',                  { fg = P.frost, underline = true })
  hi('texDelim',                   { fg = P.text })
  hi('texError',                   { fg = P.mordant, bold = true })
  hi('texTodo',                    { link = 'Todo' })
  
  -- ---------------------------------------------------------------------------
  -- render-markdown.nvim
  -- ---------------------------------------------------------------------------
  hi('RenderMarkdownH1',           { fg = P.cerise, bold = true })
  hi('RenderMarkdownH2',           { fg = P.gold, bold = true })
  hi('RenderMarkdownH3',           { fg = P.lime, bold = true })
  hi('RenderMarkdownH4',           { fg = P.frost, italic = true })
  hi('RenderMarkdownH5',           { fg = P.lilac, italic = true })
  hi('RenderMarkdownH6',           { fg = P.brick, italic = true })
  hi('RenderMarkdownH1Bg',         { bg = P.auburn })
  hi('RenderMarkdownH2Bg',         { bg = P.deep_blue })
  hi('RenderMarkdownH3Bg',         { bg = P.moss })
  hi('RenderMarkdownH4Bg',         { bg = P.grey4 })
  hi('RenderMarkdownH5Bg',         { bg = P.grey5 })
  hi('RenderMarkdownH6Bg',         { bg = P.grey5 })
  hi('RenderMarkdownCode',         { bg = P.grey5 })
  hi('RenderMarkdownCodeInfo',     { fg = P.frost })
  hi('RenderMarkdownCodeBorder',   { bg = P.grey5 })
  hi('RenderMarkdownCodeFallback', { link = 'Normal' })
  hi('RenderMarkdownCodeInline',   { fg = P.sunny, bg = P.grey5 })
  hi('RenderMarkdownQuote',        { fg = P.frost, italic = true })
  hi('RenderMarkdownQuote1',       { fg = P.frost })
  hi('RenderMarkdownQuote2',       { fg = P.grey1 })
  hi('RenderMarkdownQuote3',       { fg = P.grey2 })
  hi('RenderMarkdownQuote4',       { fg = P.grey2 })
  hi('RenderMarkdownQuote5',       { fg = P.grey3 })
  hi('RenderMarkdownQuote6',       { fg = P.grey3 })
  hi('RenderMarkdownInlineHighlight', { bg = P.grey4 })
  hi('RenderMarkdownBullet',       { fg = P.cerise })
  hi('RenderMarkdownDash',         { fg = P.grey2 })
  hi('RenderMarkdownSign',         { fg = P.grey2, bg = P.grey5 })
  hi('RenderMarkdownMath',         { fg = P.lilac })
  hi('RenderMarkdownIndent',       { fg = P.grey3 })
  hi('RenderMarkdownHtmlComment',  { link = 'Comment' })
  hi('RenderMarkdownLink',         { fg = P.frost })
  hi('RenderMarkdownWikiLink',     { fg = P.frost, bold = true })
  hi('RenderMarkdownUnchecked',    { fg = P.grey1 })
  hi('RenderMarkdownChecked',      { fg = P.lime })
  hi('RenderMarkdownTodo',         { fg = P.gold })
  hi('RenderMarkdownTableHead',    { fg = P.gold, bold = true })
  hi('RenderMarkdownTableRow',     { fg = P.text })
  hi('RenderMarkdownTableFill',    { fg = P.grey3 })
  hi('RenderMarkdownSuccess',      { fg = P.lime })
  hi('RenderMarkdownInfo',         { fg = P.frost })
  hi('RenderMarkdownHint',         { fg = P.frost })
  hi('RenderMarkdownWarn',         { fg = P.gold })
  hi('RenderMarkdownError',        { fg = P.mordant })
  
  -- ---------------------------------------------------------------------------
  -- tabby.nvim  (presets read TabLine*; extra named groups for custom lines)
  -- ---------------------------------------------------------------------------
  hi('TabbyHead',                  { fg = P.black, bg = P.gold, bold = true })
  hi('TabbyCurrentTab',            { fg = P.black, bg = P.gold, bold = true })
  hi('TabbyTab',                   { fg = P.grey0, bg = P.grey5 })
  hi('TabbyWin',                   { fg = P.grey0, bg = P.grey5 })
  hi('TabbyTail',                  { fg = P.grey0, bg = P.grey5 })
  hi('TabbyFill',                  { fg = P.grey2, bg = P.black })
  
  -- ---------------------------------------------------------------------------
  -- bullets.vim
  -- ---------------------------------------------------------------------------
  hi('BulletsBullet',              { fg = P.cerise, bold = true })
  hi('BulletsID',                  { fg = P.gold })
  
  -- ---------------------------------------------------------------------------
  -- nvim-treesitter-context (often paired with treesitter)
  -- ---------------------------------------------------------------------------
  hi('TreesitterContext',          { bg = P.grey5 })
  hi('TreesitterContextLineNumber',{ fg = P.grey2, bg = P.grey5 })
  hi('TreesitterContextBottom',    { underline = true, sp = P.grey3 })
  hi('TreesitterContextSeparator', { fg = P.grey3 })
  
  -- ---------------------------------------------------------------------------
  -- git signs commonly used alongside this stack
  -- ---------------------------------------------------------------------------
  hi('GitSignsAdd',                { fg = P.lime, bg = P.grey5 })
  hi('GitSignsChange',             { fg = P.gold, bg = P.grey5 })
  hi('GitSignsDelete',             { fg = P.cerise, bg = P.grey5 })
  hi('GitSignsTopdelete',          { fg = P.cerise, bg = P.grey5 })
  hi('GitSignsChangedelete',       { fg = P.brick, bg = P.grey5 })
  hi('GitSignsUntracked',          { fg = P.frost, bg = P.grey5 })
  hi('GitSignsAddLn',              { bg = P.moss })
  hi('GitSignsChangeLn',           { bg = P.grey4 })
  hi('GitSignsDeleteLn',           { bg = P.auburn })
  
  -- ---------------------------------------------------------------------------
  -- Terminal ANSI colours
  -- ---------------------------------------------------------------------------
  vim.g.terminal_color_0  = P.bg[1]
  vim.g.terminal_color_1  = P.cerise[1]
  vim.g.terminal_color_2  = P.lime[1]
  vim.g.terminal_color_3  = P.gold[1]
  vim.g.terminal_color_4  = P.frost[1]
  vim.g.terminal_color_5  = P.lilac[1]
  vim.g.terminal_color_6  = P.frost[1]
  vim.g.terminal_color_7  = P.text[1]
  vim.g.terminal_color_8  = P.grey3[1]
  vim.g.terminal_color_9  = P.brick[1]
  vim.g.terminal_color_10 = P.lime[1]
  vim.g.terminal_color_11 = P.sunny[1]
  vim.g.terminal_color_12 = P.frost[1]
  vim.g.terminal_color_13 = P.lilac[1]
  vim.g.terminal_color_14 = P.sunny[1]
  vim.g.terminal_color_15 = P.white[1]
  
  -- ---------------------------------------------------------------------------
  -- lualine.nvim — register a theme so `theme = 'harlequin'` works
  -- without a second file on the runtimepath.
  -- ---------------------------------------------------------------------------
  local lualine_theme = M.lualine(name)
  vim.g.harlequin_lualine = lualine_theme
  package.loaded['lualine.themes.' .. name] = lualine_theme
  
  -- Palette for users / auto-dark-mode.nvim hooks (this scheme is dark-only).
  vim.g.harlequin_palette = {
    text = P.text[1],
    bg = P.bg[1],
    cerise = P.cerise[1],
    lime = P.lime[1],
    gold = P.gold[1],
    brick = P.brick[1],
    lilac = P.lilac[1],
    frost = P.frost[1],
    sunny = P.sunny[1],
    mordant = P.mordant[1],
    grey4 = P.grey4[1],
    grey5 = P.grey5[1],
  }
  vim.g.harlequin_style = name
end

return M
