" Tokyo Night Day colorscheme for Vim
" Ported from https://github.com/tokyo-night/tokyo-night-vscode-theme
" and https://github.com/folke/tokyonight.nvim

" Initialization
hi clear
if exists('syntax_on')
  syntax reset
endif

let g:colors_name = 'tokyonight-day'
set background=light

" Color definitions
let s:bg           = '#FEFFFE'
let s:fg           = '#151515'
let s:fg_dark      = '#000000'
let s:fg_gutter    = '#b0b4c3'
let s:comment      = '#6172b0'

let s:red          = '#8c4351'
let s:orange       = '#965027'
let s:yellow       = '#8f5e15'
let s:yellow_dark  = '#634f30'
let s:green        = '#385f0d'
let s:teal         = '#33635c'
let s:cyan         = '#0f4b6e'
let s:cyan2        = '#006c86'
let s:blue         = '#2959aa'
let s:blue1        = '#3760bf'
let s:magenta      = '#5a3e8e'
let s:purple       = '#7847bd'

let s:bg_dark      = '#d0d3e5'
let s:bg_highlight = '#c4c6cd'
let s:bg_visual    = '#b6bac9'
let s:border       = '#a8aecb'
let s:black        = '#343b58'

" Terminal colors
let g:terminal_ansi_colors = [
  \ '#343b58',
  \ '#8c4351',
  \ '#385f0d',
  \ '#8f5e15',
  \ '#2959aa',
  \ '#5a3e8e',
  \ '#0f4b6e',
  \ '#3760bf',
  \ '#6172b0',
  \ '#8c4351',
  \ '#385f0d',
  \ '#8f5e15',
  \ '#2959aa',
  \ '#5a3e8e',
  \ '#0f4b6e',
  \ '#3760bf'
\]

" Helper function to set highlights
function! s:h(group, fg, bg, attr)
  if a:fg != ''
    exec 'hi ' . a:group . ' guifg=' . a:fg . ' ctermfg=' . s:GetColor(a:fg)
  endif
  if a:bg != ''
    exec 'hi ' . a:group . ' guibg=' . a:bg . ' ctermbg=' . s:GetColor(a:bg)
  endif
  if a:attr != ''
    exec 'hi ' . a:group . ' gui=' . a:attr . ' cterm=' . a:attr
  endif
endfunction

" Simple color approximation for cterm
function! s:GetColor(color)
  if a:color == s:bg
    return '254'
  elseif a:color == s:fg || a:color == s:blue1
    return '25'
  elseif a:color == s:fg_dark || a:color == s:black
    return '237'
  elseif a:color == s:comment
    return '61'
  elseif a:color == s:red
    return '131'
  elseif a:color == s:orange
    return '94'
  elseif a:color == s:yellow
    return '136'
  elseif a:color == s:green
    return '22'
  elseif a:color == s:teal
    return '23'
  elseif a:color == s:cyan || a:color == s:cyan2
    return '24'
  elseif a:color == s:blue
    return '25'
  elseif a:color == s:magenta
    return '54'
  elseif a:color == s:purple
    return '91'
  elseif a:color == s:bg_dark
    return '253'
  elseif a:color == s:bg_highlight
    return '251'
  elseif a:color == s:bg_visual
    return '250'
  elseif a:color == s:border
    return '248'
  elseif a:color == s:fg_gutter
    return '249'
  else
    return 'NONE'
  endif
endfunction

" Editor colors
call s:h('Normal', s:fg, s:bg, '')
call s:h('CursorLine', '', s:bg_dark, '')
call s:h('CursorColumn', '', s:bg_dark, '')
call s:h('ColorColumn', '', s:bg_dark, '')
call s:h('LineNr', s:fg_gutter, '', '')
call s:h('CursorLineNr', s:fg_dark, '', 'bold')
call s:h('VertSplit', s:border, s:bg, '')
call s:h('SignColumn', s:fg_gutter, s:bg, '')
call s:h('Folded', s:blue, s:bg_dark, '')
call s:h('FoldColumn', s:comment, s:bg, '')

" Cursor and selection
call s:h('Cursor', s:bg, s:fg, '')
call s:h('Visual', '', s:bg_visual, '')
call s:h('VisualNOS', '', s:bg_visual, '')
call s:h('MatchParen', s:orange, '', 'bold,underline')

" Search
call s:h('Search', s:bg, s:yellow, '')
call s:h('IncSearch', s:bg, s:orange, '')

" Status line
call s:h('StatusLine', s:fg_dark, s:bg_dark, '')
call s:h('StatusLineNC', s:comment, s:bg_dark, '')
call s:h('WildMenu', s:bg, s:blue, 'bold')

" Tab line
call s:h('TabLine', s:comment, s:bg_dark, '')
call s:h('TabLineFill', '', s:bg_dark, '')
call s:h('TabLineSel', s:bg, s:blue, 'bold')

" Popup menu
call s:h('Pmenu', s:fg, s:bg_dark, '')
call s:h('PmenuSel', s:bg, s:blue, 'bold')
call s:h('PmenuSbar', '', s:bg_highlight, '')
call s:h('PmenuThumb', '', s:comment, '')

" Diffs
call s:h('DiffAdd', s:green, s:bg_dark, '')
call s:h('DiffChange', s:blue, s:bg_dark, '')
call s:h('DiffDelete', s:red, s:bg_dark, '')
call s:h('DiffText', s:cyan, s:bg_highlight, 'bold')

" Messages
call s:h('ErrorMsg', s:red, '', 'bold')
call s:h('WarningMsg', s:orange, '', 'bold')
call s:h('ModeMsg', s:blue, '', 'bold')
call s:h('MoreMsg', s:blue, '', '')
call s:h('Question', s:green, '', '')

" Spell checking
call s:h('SpellBad', s:red, '', 'underline')
call s:h('SpellCap', s:yellow, '', 'underline')
call s:h('SpellLocal', s:cyan, '', 'underline')
call s:h('SpellRare', s:purple, '', 'underline')

" Syntax highlighting
call s:h('Comment', s:comment, '', 'italic')
call s:h('Todo', s:purple, s:bg, 'bold,italic')

call s:h('Constant', s:orange, '', '')
call s:h('String', s:green, '', '')
call s:h('Character', s:green, '', '')
call s:h('Number', s:orange, '', '')
call s:h('Boolean', s:orange, '', '')
call s:h('Float', s:orange, '', '')

call s:h('Identifier', s:fg_dark, '', '')
call s:h('Function', s:blue, '', '')

call s:h('Statement', s:magenta, '', '')
call s:h('Conditional', s:magenta, '', '')
call s:h('Repeat', s:magenta, '', '')
call s:h('Label', s:magenta, '', '')
call s:h('Operator', s:cyan2, '', '')
call s:h('Keyword', s:magenta, '', 'italic')
call s:h('Exception', s:magenta, '', '')

call s:h('PreProc', s:cyan, '', '')
call s:h('Include', s:magenta, '', '')
call s:h('Define', s:magenta, '', '')
call s:h('Macro', s:cyan, '', '')
call s:h('PreCondit', s:cyan, '', '')

call s:h('Type', s:cyan2, '', '')
call s:h('StorageClass', s:cyan, '', '')
call s:h('Structure', s:cyan, '', '')
call s:h('Typedef', s:cyan, '', '')

call s:h('Special', s:blue1, '', '')
call s:h('SpecialChar', s:yellow, '', '')
call s:h('Tag', s:red, '', '')
call s:h('Delimiter', s:fg, '', '')
call s:h('SpecialComment', s:comment, '', 'bold')
call s:h('Debug', s:orange, '', '')

call s:h('Underlined', s:cyan, '', 'underline')
call s:h('Ignore', s:comment, '', '')
call s:h('Error', s:red, s:bg, 'bold')

" Language-specific highlighting

" HTML
call s:h('htmlTag', s:cyan, '', '')
call s:h('htmlEndTag', s:cyan, '', '')
call s:h('htmlTagName', s:red, '', '')
call s:h('htmlArg', s:magenta, '', '')
call s:h('htmlSpecialChar', s:yellow, '', '')
call s:h('htmlTitle', s:fg, '', '')
call s:h('htmlH1', s:blue, '', 'bold')

" CSS
call s:h('cssClassName', s:green, '', '')
call s:h('cssIdentifier', s:cyan2, '', '')
call s:h('cssColor', s:cyan, '', '')
call s:h('cssProp', s:blue, '', '')
call s:h('cssValueLength', s:orange, '', '')
call s:h('cssValueNumber', s:orange, '', '')
call s:h('cssUnitDecorators', s:red, '', '')

" JavaScript
call s:h('javaScriptFunction', s:magenta, '', 'italic')
call s:h('javaScriptIdentifier', s:magenta, '', '')
call s:h('javaScriptNull', s:orange, '', '')
call s:h('javaScriptNumber', s:orange, '', '')
call s:h('javaScriptBraces', s:fg, '', '')

" Python
call s:h('pythonBuiltin', s:cyan2, '', '')
call s:h('pythonException', s:magenta, '', '')
call s:h('pythonDecorator', s:cyan, '', '')

" Ruby
call s:h('rubySymbol', s:teal, '', '')
call s:h('rubyConstant', s:cyan2, '', '')
call s:h('rubyAttribute', s:blue, '', '')
call s:h('rubyInclude', s:magenta, '', '')
call s:h('rubyStringDelimiter', s:green, '', '')

" Markdown
call s:h('markdownHeadingDelimiter', s:blue, '', 'bold')
call s:h('markdownH1', s:blue, '', 'bold')
call s:h('markdownH2', s:blue, '', 'bold')
call s:h('markdownH3', s:blue, '', 'bold')
call s:h('markdownLinkText', s:cyan, '', 'underline')
call s:h('markdownUrl', s:teal, '', 'underline')
call s:h('markdownCode', s:cyan, s:bg_dark, '')
call s:h('markdownCodeBlock', s:green, '', '')
call s:h('markdownCodeDelimiter', s:teal, '', '')

" Git
call s:h('gitcommitSummary', s:blue, '', '')
call s:h('gitcommitOverflow', s:red, '', '')
call s:h('gitcommitSelectedFile', s:green, '', '')
call s:h('gitcommitDiscardedFile', s:red, '', '')

" Plugin support

" GitGutter
call s:h('GitGutterAdd', s:green, '', '')
call s:h('GitGutterChange', s:yellow, '', '')
call s:h('GitGutterDelete', s:red, '', '')
call s:h('GitGutterChangeDelete', s:orange, '', '')

" fugitive.vim
call s:h('diffAdded', s:green, '', '')
call s:h('diffRemoved', s:red, '', '')

" NERDTree
call s:h('NERDTreeDir', s:blue, '', 'bold')
call s:h('NERDTreeDirSlash', s:blue, '', '')
call s:h('NERDTreeFile', s:fg, '', '')
call s:h('NERDTreeExecFile', s:green, '', '')
call s:h('NERDTreeOpenable', s:comment, '', '')
call s:h('NERDTreeClosable', s:comment, '', '')

" ALE
call s:h('ALEError', s:red, '', 'underline')
call s:h('ALEWarning', s:yellow, '', 'underline')
call s:h('ALEInfo', s:blue, '', 'underline')

" Clean up
delfunc s:h
delfunc s:GetColor
