" Factory Safety version 0.2
"
" A laarge chunk of code was taken from
" Static colorless colorscheme for Vim
"
" Made by nayk (https://codeberg.org/nayk)

set background=dark
if version > 580
    highlight clear
    if exists("syntax_on")
        syntax reset
    endif
endif

set t_Co=256
let g:colors_name = "factory-safety"

highlight Normal   ctermfg=230 ctermbg=232 cterm=none
highlight Comment  ctermfg=144
highlight String   ctermfg=220 ctermbg=none cterm=none
highlight Constant ctermfg=220 ctermbg=none cterm=none
highlight ErrorMsg ctermfg=15  ctermbg=88
highlight SpellCap ctermfg=230 ctermbg=25

highlight ColorColumn  ctermbg=30
highlight CursorLineNr ctermfg=220 cterm=bold
highlight CursorLine   cterm=none  ctermbg=235
highlight LineNr       ctermfg=101

highlight Folded     ctermfg=137 ctermbg=23
highlight NonText    ctermfg=230
highlight SpecialKey ctermfg=144
highlight Visual     ctermfg=0 ctermbg=220
highlight VertSplit  ctermfg=220 ctermbg=none cterm=none

highlight StatusLine   ctermfg=0 ctermbg=220 cterm=bold
highlight StatusLineNC ctermfg=248 ctermbg=238 cterm=none

highlight TabLineSel   ctermbg=none ctermfg=220 cterm=bold
highlight TabLine     ctermfg=240  ctermbg=220 cterm=none
highlight TabLineFill ctermfg=none ctermbg=220 cterm=none

" vimdiff mode.
" highlight DiffAdd    ctermfg=15  ctermbg=23
" highlight DiffChange ctermfg=15  ctermbg=53
" highlight DiffDelete ctermfg=233 ctermbg=none
" highlight DiffText   ctermfg=15  ctermbg=124

hi DiffAdd guifg=#f8efd8 guibg=#51a163 guisp=NONE gui=NONE ctermfg=230 ctermbg=71 cterm=NONE
hi DiffChange guifg=#f8efd8 guibg=#3b80a1 guisp=NONE gui=NONE ctermfg=230 ctermbg=67 cterm=NONE
hi DiffDelete guifg=#f8efd8 guibg=#d33934 guisp=NONE gui=NONE ctermfg=230 ctermbg=167 cterm=NONE
hi DiffText guifg=#f8efd8 guibg=#51a163 guisp=NONE gui=NONE ctermfg=230 ctermbg=71 cterm=NONE
hi Diffadd guifg=#f8efd8 guibg=#51a163 guisp=NONE gui=NONE ctermfg=230 ctermbg=71 cterm=NONE
hi Diffdelete guifg=#f8efd8 guibg=#d33934 guisp=NONE gui=NONE ctermfg=230 ctermbg=167 cterm=NONE
" " diff filetype. Mimic git diff output.
" highlight diffAdded   ctermfg=2
" highlight diffLine    ctermfg=6
" highlight diffRemoved ctermfg=1

" Link to already fixed colors.
highlight clear Conceal
highlight link  Conceal Folded

highlight clear CursorColumn
highlight link  CursorColumn StatusLine

highlight clear FoldColumn
highlight link  FoldColumn Folded

highlight clear qfError
highlight link  qfError qfLineNr

highlight clear Question
highlight link  Question MoreMsg

highlight clear SignColumn
highlight link  SignColumn ColorColumn

highlight clear Todo
highlight link  Todo Comment

highlight clear WarningMsg
highlight link  WarningMsg CursorLineNr

highlight clear WildMenu
highlight link  WildMenu Search

highlight clear gitcommitBranch
highlight clear gitcommitFile
highlight clear gitcommitHeader
highlight clear gitcommitType
highlight link  gitcommitBranch Comment
highlight link  gitcommitFile   Comment
highlight link  gitcommitHeader Comment
highlight link  gitcommitType   Comment

highlight clear Pmenu
highlight clear PmenuSbar
highlight clear PmenuSel
highlight link  Pmenu     DiffChange
highlight link  PmenuSbar StatusLine
highlight link  PmenuSel  DiffText

highlight clear SpellBad
highlight clear SpellLocal
highlight clear SpellRare
highlight link  SpellBad   ErrorMsg
highlight link  SpellLocal DiffAdd
highlight link  SpellRare  DiffChange

highlight clear StatusLineTerm
highlight clear StatusLineTermNC
highlight link  StatusLineTerm   StatusLine
highlight link  StatusLineTermNC StatusLineNC

" Clear syntax coloring.
highlight clear Error
highlight clear Identifier
highlight clear MatchParen
highlight clear PreProc
highlight clear Title
highlight clear Type
highlight clear Underlined
" highlight clear Constant
highlight clear Special
highlight clear Statement

" Plugins...
highlight FernMarkedText ctermfg=39 ctermbg=234

highlight SyntasticErrorSign        ctermfg=197 ctermbg=234
highlight SyntasticWarningSign      ctermfg=179 ctermbg=234
highlight SyntasticStyleErrorSign   ctermfg=39  ctermbg=234
highlight SyntasticStyleWarningSign ctermfg=35  ctermbg=234

augroup Static
  autocmd!

  " When editing markdown files Vim will still display syntax colors because it
  " adds file type specific highlights after it has loaded Static.
  autocmd FileType markdown setlocal syntax=

  " Help text is easier to read with syntax off. Adds brackets, asterix's etc.
  autocmd FileType help setlocal syntax=
augroup END
