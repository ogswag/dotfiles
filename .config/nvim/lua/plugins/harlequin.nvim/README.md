# harlequin.nvim

Neovim port of [nielsmadan/harlequin](https://github.com/nielsmadan/harlequin).

Dark, high-contrast, warm palette inspired by Molokai and Badwolf. Chrome that stays on screen (line numbers, splits, statusline) stays low-key; code uses the original accents.

Two schemes, same accents:

| Command | Background |
|---|---|
| `:colorscheme harlequin` | original warm charcoal `#1C1B1A` |
| `:colorscheme harlequin-black` | OLED `#000000` |

## Install

### Drop-in (no plugin manager)

Put the plugin on the runtimepath (`lua/` + `colors/`). Drop-in of a single `colors/*.lua` file is not enough anymore; highlights live in `lua/harlequin/init.lua`.

```lua
vim.opt.termguicolors = true
vim.cmd.colorscheme('harlequin')       -- or 'harlequin-black'
```

### lazy.nvim

```lua
{
  dir = '/path/to/harlequin.nvim', -- or a git remote if you publish it
  lazy = false,
  priority = 1000,
  config = function()
    vim.opt.termguicolors = true
    vim.cmd.colorscheme('harlequin') -- or 'harlequin-black'
  end,
}
```

## Palette

| Name    | Hex     | Role                          |
|---------|---------|-------------------------------|
| bg      | `#1C1B1A` / `#000000` | editor (`harlequin` / `harlequin-black`) |
| text    | `#F8F8F2` | default foreground          |
| cerise  | `#FF0033` | keywords / statements       |
| gold    | `#FFB829` | functions / fields          |
| brick   | `#CB4154` | types                       |
| lime    | `#AEEE00` | includes / preproc / dirs   |
| lilac   | `#AE81FF` | constants / numbers         |
| frost   | `#2C89C7` | comments / info             |
| sunny   | `#FFFC7F` | strings                     |
| mordant | `#AE0C00` | errors                      |

`harlequin-black` keeps those accents. Only the surfaces change: editor `#000000`, panels/cursorline `#0A0A0A`, selection/folds `#161616`. Diff washes are darker so they do not glow on an OLED panel.

Treesitter `@variable` uses the default foreground so a file is not a wall of gold. Functions, types, and keywords keep the original Harlequin accents.

## Plugin support

Defined in the same file:

- lazy.nvim
- nvim-treesitter (captures + a few legacy `TS*` links)
- nvim-lspconfig, mason.nvim, mason-lspconfig.nvim
- telescope.nvim, telescope-fzf-native.nvim, telescope-file-browser.nvim
- plenary.nvim (no dedicated groups)
- blink.cmp, LuaSnip, friendly-snippets
- conform.nvim, nvim-lint
- lualine.nvim (`theme = 'harlequin'` or `'harlequin-black'`)
- which-key.nvim
- indent-blankline.nvim (v2 and v3)
- nvim-web-devicons, mini.icons
- mini.pairs, mini.comment, mini.bufremove (no extra chrome)
- nvim-ufo, promise-async
- zen-mode.nvim
- vimtex
- render-markdown.nvim
- tabby.nvim (`TabLine*` plus `TabbyHead` / `TabbyCurrentTab` / `TabbyTab` / `TabbyFill`)
- bullets.vim
- im-select.nvim (no highlight groups)
- auto-dark-mode.nvim — this scheme is dark only; keep `fallback = 'harlequin'` for the dark side

Original Vim-scheme extras kept: EasyMotion, coc.nvim, language-specific syntax groups.

## lualine

```lua
require('lualine').setup({
  options = { theme = 'harlequin' },       -- or 'harlequin-black'
})
```

Loading the colorscheme registers `package.loaded['lualine.themes.harlequin']` (and `...harlequin-black`). Load the colorscheme before lualine, or run `:colorscheme` again after lualine starts.

The active table is also in `vim.g.harlequin_lualine`. `vim.g.harlequin_style` is `'harlequin'` or `'harlequin-black'`.

## tabby.nvim

Presets that use `TabLine` / `TabLineSel` / `TabLineFill` work as-is. For a custom line:

```lua
local theme = {
  fill = 'TabbyFill',
  head = 'TabbyHead',
  current_tab = 'TabbyCurrentTab',
  tab = 'TabbyTab',
  win = 'TabbyWin',
  tail = 'TabbyTail',
}
```

## auto-dark-mode.nvim

Harlequin has no light variant.

```lua
require('auto-dark-mode').setup({
  set_dark_mode = function()
    vim.api.nvim_set_option_value('background', 'dark', {})
    vim.cmd.colorscheme('harlequin-black') -- or 'harlequin'
  end,
  set_light_mode = function()
    vim.cmd.colorscheme('your-light-scheme')
  end,
})
```

## Overrides

```lua
vim.api.nvim_create_autocmd('ColorScheme', {
  pattern = { 'harlequin', 'harlequin-black' },
  callback = function()
    vim.api.nvim_set_hl(0, 'Comment', { fg = '#2C89C7', italic = true })
  end,
})
```

Hex values used by the loaded scheme are in `vim.g.harlequin_palette`.
