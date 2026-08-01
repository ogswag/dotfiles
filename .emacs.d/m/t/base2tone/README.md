# base2tone-themes

Native Emacs port of [Base2Tone](https://github.com/atelierbram/Base2Tone) by
Bram de Haan — 20 duotone color schemes, each in **dark** and **light**
variants (40 themes total).

## How it's built

This follows the doom-themes / base16-emacs model: a single core holds all the
face logic, and each loadable theme is a thin generated stub.

- **`base2tone-theme.el`** — the core. Holds the swatch→face mapping (matched to
  the official [Base2Tone VSCode
  themes](https://github.com/atelierbram/Base2Tone-VSCode-Themes) so colors line
  up with that editor port) and `base2tone-theme-define`. Edit faces here.
- **`palettes/base2tone-*.yml`** — the 20 upstream scheme palettes (32 swatches
  each: `A` uno ramp, `B` duo accent, `C` neutral, `D` second accent). Colors
  only.
- **`gen-base2tone.el`** — batch generator. Reads the palettes and writes the
  `base2tone-<scheme>-<dark|light>-theme.el` stubs.
- **`base2tone-<scheme>-<variant>-theme.el`** — generated, loadable themes. Do
  not edit by hand.

Dark and light share the same palette; only the role mapping differs.

## Regenerating

After editing `base2tone-theme.el` or a palette:

```sh
cd base2tone
emacs --batch -l gen-base2tone.el
```

To refresh palettes from upstream:

```sh
for s in cave desert drawbridge earth evening field forest garden heath lake \
         lavender mall meadow morning motel pool porch sea space suburb; do
  curl -fsSL "https://raw.githubusercontent.com/atelierbram/Base2Tone/master/db/schemes/base2tone-$s.yml" \
    -o "palettes/base2tone-$s.yml"
done
```

## Usage

```elisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/m/t/base2tone/")
(add-to-list 'load-path             "~/.emacs.d/m/t/base2tone/")
(load-theme 'base2tone-evening-dark t)
```

Then `M-x load-theme RET base2tone-` and tab-complete to browse all 40.
