;;; font.el --- Font selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Picks the first font installed on this machine from an ordered preference
;; list, so the same config works across machines with different fonts.

;;; Code:

(require 'seq)

(defconst my/mono-fonts
  '("Liga mononoki"
    "mononoki"
    "Hack Nerd Font"
    "Hack"
    "Hasklig"
    "Menlo"
    "DejaVu Sans Mono"
    "Consolas")
  "Monospaced font families, most preferred first.")

(defconst my/sans-fonts
  '("Source Sans 3"
    "Source Sans"
    "Source Sans Pro"
    "Verdana"
    "DejaVu Sans"
    "Noto Sans")
  "Proportional font families, most preferred first.")

(defconst my/font-size 13
  "Point size for the default frame font.
Only applied to the frame font: `fixed-pitch' and `variable-pitch' must
be left sizeless so they scale with it.")

(defun my/first-available-font (candidates)
  "Return the first family in CANDIDATES installed on this system, or nil."
  (let ((families (font-family-list)))
    (seq-find (lambda (family) (member family families)) candidates)))

(let ((mono (my/first-available-font my/mono-fonts))
      (sans (my/first-available-font my/sans-fonts)))
  (when mono
    (set-frame-font (format "%s %d" mono my/font-size) t t)
    ;; No size here — `fixed-pitch' must scale with the frame font, and a
    ;; family string with a size baked in matches nothing.
    (set-face-attribute 'fixed-pitch nil :family mono))
  (when sans
    (set-face-attribute 'variable-pitch nil :family sans)))

;;; font.el ends here
