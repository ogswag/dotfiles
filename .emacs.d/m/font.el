;;; font.el --- Font selection -*- lexical-binding: t; -*-

;;; Commentary:
;; pick first installed font from a preference list.

;;; Code:

(require 'seq)

(defconst my/mono-fonts
  '("IoskeleyMono Nerd Font"
    "Inconsolata LGC Nerd Font"
    "Inconsolata LGC"
    "Lilex Nerd Font"
    "Lilex"
    "Liga Mononoki"
    "mononoki"
    "Hack Nerd Font"
    "Hack"
    "Menlo"
    "DejaVu Sans Mono"
    "Consolas"))

(defconst my/nerd-mono-fonts
  '("IoskeleyMono Nerd Font"
    "Inconsolata LGC Nerd Font Mono"
    "Lilex Nerd Font Mono"
    "Hack Nerd Font Mono"
    "Mononoki Nerd Font Mono"
    "Symbols Nerd Font Mono"))

(defconst my/sans-fonts
  '("Open Sans"
    "Source Sans 3"
    "Verdana"
    "DejaVu Sans"
    "Noto Sans"))

(defconst my/font-size 15)

(defun my/first-available-font (candidates)
  "Return the first family in CANDIDATES installed on this system, or nil."
  (let ((families (font-family-list)))
    (seq-find (lambda (family) (member family families)) candidates)))

(defvar my/nerd-mono-font (my/first-available-font my/nerd-mono-fonts))

(defun my/font-applied-p (family size)
  "Non-nil when the default face is already FAMILY at SIZE points."
  (and (equal (face-attribute 'default :family nil t) family)
       (equal (face-attribute 'default :height nil t) (* 10 size))))


(let ((mono (my/first-available-font my/mono-fonts))
      (sans (my/first-available-font my/sans-fonts)))
  (when (and mono (not (my/font-applied-p mono my/font-size)))
    (set-frame-font (format "%s %d" mono my/font-size) nil t))
  (when mono
    (set-face-attribute 'fixed-pitch nil :family mono))
  (when sans
    (set-face-attribute 'variable-pitch nil :family sans :height 0.98)))

;;;; Getting the fonts in the first place
(use-package fontdue
  :load-path my-vendor-directory
  :defer t
  :commands (fontdue-report fontdue-install-missing
                            fontdue-install-source fontdue-verify)
  ;; `emacs-startup-hook' rather than a command: the point of `ask' is that a machine missing fonts.
  :hook (emacs-startup . fontdue-maybe-install)
  :custom
  ;; System-wide, so the fonts are there for every application and not only for this account.
  (fontdue-scope 'system)
  (fontdue-auto-install 'ask)
  ;; One source of truth.
  (fontdue-wanted (lambda ()
                    (append my/mono-fonts my/nerd-mono-fonts my/sans-fonts)))
  (fontdue-system-families
   '("Menlo" "Monaco" "Verdana" "Helvetica" "Helvetica Neue" "Courier New"
     "Times New Roman" "Arial" "Georgia" "Apple Color Emoji" "SF Mono"
     "PT Mono" "Andale Mono"))
  (fontdue-unobtainable-families
   '(("Consolas" . "Microsoft, not redistributable -- comes with Office")))
  (fontdue-sources
   '((nerd-inconsolata-lgc
      :urls ("https://github.com/ryanoasis/nerd-fonts/releases/download/v3.5.0/InconsolataLGC.zip")
      :provides ("Inconsolata LGC Nerd Font"
                 "Inconsolata LGC Nerd Font Mono"
                 "Inconsolata LGC Nerd Font Propo")
      :size "17 MB" :license "OFL-1.1")
     (nerd-lilex
      :urls ("https://github.com/ryanoasis/nerd-fonts/releases/download/v3.5.0/Lilex.zip")
      :provides ("Lilex Nerd Font" "Lilex Nerd Font Mono" "Lilex Nerd Font Propo")
      :size "41 MB" :license "OFL-1.1")
     (nerd-hack
      :urls ("https://github.com/ryanoasis/nerd-fonts/releases/download/v3.5.0/Hack.zip")
      :provides ("Hack Nerd Font" "Hack Nerd Font Mono" "Hack Nerd Font Propo")
      :size "18 MB" :license "MIT")
     (nerd-mononoki
      :urls ("https://github.com/ryanoasis/nerd-fonts/releases/download/v3.5.0/Mononoki.zip")
      :provides ("Mononoki Nerd Font" "Mononoki Nerd Font Mono"
                 "Mononoki Nerd Font Propo")
      :size "16 MB" :license "OFL-1.1")
     (nerd-symbols
      :urls ("https://github.com/ryanoasis/nerd-fonts/releases/download/v3.5.0/NerdFontsSymbolsOnly.zip")
      :provides ("Symbols Nerd Font" "Symbols Nerd Font Mono")
      :size "3 MB" :license "OFL-1.1")
     (nerd-ioskeley-mono
      :urls ("https://github.com/ahatem/IoskeleyMono/releases/download/v2.0.0/IoskeleyMono-NerdFont.zip")
      :provide ("IoskeleyMono Nerd Font" "IoskeleyMono Nerd Font Condensed" "IoskeleyMono Nerd Font SemiCondensed")
      :size "121 MB" :license "OFL-1.1")
     ;; The unpatched originals.
     (inconsolata-lgc
      :urls ("https://github.com/MihailJP/Inconsolata-LGC/releases/download/v3.300/InconsolataLGC-FullTTF-3.300.tar.xz")
      :provides ("Inconsolata LGC")
      :size "1.4 MB" :license "OFL-1.1")
     (lilex
      :urls ("https://github.com/mishamyrt/Lilex/releases/download/2.700/Lilex.zip")
      :provides ("Lilex")
      :size "3.7 MB" :license "OFL-1.1")
     (mononoki
      :urls ("https://github.com/madmalik/mononoki/releases/download/1.6/mononoki.zip")
      :provides ("mononoki")
      :size "0.5 MB" :license "OFL-1.1")
     (liga-mononoki
      :urls ("https://raw.githubusercontent.com/leopoldfajtak/liga_mononoki/5381a1f38400ff3fecc4e1c72368357dc5a617ad/LigaMononoki-Regular.ttf"
             "https://raw.githubusercontent.com/leopoldfajtak/liga_mononoki/5381a1f38400ff3fecc4e1c72368357dc5a617ad/LigaMononoki-Italic.ttf"
             "https://raw.githubusercontent.com/leopoldfajtak/liga_mononoki/5381a1f38400ff3fecc4e1c72368357dc5a617ad/LigaMononoki-Bold.ttf"
             "https://raw.githubusercontent.com/leopoldfajtak/liga_mononoki/5381a1f38400ff3fecc4e1c72368357dc5a617ad/LigaMononoki-BoldItalic.ttf")
      :provides ("Liga Mononoki")
      :size "0.7 MB" :license "MIT")
     (hack
      :urls ("https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip")
      :provides ("Hack")
      :size "0.6 MB" :license "MIT")
     (dejavu
      :urls ("https://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.zip")
      :provides ("DejaVu Sans" "DejaVu Sans Mono" "DejaVu Serif"
                 "DejaVu Sans Condensed" "DejaVu Serif Condensed"
                 "DejaVu Math TeX Gyre")
      :size "5.5 MB" :license "Bitstream Vera")
     (source-sans-3
      :urls ("https://github.com/adobe-fonts/source-sans/releases/download/3.052R/OTF-source-sans-3.052R.zip")
      :provides ("Source Sans 3")
      :size "2.4 MB" :license "OFL-1.1")
     (noto-sans
      :urls ("https://raw.githubusercontent.com/google/fonts/main/ofl/notosans/NotoSans%5Bwdth%2Cwght%5D.ttf"
             "https://raw.githubusercontent.com/google/fonts/main/ofl/notosans/NotoSans-Italic%5Bwdth%2Cwght%5D.ttf")
      :provides ("Noto Sans")
      :size "4.4 MB" :license "OFL-1.1")
     (open-sans
      :urls ("https://raw.githubusercontent.com/google/fonts/main/ofl/opensans/OpenSans%5Bwdth%2Cwght%5D.ttf"
             "https://raw.githubusercontent.com/google/fonts/main/ofl/opensans/OpenSans-Italic%5Bwdth%2Cwght%5D.ttf")
      :provides ("Open Sans")
      :size "1.1 MB" :license "OFL-1.1"))))

;;; font.el ends here
