;; Set default font face
(cond
 ((eq system-type 'windows-nt)
  (WHEN (member "Consolas" (font-family-list))
	(set-frame-font "Consolas" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "Zed Mono" (font-family-list))
	(set-frame-font "Zed Mono Light 14" t t)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
	(set-frame-font "DejaVu Sans Mono" t t))))

(defun az-modeline-font ()
  (set-face-attribute 'mode-line-active nil :family "Zed Mono Light" :height 120)
  (set-face-attribute 'mode-line-inactive nil :family "Zed Mono Light" :height 120))
(add-hook 'window-setup-hook 'az-modeline-font)
(add-hook 'window-state-change-hook 'az-modeline-font)
(add-hook 'window-divider-mode-hook 'az-modeline-font)

(use-package eclipse-theme
  :ensure t)
(use-package ef-themes
  :ensure t)
;; Yellow comments and green strings like older versions of the Modus
;; themes
(setq modus-vivendi-palette-overrides
      '((comment blue-intense)
        (string maroon)
        (bg-line-number-active bg-hl-line)
        (bg-line-number-inactive bg-main)))
(setq modus-vivendi-tinted-palette-overrides
      '((cursor fg-alt)
        (comment blue-intense)
        (string red)
        (bg-hl-line bg-dim)
        (bg-line-number-active bg-dim)
        (bg-line-number-inactive bg-main)
        (bg-paren-match bg-graph-yellow-0)
        (fg-paren-match bg-main)))


(setq  ef-bio-palette-overrides
       '((comment blue)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/colors/") t)
(require 'lupan-themes)

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-dark-theme 'modus-vivendi-tinted)
  (setq auto-dark-light-theme nil)
  (setq auto-dark-polling-interval-seconds 5)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

(use-package ns-auto-titlebar
  :ensure t)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; (use-package simple-modeline
;;   :ensure t
;;   :hook (after-init . simple-modeline-mode))
