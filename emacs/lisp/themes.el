;; Set default font face
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
	(set-frame-font "Consolas" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "UbuntuMono Nerd Font" (font-family-list))
	(set-frame-font "UbuntuMono Nerd Font 16" t t)
    (setq-default line-spacing 4)
    (defun az-modeline-font ()
      (set-face-attribute 'mode-line-active nil :family "BlexMono Nerd Font" :height 120)
      (set-face-attribute 'mode-line-inactive nil :family "BlexMono Nerd Font" :height 120)
      )
    (add-hook 'window-setup-hook 'az-modeline-font)
    (add-hook 'window-state-change-hook 'az-modeline-font)
    (add-hook 'window-divider-mode-hook 'az-modeline-font)
    (defun az-minibuffer-setup ()
      (set (make-local-variable 'face-remapping-alist)
           '((default :family "BlexMono Nerd Font" :height 120))))
    (add-hook 'minibuffer-setup-hook 'az-minibuffer-setup)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
	(set-frame-font "DejaVu Sans Mono" t t))))


(use-package adwaita-dark-theme
  :ensure t)
(use-package standard-themes
  :ensure t)
(use-package one-themes
  :ensure t)

(setq standard-dark-palette-overrides
      '((bg-main                 "#252930")
        (bg-region               "#384051")
        (fg-main                 "#C3C8D2")
        (string                  "#98C379")
        (name                    "#98C379")
        (constant                "#98C379")
        (comment                 "#808a9d")
        (docstring               "#808a9d")
        (bg-hl-line              "#2A2F37")
        (fringe                  "#252930")
        (bg-paren                "#C678DD")
        (fg-line-number-inactive "#5C6370")
        (fg-line-number-active   "#C3C8D2")
        (bg-line-number-inactive "#252930")
        (bg-line-number-active   "#2A2F37")
        (keyword                 "#E06C75")
        )
      )

;; (defun my-standard-themes-custom-faces ()
;;   "My customizations on top of the Standard themes.
;; This function is added to the `standard-themes-post-load-hook'."
;;   (standard-themes-with-colors
;;     (custom-set-faces
;;      `(show-paren-match ((,c :background "#C678DD" :foreground "#252930")))
;;      `(mode-line-active ((,c :background "#424857" :foreground "#ABB2BF" :box (:line-width (1 . -1) :style flat-button :color "#5c6370"))))
;;      `(mode-line-inactive ((,c :background "#252930" :foreground "#687387" :box (:line-width (1 . -1) :style flat-button :color "#252930"))))
;;      )))

;; Using the hook lets our changes persist when we use the commands
;; `standard-themes-toggle', `standard-themes-load-dark',
;; `standard-themes-load-light'.
(add-hook 'standard-themes-post-load-hook #'my-standard-themes-custom-faces)

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-light-theme 'one-light)
  (setq auto-dark-dark-theme 'one-dark)
  (setq auto-dark-polling-interval-seconds 5)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

(use-package ns-auto-titlebar
  :ensure t)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))
