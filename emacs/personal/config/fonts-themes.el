;; >> SET DEFAULT FONT FACE <<
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas 12" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "IosevkaCustomTerminal Nerd Font Mono" (font-family-list))
    (set-frame-font "IosevkaCustomTerminal Nerd Font Mono 18" t t)
    (set-face-attribute 'fixed-pitch nil :family "IosevkaCustomTerminal Nerd Font Mono")
    (set-face-attribute 'variable-pitch nil :family "Arial")))
 ((eq system-type 'gnu/linux)
  (when (member "IosevkaCustomTerminal Nerd Font Mono" (font-family-list))
    (set-frame-font "IosevkaCustomTerminal Nerd Font Mono 14" t t))))

;; >> ENABLE LIGATURES <<
(load "~/.emacs.d/lisp/packages/ligature.el")

;; >> INSTALL CUSTOM THEMES <<
(setq custom-safe-themes t)

(use-package catppuccin-theme
  :ensure t)
(use-package nimbus-theme
  :ensure t)

;; >> AUTO-DARK <<
;; Package for syncing themes with system
(use-package auto-dark
  :ensure t
  :diminish auto-dark-mode
  :config
  (setq auto-dark-light-theme 'leuven)
  (setq auto-dark-dark-theme 'nimbus)
  (setq auto-dark-polling-interval 3)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

(custom-set-faces
 ;; set fringe to no background for every theme
 '(fringe ((t (:background nil)))))

