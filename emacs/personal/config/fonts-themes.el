;; >> SET DEFAULT FONT FACE <<
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas 12" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "Iosevka Sans" (font-family-list))
    (set-frame-font "Iosevka Sans 14" t t)
    (set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono")
    (set-face-attribute 'variable-pitch nil :family "Arial")))
 ((eq system-type 'gnu/linux)
  (when (member "Ubuntu Mono" (font-family-list))
    (set-frame-font "Ubuntu Mono 14" t t))))

;; >> INSTALL CUSTOM THEMES <<
(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/personal/themes/")
(add-to-list 'load-path "~/.emacs.d/personal/themes/")

(use-package nimbus-theme
  :ensure t)

;; >> AUTO-DARK <<
;; Package for syncing themes with system
(use-package auto-dark
  :ensure t
  :diminish auto-dark-mode
  :config
  (setq auto-dark-light-theme 'tango-now-light)
  (setq auto-dark-dark-theme 'shanty-themes-dark)
  (setq auto-dark-polling-interval 3)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

(custom-set-faces
 ;; make fringe transparent for any theme
 '(fringe ((t (:background nil)))))
