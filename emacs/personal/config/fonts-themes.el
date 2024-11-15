;; >> SET DEFAULT FONT FACE <<
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas 12" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "Iosevka Custom Normal" (font-family-list))
    (set-frame-font "Iosevka Custom Normal 18" t t)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka Custom Normal")
    (set-face-attribute 'variable-pitch nil :family "Arial")))
 ((eq system-type 'gnu/linux)
  (when (member "IosevkaCustomTerminal Nerd Font Mono" (font-family-list))
    (set-frame-font "IosevkaCustomTerminal Nerd Font Mono 14" t t))))

;; >> INSTALL CUSTOM THEMES <<
(setq custom-safe-themes t)

(use-package twilight-bright-theme
  :ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/personal/themes/")
(add-to-list 'load-path "~/.emacs.d/personal/themes/")

;; (load-file "~/.emacs.d/personal/themes/tango-now-theme.el")
;; (load-file "~/.emacs.d/personal/themes/tango-now-light-theme.el")
;; (load-file "~/.emacs.d/personal/themes/gunmetal.el")
;; (load-file "~/.emacs.d/personal/themes/volcano-dark-theme.el")
;; (load-file "~/.emacs.d/personal/themes/volcano-light-theme.el")

(use-package nimbus-theme
  :ensure t)

;; >> AUTO-DARK <<
;; Package for syncing themes with system
(use-package auto-dark
  :ensure t
  :diminish auto-dark-mode
  :config
  (setq auto-dark-light-theme 'twilight-bright)
  (setq auto-dark-dark-theme 'tango-now)
  (setq auto-dark-polling-interval 3)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

(custom-set-faces
 ;; set fringe to no background for every theme
 '(fringe ((t (:background nil)))))
