;; Set default font face
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
	  (set-frame-font "Consolas 12" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "JetBrainsMono Nerd Font" (font-family-list))
	  (set-frame-font "JetBrainsMono Nerd Font 13" t t)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
	  (set-frame-font "DejaVu Sans Mono 12" t t))))

;; (setq-default line-spacing 4) ;; Change line spacing
(add-to-list 'load-path (expand-file-name "~/.emacs.d/colors/lupan-themes") t)
(require 'lupan-themes)

;; Install custom themes
(use-package leuven-theme
  :ensure t)
;; (use-package one-themes
  ;; :ensure t)
(use-package nimbus-theme
  :ensure t)
(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(use-package monokai-theme
  :ensure t)

;; >> MODUS THEMES CUSTOM OPTIONS <<
;; (setq modus-themes-italic-constructs t)
;; (setq modus-vivendi-palette-overrides
;;       '(
;;         ;; interface
;;         (fringe bg-main)
;;         (bg-hl-line bg-inactive)
;;         (bg-line-number-inactive bg-main)
;;         (bg-line-number-active bg-inactive)
;;         (fg-line-number-active fg-dim)
;;         (fg-line-number-inactive border)
;;         ;; code parts
;;         (string green)
;;         )
;;       )

;; >> AUTO-DARK <<
;; Package for syncinng themes with system 
(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-light-theme 'leuven)
  (setq auto-dark-dark-theme 'monokai)
  (setq auto-dark-polling-interval-seconds 5)
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-allow-powershell t)
  (auto-dark-mode t))

;; >> AUTO TITLEBAR <<
;; correct transparent title bar colors for macOS
(use-package ns-auto-titlebar
  :ensure t)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; >> LIGATURE <<
;; package for ligature support in Emacs
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;"))
  (global-ligature-mode t))
