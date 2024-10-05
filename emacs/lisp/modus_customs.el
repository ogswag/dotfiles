;; (setq modus-operandi-palette-overrides ;; this is the light theme
;;       '((cursor "#FB8718")
;;         (bg-main "#FFFFFF")
;;         (fg-main "#323232")
;;         (bg-mode-line-active "#E3F4FF")
;;         (fg-mode-line-active "#427498")
;;         (border-mode-line-active "#427498")
;;         (bg-line-number-inactive bg-main)
;;         (fg-line-number-inactive "#AAAAAA")
;;         (bg-line-number-active bg-main)
;;         (fg-line-number-active "#1D50A8")
;;         ;; icons in the fringe
;;         (fg-prominent-err "#A93939")
;;         (bg-prominent-err "#FFD1D1")
;;         (fg-prominent-warning "#AE7504")
;;         (bg-prominent-warning "#FFF1BA")
;;         (fg-prominent-note "#0063A9")
;;         (bg-prominent-note "#D2ECFF")
;;         ;; parenthesis match
;;         (bg-paren-match "#721045")
;;         (fg-paren-match bg-main)
;;         ;; selection
;;         (bg-region bg-mark-select)
;;         (fg-region fg-mark-select)
;;         ))
;; (setq ef-frost-palette-overrides
;;       '(
;;         (bg-paren "#FFEA00")
;;         (variable fg-main)
;;         (string "#00006A")
;;         (keyword "#1154B0")
;;         (builtin fg-main)
;;         (fnname  fg-main)
;;         (type    fg-main)
;;         (name    keyword)
;;         (bg-hover bg-cyan-intense)
;;         (bg-mode-line "#C1E6FF")
;;         (bg-region cyan-faint)
;;         (fg-region bg-main)
;;         (comment "#008895")
;;         ))
;; (setq ef-tritanopia-dark-palette-overrides
;;       '((bg-paren "#0000BA")
;;         (fg-main "#FF3B3B")
;;         (comment "#BB5F5F")
;;         (bg-region "#010055")
;;         (fg-region "#7785CB")
;;         (bg-hover bg-removed-refine)
;;         (accent-0 red-cooler)
;;         (accent-2 red-faint)
;;         (accent-3 red-cooler)
;;         (link red-warmer)
;;         (prompt red-warmer)
;;         (docstring red-faint)
;;         (info red-faint)
;;         (underline-warning yellow-warmer)
;;         (unerline-info cyan-cooler)
;;         (variable fg-main)
;;         (keyword "#D85D5D")
;;         (builtin "#D85D5D")
;;         (fnname  "#D85D5D")
;;         (type    "#D85D5D")
;;         (name    "#D85D5D")
;;         (string  "#D85D5D")))

;; (defun my-load-light () ;; this is the light one
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      `(font-lock-comment-face ((t (:inherit modus-themes-slant :background "#E8FDEC" :foreground "#0E8929"))))
;;      `(font-lock-constant-face ((t (:background "#F8EFF9" :foreground "#8F3799"))))
;;      ;; `(font-lock-builtin-face ((t (:background "#FDEDF3" :foreground "#D51259"))))
;;      ))
;;   (modus-themes-load-theme 'modus-operandi))

;; (defun my-load-dark () ;; this is the dark one
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      `(font-lock-comment-face ((t (:background nil :foreground "#437453"))))
;;      `(font-lock-constant-face ((t (:background nil :foreground "#7B3A44"))))
;;      )
;;     ))

;; Add theme loading with custom colors hooks
;; (add-hook 'auto-dark-light-mode-hook #'my-load-light)
;; (add-hook 'auto-dark-dark-mode-hook #'my-load-dark)
