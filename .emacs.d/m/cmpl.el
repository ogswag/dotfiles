;;; cmpl.el --- Completion and discoverability -*- lexical-binding: t; -*-

;;; Commentary:
;; completion (corfu/vertico/etc).

;;; Code:

(use-package which-key :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  ;; which-key.nvim runs at `delay = 500'.
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.05)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;;;; Icons

;; Set by m/font.el, which loads first.  Declared so the byte compiler can see
(defvar my/nerd-mono-font)

(use-package nerd-icons :ensure t
  :demand t
  :config
  ;; nerd-icons defaults to "Symbols Nerd Font Mono", which is not installed here.
  (when my/nerd-mono-font
    (setopt nerd-icons-font-family my/nerd-mono-font)))

;;;; Minibuffer

(use-package vertico :ensure t
  :init (vertico-mode))

(use-package vertico-directory :ensure nil ; ships with vertico
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)       ; Enter directories
              ("DEL" . vertico-directory-delete-char) ; Smart backspace
              ("C-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; fzf/fzy-style scoring for every completing-read and completion-at-point UI.
(use-package fzf-native
  :vc (:url "https://github.com/dangduc/fzf-native" :rev :newest))

(use-package fussy
  :vc (:url "https://github.com/jojojames/fussy" :rev :newest)
  :config
  (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)
  (fussy-setup-fzf)
  (fussy-eglot-setup)
  (fussy-company-setup)
  (fussy-setup))

(use-package marginalia :ensure t
  :after vertico
  :commands (marginalia-mode marginalia-cycle)
  :init (marginalia-mode))

(use-package nerd-icons-completion :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;;; In-buffer completion

(use-package corfu :ensure t
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-count 20)
  (corfu-min-width 24)
  (corfu-preview-current 'insert)
  (corfu-preselect 'valid)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil)
  (corfu-border-width 1)
  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ("<tab>" . corfu-insert)
        ("S-TAB" . corfu-previous)
        ("<backtab>" . corfu-previous)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-e" . corfu-quit)
        ("RET" . nil))
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay '(0.15 . 0.1))
  (with-eval-after-load 'nerd-icons-corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(use-package nerd-icons-corfu :ensure t :after corfu)

;;;; Extra sources

(use-package cape :ensure t
  :demand t
  :config
  (add-hook 'completion-at-point-functions #'cape-file 20))

(use-package tempel :ensure t
  :demand t
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("<tab>" . tempel-next)
              ("S-TAB" . tempel-previous)
              ("<backtab>" . tempel-previous))
  :config
  (add-hook 'completion-at-point-functions #'tempel-complete 30))

;;; cmpl.el ends here
