;;; LSP Support
(use-package eglot
  :ensure t)

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
			   '(python-mode . ("ruff-lsp"))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(LaTeX-mode . ("texlab"))))

(add-hook 'LaTeX-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)
  

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; Message navigation bindings
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)				 ;; Enable cycling for `corfu-next/previous'
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)		; Always show candidates in menu

  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)		; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)

  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions

  :custom
  (setq corfu-popupinfo-delay 1.0)
  :init
  (global-corfu-mode)
  (corfu-echo-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.	Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
)

;;; Completion framework for minibuffer
(use-package vertico
  :custom
  (vertico-count 15)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)
  :config
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;;; Extended completion utilities
(use-package consult
  :ensure t)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-c j") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)
(setq read-buffer-completion-ignore-case t
	  read-file-name-completion-ignore-case t
	  completion-ignore-case t)
