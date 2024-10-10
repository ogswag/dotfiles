;;; completion-bundle.el --- LSP and Minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:
;;; LSP, Minibuffer and other completion related settings in one package

;;; Code:
;; >> VERTICO <<
;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :init (vertico-mode)
  )

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; >> ORDERLESS <<
;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; >> CONSULT <<
;; Extended completion utilities
(use-package consult
  :ensure t
  :bind (("C-c i"   . #'consult-imenu)
         ("C-c l"   . #'consult-line)
         ("C-c b"   . #'consult-buffer)
         ("C-x b"   . #'consult-buffer)
         ("C-c r"   . #'consult-recent-file)
         ("C-c R"   . #'consult-bookmark)
         ("C-c `"   . #'consult-flymake)
         ("C-c h"   . #'consult-ripgrep)))

;; >> MARGINALIA <<
;; Marginalia (documentation and notes) in the minibuffer
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; >> EGLOT <<
;; LSP Support
(use-package eglot
  :ensure t
  :diminish eglot
  :config
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider)
        eglot-send-changes-idle-time 3    ;; be slower sending changes
        eglot-extend-to-xref t            ;; external files ok
        eglot-events-buffer-size 100000)  ;; smaller events buffer
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--fallback-style=Google"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    )))
  :hook ((c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (c-mode . eglot-ensure))
  :custom (eglot-autoshutdown t)
  )

;; >> FLYMAKE <<
;; Inline static analysis
(use-package flymake
  :diminish flymake-mode
  :hook (prog-mode-hook . flymake-mode))

;; >> CORFU <<
;; Pop-up completion
(use-package corfu
  :ensure t
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt)    ;; Preselect the candidate
  (corfu-auto t)               ;; Enable auto completion
  (corfu-quit-at-boundary t)   ;; (Never) quit at completion boundary
  (corfu-quit-no-match t)      ;; (Never) quit, even if there is no match
  (corfu-popupinfo-delay 0.2)
  (corfu-auto-delay  0.3)
  (corfu-auto-prefix 2)

  :config
  ;; Unbind RET completely
  (keymap-unset corfu-map "RET")
  (corfu-echo-mode t)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t)

  :bind
  ;; Use TAB for cycling, default is `corfu-complete'.
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("ESC" . corfu-quit)
        ([escape] . corfu-quit)
        )

  :hook ((prog-mode . corfu-mode)
         (toml-ts-mode . corfu-mode)))

;; >> CAPE <<
;; Add completion extensions
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)     ;; Complete word from current buffers. See also dabbrev-capf on Emacs 29.
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete Elisp in Org or Markdown code block.
  (add-hook 'completion-at-point-functions #'cape-file)        ;; Complete file name.
  (add-hook 'completion-at-point-functions #'cape-history)     ;; Complete from Eshell, Comint or minibuffer history.
  (add-hook 'completion-at-point-functions #'cape-keyword)     ;; Complete programming language keyword.
  (add-hook 'completion-at-point-functions #'cape-tex)         ;; Complete Unicode char from TeX command, e.g. `\hbar'.
  )

(provide 'completion-bundle)
;;; completion-bundle.el ends here
