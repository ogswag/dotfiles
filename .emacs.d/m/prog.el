;;; prog.el --- Programming modes and languages -*- lexical-binding: t; -*-

;;; Commentary:
;; LaTeX has enough setup of its own to warrant m/l.el.

;;; Code:

;;;; General

(add-hook 'after-init-hook #'show-paren-mode)

(setq which-func-update-delay 1.0)

(use-package rainbow-delimiters :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

;;;; Shell

(setq-default sh-basic-offset 4)

;;;; Markdown

(use-package markdown-mode :ensure t)

;;;; Zig

(use-package zig-mode :ensure t)

;;; prog.el ends here
