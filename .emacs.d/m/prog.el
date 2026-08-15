;;; prog.el --- Programming modes and languages -*- lexical-binding: t; -*-

;;; Commentary:
;; programming buffer defaults.

;;; Code:

;;;; General

(setq which-func-update-delay 1.0)

;; `TeX-mode', not `LaTeX-mode': plain TeX is the common case here (m/l.el).
(use-package rainbow-delimiters :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;;; Parens

(setq show-paren-style 'expression)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-context-when-offscreen 'overlay)
(setq blink-matching-paren 'jump-offscreen)

(use-package elec-pair :ensure nil ; built-in
  :commands (electric-pair-local-mode electric-pair-mode)
  :hook ((TeX-mode LaTeX-mode latex-mode tex-mode markdown-mode prog-mode) . electric-pair-local-mode))

;;;; Structural editing

(use-package puni :ensure t
  :hook (prog-mode . puni-mode)
  :bind (:map puni-mode-map
              ("M-d" . nil)
              ("M-DEL" . nil)
              ("M-(" . nil)
              ("M-)" . nil)
              ("C-)" . puni-slurp-forward)
              ("C-}" . puni-barf-forward)
              ("C-(" . puni-slurp-backward)
              ("C-{" . puni-barf-backward)
              :map emacs-lisp-mode-map
              ("C-c s" . puni-splice)
              ("C-c r" . puni-raise)
              ("C-c w" . puni-wrap-round)))

;;;; Diagnostics

(use-package flymake :ensure nil
  :hook (emacs-lisp-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 1.0)
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicator-position 'right-margin)
  (flymake-margin-indicators-string
   '((error "!" compilation-error)
     (warning "?" compilation-warning)
     (note "i" compilation-info))))

;;;; Emacs Lisp

(defun my/elisp-check-parens ()
  "Abort the save when brackets are unbalanced.  For `write-contents-functions'."
  (check-parens)
  ;; nil = "I did not write the file; keep going".
  nil)

(defun my/elisp-check-parens-on-save ()
  "Refuse to save this buffer while its brackets are unbalanced."
  (add-hook 'write-contents-functions #'my/elisp-check-parens nil t))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-check-parens-on-save)

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

;; Evaluation results next to point rather than in the echo area.
(use-package eros :ensure t
  :hook (after-init . eros-mode))

(use-package macrostep :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c m" . macrostep-expand)))


;;;; Shell

(setq-default sh-basic-offset 4)

;;;; Markdown

(use-package markdown-mode :ensure t)

;;;; Zig

(use-package zig-mode :ensure t)

;;; prog.el ends here
