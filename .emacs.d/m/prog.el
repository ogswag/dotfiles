;;; prog.el --- Programming modes and languages -*- lexical-binding: t; -*-

;;; Commentary:
;; programming buffer defaults.

;;; Code:

;;;; General

(setq which-func-update-delay 1.0)

(defconst my/prog-number-regexp
  (rx symbol-start
      (or (seq (or "0x" "0X") (+ (any "0-9a-fA-F_")))
          (seq (or "0b" "0B") (+ (any "01_")))
          (seq (or "0o" "0O") (+ (any "0-7_")))
          (seq (any "0-9") (* (any "0-9_"))
               (? "." (* (any "0-9_")))
               (? (any "eE") (? (any "+-")) (+ (any "0-9")))))
      (? (or (seq (any "uU") (? (any "lL")) (? (any "lL")))
             (seq (any "lL") (? (any "lL")) (? (any "uU")))
             (any "fFdD")))
      symbol-end))

(defvar-local my/prog-number--keywords nil)

(defun my/prog-number-setup ()
  "Highlight numbers with `font-lock-number-face'."
  (unless my/prog-number--keywords
    (setq my/prog-number--keywords
          `((,my/prog-number-regexp 0 'font-lock-number-face)))
    (font-lock-add-keywords nil my/prog-number--keywords 'append)))

(add-hook 'prog-mode-hook #'my/prog-number-setup)

;; `TeX-mode', not `LaTeX-mode': plain TeX is the common case here (m/l.el).
(use-package rainbow-delimiters :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;;; Parens

(setq show-paren-style 'mixed)
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

;;;; Objective-C

(defface my/objc-pragma-mark-overline
  '((t :overline "light gray" :extend t))
  "Dim gray overline across a `#pragma mark -' line.")

(defconst my/objc-pragma-mark-regexp
  "^[ \t]*\\(#pragma mark\\)\\([ \t]+-.*\\)\\(\n\\)?"
  "Line that starts with `#pragma mark -'.")

(defun my/objc-in-body-p (pos)
  "Non-nil if POS is inside @interface/@implementation/@protocol ... @end."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (and (re-search-backward
            "^[ \t]*@\\(implementation\\|interface\\|protocol\\|end\\)\\>"
            nil t)
           (not (looking-at "[ \t]*@end\\>"))))))

(defun my/objc-pragma-mark-matcher (limit)
  "Find the next `#pragma mark -' line in an ObjC body before LIMIT."
  (catch 'found
    (while (re-search-forward my/objc-pragma-mark-regexp limit t)
      (when (my/objc-in-body-p (match-beginning 0))
        (throw 'found t)))
    nil))

(defvar-local my/objc-pragma-mark--keywords nil)

(defun my/objc-pragma-mark-setup ()
  "Highlight `#pragma mark -' section headers in this ObjC buffer."
  (unless my/objc-pragma-mark--keywords
    (setq my/objc-pragma-mark--keywords
          '((my/objc-pragma-mark-matcher
             (1 'font-lock-preprocessor-face t)
             (2 'default t)
             (0 'my/objc-pragma-mark-overline prepend))))
    (font-lock-add-keywords nil my/objc-pragma-mark--keywords 'append)))

(add-hook 'objc-mode-hook #'my/objc-pragma-mark-setup)

;;; prog.el ends here
