;;; spell.el --- Spell checking -*- lexical-binding: t; -*-

;;; Commentary:
;; spellcheck.

;;; Code:

(use-package jinx :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)           ; was `ispell-word'
         ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "en_US ru-yo")
  :config
  (let ((entry '(tex-mode font-latex-math-keywords-face)))
    (if-let* ((cell (assq (car entry) jinx-exclude-faces)))
        (setcdr cell (delete-dups (append (cdr cell) (cdr entry))))
      (push entry jinx-exclude-faces))))

;;; spell.el ends here
