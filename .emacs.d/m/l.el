;; My LaTeX development environment setup -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package math-delimiters
  :load-path "~/.emacs.d/m/p")

(with-eval-after-load 'org
  (define-key org-mode-map "$" #'math-delimiters-insert)
  (define-key org-mode-map "№" #'math-delimiters-insert))

(with-eval-after-load 'tex
  (define-key TeX-mode-map "$" #'math-delimiters-insert)
  (define-key TeX-mode-map "№" #'math-delimiters-insert))

(with-eval-after-load 'tex-mode
  (define-key tex-mode-map "$" #'math-delimiters-insert)
  (define-key tex-mode-map "№" #'math-delimiters-insert))

(use-package auctex :ensure t
  :defer t)

(use-package latex :ensure auctex
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad)))))))

  (setq LaTeX-indent-level 4
        LaTeX-item-indent 0
        TeX-brace-indent-level 4))

(add-hook 'LaTeX-mode-hook #'outline-minor-mode)

(use-package math-symbol-lists :ensure t :defer t)

(use-package laas
  :defer t
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))
