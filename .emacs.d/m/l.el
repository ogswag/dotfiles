;;; l.el --- LaTeX development environment -*- lexical-binding: t; -*-

;;; Code:

;; AUCTeX installs a `major-mode-remap-alist' entry sending `latex-mode' to its
;; own `LaTeX-mode', so no `auto-mode-alist' entry is needed for .tex here.
(setq tex-default-mode 'latex-mode)
(setq TeX-force-default-mode t)

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

;;;; Switch languages inside math delimiters (emacs-mac only)

(defvar-local my/latex-math--saved-source nil
  "Saved macOS input source before entering math mode.")

(defconst my/mac-input-source-available-p
  (and (fboundp 'mac-input-source)
       (fboundp 'mac-select-input-source))
  "Non-nil if this build provides the macOS input-source functions.")

(defun my/latex-math-auto-input-source ()
  "Switch macOS input to English in LaTeX math, restore on exit.
No-op unless the current build provides `mac-input-source' and
AUCTeX's `texmathp' is available."
  (when (and my/mac-input-source-available-p
             (or (fboundp 'texmathp) (require 'texmathp nil t))
             (derived-mode-p 'latex-mode)
             (eq (current-buffer) (window-buffer (selected-window))))
    (let ((current-source (funcall 'mac-input-source))
          (in-math (ignore-errors (funcall 'texmathp))))
      (cond
       ((and in-math (not my/latex-math--saved-source))
        ;; Already on English — mark as such but don't bother switching
        (setq my/latex-math--saved-source
              (if (equal current-source "com.apple.keylayout.US")
                  :already-english
                current-source))
        (unless (equal current-source "com.apple.keylayout.US")
          (funcall 'mac-select-input-source "com.apple.keylayout.US")))
       ((and (not in-math) my/latex-math--saved-source)
        (unless (eq my/latex-math--saved-source :already-english)
          (funcall 'mac-select-input-source my/latex-math--saved-source))
        (setq my/latex-math--saved-source nil))))))

;; (when my/mac-input-source-available-p
;;   (add-hook 'post-command-hook #'my/latex-math-auto-input-source))

(when my/mac-input-source-available-p
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-hook 'post-command-hook
                        #'my/latex-math-auto-input-source nil t))))

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

(use-package laas :ensure t
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

(add-hook 'LaTeX-mode-hook #'visual-line-mode)

;;; l.el ends here
