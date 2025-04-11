;;; treesitter.el --- tree-sitter setup
;;; Commentary:

;; Code:
(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp" "master" "src")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (c++-mode . c++-ts-mode)
   (c-mode . c-ts-mode)))

;; (defun mp-remove-treesit-sexp-changes ()
;;   (when (eq forward-sexp-function #'treesit-forward-sexp)
;;     (setq forward-sexp-function nil))
;;   (when (eq transpose-sexps-function #'treesit-transpose-sexps)
;;     (setq transpose-sexps-function #'transpose-sexps-default-function))
;;   (when (eq forward-sentence-function #'treesit-forward-sentence)
;;     (setq forward-sentence-function #'forward-sentence-default-function)))

;; (add-hook 'prog-mode-hook #'mp-remove-treesit-sexp-changes)

;;; treesitter.el ends here
