(use-package lsp-mode
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

;; (use-package lsp-treemacs
;;   :ensure t)
;; 
;; (use-package projectile
;;   :ensure t)
;; 
;; (use-package hydra
;;   :ensure t)
;; 
;; (use-package flycheck
;;   :ensure t)
;; 
(use-package company
  :ensure t)

;; (use-package dap-mode
;;   :ensure t)

(use-package marginalia
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)
(dolist (hook '(prog-mode-hook LaTeX-mode-hook toml-ts-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'company-mode))

;; ;; Use minimalist Ivy for most things.
;; (use-package ivy
;;   :ensure t
;;   :diminish                             ;; don't show Ivy in minor mode list
;;   :config
;;   (ivy-mode 1)                          ;; enable Ivy everywhere
;;   (setq ivy-use-virtual-buffers t)      ;; show bookmarks and recent files in buffer list
;;   (setq ivy-count-format "(%d/%d) ")
;;   (setq enable-recursive-minibuffers t)
;; 
;;   (setq ivy-re-builders-alist
;;       '((swiper . ivy--regex-plus)
;;         (t      . ivy--regex-fuzzy)))   ;; enable fuzzy searching everywhere except for Swiper
;; 
;;   (global-set-key (kbd "s-b") 'ivy-switch-buffer)  ;; Cmd+b show buffers and recent files
;;   (global-set-key (kbd "M-s-b") 'ivy-resume))      ;; Alt+Cmd+b resume whatever Ivy was doing
;; 
;; 
;; ;; Swiper is a better local finder.
;; (use-package swiper
;;   :config
;;   (global-set-key "\C-s" 'swiper)       ;; Default Emacs Isearch forward...
;;   (global-set-key "\C-r" 'swiper)       ;; ... and Isearch backward replaced with Swiper
;;   (global-set-key (kbd "s-f") 'swiper)) ;; Cmd+f find text
;; 
;; 
;; ;; Better menus with Counsel (a layer on top of Ivy)
;; (use-package counsel
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x)            ;; Alt+x run command
;;   (global-set-key (kbd "s-P") 'counsel-M-x)            ;; Cmd+Shift+p run command
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)  ;; Replace built-in Emacs 'find file' (open file) with Counsel
;;   (global-set-key (kbd "s-o") 'counsel-find-file))     ;; Cmd+o open file
;; 
;; (use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
;; (use-package flx)   ;; enable fuzzy matching
;; 
;; 
;; ;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   (setq ivy-rich-path-style 'abbrev)) ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)

;; 
;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)  ;; clangd is fast
;; 
;;(with-eval-after-load 'lsp-mode
;;  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;  (require 'dap-cpptools)
;;  (yas-global-mode))
;;
;;(setq company-backends '((company-capf company-dabbrev company-files company-yasnippet)))
