;;; Go Support
(use-package go-mode
  :ensure t)

;;; Haskell Support
(use-package haskell-mode
  :ensure t)

;;; JSON Support
(use-package json-mode
  :ensure t)

;;; Lua Support
(use-package lua-mode
  :ensure t)

;;; NASM Support
(use-package nasm-mode
  :ensure t)

;;; PHP Support
(use-package php-mode
  :ensure t)

;;; Rust Support
(use-package rust-mode
  :ensure t)

;;; Additional Lisp support
(use-package sly
  :ensure t)

;;; Swift Support
(use-package swift-mode
  :ensure t)

;;; Typescript Support
(use-package typescript-mode
  :ensure t)

;;; YAML Support
(use-package yaml-mode
  :ensure t)

;;; Markdown support
(use-package markdown-mode
  :ensure t)

(use-package jinx
  :ensure t)
;; Enable Jinx per mode
(dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
  (add-hook hook #'jinx-mode))
"ещё её ей"
(setq-default jinx-languages "en_US ru-yo")

;; (use-package typst-ts-mode
;;   :quelpa (typst-ts-mode :fetcher codeberg :repo "meow_king/typst-ts-mode"))
;; 
;; (use-package websocket
;;   :ensure t)
;; 
;; (use-package typst-preview
;;   :quelpa (typst-preview :fetcher github :repo "havarddj/typst-preview.el"))
