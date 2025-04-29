;;; -*- lexical-binding: t; -*-

(setq warning-suppress-types '((files)))
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq native-comp-async-report-warnings-errors 'silent)

(setq mac-command-modifier 'hyper)   ;; make cmd key do Meta
(setq mac-option-modifier 'meta)     ;; make opt key do Super
(setq mac-control-modifier 'control) ;; make Control key do Control
(load "~/.emacs.d/elisp/keys.el")


(fringe-mode '(8 . 0))
(setq-default indicate-empty-lines t)


(tool-bar-mode 0)
(setq use-dialog-box nil)
(scroll-bar-mode 0)
(context-menu-mode 1)


(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))


(setq
 make-backup-files nil  ;; disable automatic backup~ file
 auto-save-default nil
 create-lockfiles nil)  ;; stop creating #auto-save# files


(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)


(setq-default ring-bell-function 'ignore)

(setq vc-follow-symlinks t)  ;; auto follow symlinkgs without asking

(setq use-short-answers t)   ;; Use y-n instead of yes-no

(global-goto-address-mode t) ;; Make links clickable


;; Fill column
(setq-default fill-column 240)
;; Fill column ruler
(use-package display-fill-column-indicator-mode
  :hook (prog-mode LaTeX-mode))


(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(setq-default tab-width 4)          ;; Set tab width to 4 spaces
(setq-default c-ts-mode-indent-style 'bsd)
(setq-default c-ts-mode-indent-offset 4)
(electric-indent-mode 1)
(electric-pair-mode 1)


(blink-cursor-mode 1) ;; Stop (or don't) cursor blinking
(setq-default cursor-type 'bar) ;; Set cursor shape


(delete-selection-mode 1) ;; Paste over selected region


(subword-mode 1) ;; Navigate inside camelCaseWords


;; Enable mouse in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))


(add-hook 'prog-mode-hook #'column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (setq-default display-line-numbers 'relative)
;; (setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-grow-only t)
(setq-default display-line-numbers-width-start t)


;; Do not wrap line by default, unless in specific modes
(setq-default truncate-lines t)
(use-package visual-line
  :hook (LaTeX-mode toml-ts-mode))


(global-hl-line-mode t)


;; Allow right-left scrolling with mouse
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)


;; Delete trailing whitespace before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Simple and clean whitespace mode setup
(progn
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46])
          (newline-mark 10 [182 10])
          (tab-mark 9 [9655 9] [92 9])
          )))


(savehist-mode t) ;; Persist history over Emacs restarts
(save-place-mode t) ;; Save place in buffer
(global-auto-revert-mode 1) ;; auto revert/refresh file when change detected
(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "private/tmp")
  (recentf-mode 1))


;; Ignore case in completion
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)


(setq custom-safe-themes t)


(require 'project)


(setq-default default-input-method 'russian-computer)


(defun compile-yandex-g++14.1 ()
  "Compile current .cpp file with optimizations and run the executable."
  (interactive)
  (let* ((cpp-file (buffer-file-name))
         (build-file (replace-regexp-in-string "\\.cpp$" ".cpp.build" cpp-file))
         (compile-command (format "g++ -O2 -lm -std=c++20 -x c++ %s -o %s"
                                  (shell-quote-argument cpp-file)
                                  (shell-quote-argument build-file))))

    (unless cpp-file
      (error "Buffer is not visiting a file"))

    (unless (string-match "\\.cpp$" cpp-file)
      (error "Not a C++ file"))

    ;; Compile the program
    (if (zerop (shell-command compile-command))
        ;; If compilation succeeds, run the executable
        (progn
          (message "Compilation successful. Running...")
          (async-shell-command build-file))
      ;; Show error message if compilation fails
      (error "Compilation failed"))))


(cond
 ((eq system-type 'windows-nt)
  (when (member "Cascadia Code 12" (font-family-list))
    (set-frame-font "Cascadia Code" t t)
    (set-face-attribute 'fixed-pitch nil :family "Cascadia Code")
    (set-face-attribute 'variable-pitch nil :family "Calibri")))
 ((eq system-type 'darwin)
  (when (member "SF Mono" (font-family-list))
    (set-frame-font "SF Mono 13" t t)
    (set-face-attribute 'fixed-pitch nil :family "Monaco")
    (set-face-attribute 'variable-pitch nil :family "Arial")))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono 12" t t)
    (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))))


;; (load "~/.emacs.d/elisp/colors.el")
;; (load "~/.emacs.d/elisp/font.el")
(load "~/.emacs.d/elisp/treesitter.el")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

;; (add-to-list 'load-path "~/.emacs.d/package-local/")

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(use-package vimrc-mode
  :ensure t
  :defer t
  :mode ("\\.vim\\'" "vimrc")
  :commands (vimrc-mode))

(use-package rainbow-mode
  :ensure t
  :defer t
  :commands (rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :demand t
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package which-key
  :ensure nil ; builtin
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(use-package avy
  :ensure t
  :demand t
  :bind ("H-j" . 'avy-goto-char))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :defer t
  :commands (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package highlight-doxygen
  :ensure t
  :defer t
  :commands (highlight-doxygen-global-mode)
  :hook (c++-mode c++-ts-mode))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.md\\'"
  :commands (markdown-mode))

(use-package rmsbolt
  :ensure t
  :defer t
  :commands (rmsbolt))

(use-package ace-window
  :ensure t
  :demand t)
(ace-window-display-mode 1)

(use-package undo-fu
  :ensure t
  :demand t
  :bind
  (("C-{" . undo-fu-only-undo)
   ("C-}" . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :demand t
  :config
  (undo-fu-session-global-mode t))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :init (vertico-mode))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package monokai-theme
  :ensure t)

(use-package standard-themes
  :ensure t)

(use-package atom-one-dark-theme
  :ensure t
  :demand t)

(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(load-theme 'polar-bear-colored t)  ; Load the built-in theme

;; (use-package auto-dark
;;   :ensure t
;;   :custom
;;   (auto-dark-themes '((ef-owl) (ef-duo-light)))
;;   (auto-dark-polling-interval-seconds 5)
;;   (auto-dark-allow-osascript t)
;;   ;; (auto-dark-detection-method nil) ;; dangerous to be set manually
;;   :hook
;;   (auto-dark-dark-mode
;;    . (lambda ()
;;        ;; something to execute when dark mode is detected
;;        ))
;;   (auto-dark-light-mode
;;    . (lambda ()
;;        ;; something to execute when light mode is detected
;;        ))
;;   :init (auto-dark-mode))

(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C++"   (clang-format "--style=file" "--fallback-style=webkit"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

(use-package devdocs
  :ensure t)

(use-package eglot
  :ensure nil
  :defer t
  :hook
  ((c++-ts-mode . eglot-ensure)  ; For Tree-sitter C++ mode
   (c-ts-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure))   ; For Tree-sitter C mode
  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  ;; (add-hook 'c++-mode-hook #'eglot-ensure)
  ;; (add-hook 'c-mode-hook #'eglot-ensure)

  :commands (eglot
             eglot-ensure
             eglot-rename
             eglot-format-buffer))

(setq-default eglot-workspace-configuration
              `(:pylsp (:plugins
                        (;; Fix imports and syntax using `eglot-format-buffer`
                         :isort (:enabled t)
                         :autopep8 (:enabled t)

                         ;; Syntax checkers (works with Flymake)
                         :pylint (:enabled t)
                         :pycodestyle (:enabled t)
                         :flake8 (:enabled t)
                         :pyflakes (:enabled t)
                         :pydocstyle (:enabled t)
                         :mccabe (:enabled t)

                         :yapf (:enabled :json-false)
                         :rope_autoimport (:enabled :json-false)))))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(corfu-popupinfo-mode t)

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))
