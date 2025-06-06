;;; -*- lexical-binding: t; -*-

(setq warning-suppress-types '((files)))
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil t t))

(setq native-comp-async-report-warnings-errors 'silent)

(setq mac-command-modifier 'hyper)   ;; make command key do Hyper
(setq mac-option-modifier 'meta)     ;; make option/alt  key do Meta
(setq mac-control-modifier 'control) ;; make control key do Control

;; Control emacs frame
(use-package moom
  :ensure t
  :custom
  (moom-use-font-module nil)
  :config (moom-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :commands (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(load "~/.emacs.d/elisp/xah-functions.el" nil t t)
(load "~/.emacs.d/elisp/yandex-compile-commands.el" nil t t)
(load "~/.emacs.d/elisp/keys.el" nil t t)

(setq-default frame-resize-pixelwise 1)

(setq org-log-done nil
      org-agenda-files   (list "~/org/")
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file)

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
(setq-default fill-column 120)
;; Fill column ruler
(use-package display-fill-column-indicator-mode
  :hook (prog-mode LaTeX-mode))


(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(setq-default tab-width 4)          ;; Set tab width to 4 spaces
(setq-default c-ts-mode-indent-offset 2)
(setq-default c-ts-mode-indent-style 'gnu)
(setq-default c-indentation-style 'gnu)


(electric-indent-mode 1)
(setq-default electric-indent-inhibit t)
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


(cond
 ((eq system-type 'windows-nt)
  (when (member "Cascadia Code" (font-family-list))
    (set-frame-font "Cascadia Code 12" t t)
    (set-face-attribute 'fixed-pitch nil :family "Cascadia Code")
    (set-face-attribute 'variable-pitch nil :family "Calibri")))
 ((eq system-type 'darwin)
  (cond ((member "Iosevka Berkley" (font-family-list))
         (set-frame-font "Iosevka Berkley 15" t t)
         (set-face-attribute 'fixed-pitch nil :family "Iosevka Berkley")
         (set-face-attribute 'variable-pitch nil :family "Helvetica Neue"))
        ((member "Monaco" (font-family-list))
         (set-frame-font "Monaco 14" t t)
         (set-face-attribute 'fixed-pitch nil :family "Monaco")
         (set-face-attribute 'variable-pitch nil :family "Helvetica Neue"))))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono 12" t t)
    (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))))

(use-package ligature
  :ensure t
  :config
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 't '("--" "---" "==" "===" "!=" "!==" "=!="
                               "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                               "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                               "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                               "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                               "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                               "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                               "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                               "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                               "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                               "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                               ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                               "<:<" ";;;"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


(load "~/.emacs.d/elisp/treesitter.el" nil t t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/" 'nomessage)
(add-to-list 'load-path "~/.emacs.d/themes/" 'nomessage)

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
  :defer t)

(use-package highlight-parentheses
  :ensure t
  :demand t
  :hook (after-init . global-highlight-parentheses-mode))

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
  :bind
  ("H-'" . 'avy-goto-char))

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
  :demand t)

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

(use-package affe
  :ensure
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; The default regular expression transformation of Consult is limited.  It is
;; recommended to configure Orderless as affe-regexp-compiler in Consult.

(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (cdr (orderless-compile input)))
  (cons input (apply-partially #'orderless--highlight input t)))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)


(use-package monokai-theme
  :ensure t
  :defer t)

(use-package standard-themes
  :ensure t
  :defer t)

(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
;; (load-theme 'polar-bear t)

;; (setq-default polar-bear-operator-color "#B14747"
;;               polar-bear-delimiter-color "#B14747"
;;               polar-bear-rainbow-delimiters-style 'strong)

(use-package auto-dark
  :ensure t
  :demand t
  :custom
  (auto-dark-themes '((polar-bear) (twilight-bright)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript t)
  ;; (auto-dark-detection-method nil) ;; dangerous to be set manually
  :hook
  (auto-dark-dark-mode
   . (lambda ()
       ;; something to execute when dark mode is detected
       ))
  (auto-dark-light-mode
   . (lambda ()
       ;; something to execute when light mode is detected
       ))
  :init (auto-dark-mode))

(use-package format-all
  :ensure t
  :commands format-all-mode
  ;; :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C++"    (clang-format "--style=file" "--fallback-style=llvm"))
                  ("Shell"  (shfmt "-i" "4" "-ci"))
                  ("Python" (ruff))
                  ("JSON"   (prettier)
                   ))))

(use-package devdocs
  :ensure t)

(use-package eglot
  :ensure nil
  :defer t
  ;; :hook
  ;; ((c++-ts-mode . eglot-ensure)  ; For Tree-sitter C++ mode
  ;;  (c-ts-mode . eglot-ensure)
  ;;  (python-ts-mode . eglot-ensure))   ; For Tree-sitter C mode
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
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


(defun my-move-bol-or-prev-eol ()
  "Move to beginning of line, or to end of previous line if already at bol."
  (interactive)
  (if (bolp)
      (progn
        (forward-line -1)
        (end-of-line))
    (beginning-of-line)))
