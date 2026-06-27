;; init.el --- My emacs config -*- lexical-binding: t; -*-

;;; Code:
;; set fonts
(set-frame-font
 (let ((fontlist (font-family-list)))
   (cond
    ((member "Liga mononoki" fontlist) "Liga mononoki 13")
    ((member "Cascadia Mono" fontlist) "Cascadia Mono")
    ((member "Consolas" fontlist) "Consolas")
    ((member "Menlo" fontlist) "Menlo")
    ((member "DejaVu Sans Mono" fontlist) "DejaVu Sans Mono")
    (t nil)))
 t t)

(set-face-attribute 'fixed-pitch nil :family
		    (let ((fontlist (font-family-list)))
		      (cond
		       ;; Size MUST NOT be set here - otherwise the font will not scale with the main frame font
		       ((member "Liga mononoki" fontlist) "Liga mononoki")
		       ((member "Cascadia Mono" fontlist) "Cascadia Mono")
		       ((member "Consolas" fontlist) "Consolas")
		       ((member "Menlo" fontlist) "Menlo")
		       ((member "DejaVu Sans Mono" fontlist) "DejaVu Sans Mono")
		       (t nil))))

(set-face-attribute 'variable-pitch nil :family
		    (let ((fontlist (font-family-list)))
		      (cond
		       ;; Size MUST NOT be set here - otherwise the font will not scale with the main frame font
		       ((member "Fira Sans" fontlist) "Fira Sans")
		       ((member "Source Sans 3" fontlist) "Source Sans 3")
		       ((member "Verdana" fontlist) "Verdana")
		       ((member "DejaVu Sans" fontlist) "DejaVu Sans")
		       ((member "Noto Sans" fontlist) "Noto Sans")
		       (t nil))))


(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier 'super)

(add-hook 'after-init-hook #'show-paren-mode)

(setq ns-pop-up-frames nil)

(unless (eq window-system 'mac)
  (setq pixel-scroll-precision-use-momentum nil)
  ;; (pixel-scroll-precision-mode 1)
  (setq scroll-margin 10)
  (setq scroll-conservatively 101))

(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)

(setq default-input-method nil)

(setq native-comp-async-query-on-exit t)

(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

(setq which-func-update-delay 1.0)

(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")
(defun my/shell-command-colorize ()
  (when (equal (buffer-name) "*Shell Command Output*")
    (ansi-color-apply-on-buffer)))
(add-hook 'shell-command-setup-hook #'my/shell-command-colorize)

(setq x-stretch-cursor t)
(setq-default cursor-type 'bar)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(transient-mark-mode t)
(setq mark-even-if-inactive nil)
(add-hook 'deactivate-mark-hook
          (lambda ()
            (setq mark-ring nil)
            (set-marker (mark-marker) nil)))
(delete-selection-mode t)

(setq-default truncate-lines t)

(global-visual-wrap-prefix-mode t)

(electric-pair-mode -1)

(set-fringe-style 0)
(setq-default fringe-indicator-alist
              '((truncation left-triangle right-triangle)
                (continuation left-curly-arrow right-curly-arrow)
                (overlay-arrow . right-triangle)
                (up . up-arrow)
                (down . down-arrow)
                (top top-left-angle top-right-angle)
                (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                (empty-line . empty-line)
                (unknown . question-mark)))


(use-package savehist :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring))
  :init
  (setq history-length 300))

(use-package saveplace :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :init
  (setq save-place-limit 400))

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq auto-revert-use-notify t)
(setq auto-revert-avoid-polling nil)
(setq auto-revert-verbose t)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))
(global-auto-revert-mode t)

(use-package recentf :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :init
  (setq recentf-auto-cleanup 'mode)
  (setq recentf-exclude
        (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
              "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
              "\\.7z$" "\\.rar$"
              "COMMIT_EDITMSG\\'"
              "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
              "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "✦")
  (uniquify-after-kill-buffer-p t))

;; Global: make buttons appear in-buffer (clickable with mouse-1)
(setq outline-minor-mode-use-buttons t)

;; Optional: cycle visibility with Tab on headings
(setq outline-minor-mode-cycle t)
(setq outline-minor-mode-use-buttons 'in-margins)
;; (setq outline-minor-mode-highlight 'override)

;;><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
;;
;;    A little bit of packaging
;;><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
;;
;;
;; Initialize and refresh package contents again if needed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(when (and (version< emacs-version "29.1")
           (not (package-installed-p 'use-package)))
  (package-install 'use-package))
(require 'use-package)

(use-package exec-path-from-shell   :ensure t
  :if (and (or (display-graphic-p) (daemonp))
           (eq system-type 'darwin)) ; macOS only
  :demand t
  :functions exec-path-from-shell-initialize
  :config
  (dolist (var '("TMPDIR"
                 "SSH_AUTH_SOCK" "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 ;; "FZF_DEFAULT_COMMAND" "FZF_DEFAULT_OPTS" ; fzf
                 ;; "VIRTUAL_ENV" ; Python
                 ;; "GOPATH" "GOROOT" "GOBIN" ; Go
                 ;; "CARGO_HOME" "RUSTUP_HOME" ; Rust
                 ;; "NVM_DIR" "NODE_PATH" ; Node/JS
                 "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  ;; Initialize
  (exec-path-from-shell-initialize))

(use-package compile-angel :ensure t
  :demand t
  :config
  ;; The following disables compilation of packages during installation, compile-angel will handle it.
  (setq package-native-compile nil)

  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  (compile-angel-on-load-mode 1))

(use-package which-key :ensure nil
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(use-package rainbow-delimiters :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

(use-package hide-mode-line :ensure t
  :commands (hide-mode-line-mode global-hide-mode-line-mode))

(setq-default default-input-method 'russian-computer) ; add russian-computer to input languages

(use-package reverse-im :ensure t :demand t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t)
  ;; On Linux with Fcitx/IBus, also configure native input method
  (when (eq system-type 'gnu/linux)
    ;; Try to use IBus if available
    (if-let* ((ibus-method (getenv "IBUS_ADDRESS")))
        (message "IBus detected, using native input method")
      ;; Fall back to reverse-im
      (message "Using reverse-im for Russian input"))))

(use-package markdown-mode :ensure t)

(use-package undo-fu :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))

(use-package undo-fu-session :ensure t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

(use-package zig-mode :ensure t)

(use-package vertico :ensure t
  :init (vertico-mode))
(use-package vertico-directory
  :after vertico
  :ensure nil  ; vertico-directory is included with vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)      ; Enter directories
              ("DEL" . vertico-directory-delete-char) ; Smart backspace
              ("C-DEL" . vertico-directory-delete-word)) ; Delete whole directory
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia :ensure t
  :after vertico
  :commands (marginalia-mode marginalia-cycle)
  :init (marginalia-mode))

(use-package mwim :ensure t)

;;><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'no-message)

(load "~/.emacs.d/m/k.el" 'no-message)

(load "~/.emacs.d/m/l.el" 'no-message)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(setq tex-default-mode 'latex-mode)
(setq TeX-force-default-mode t)

;;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;END;

(provide 'init.el)

;;; init.el ends here
