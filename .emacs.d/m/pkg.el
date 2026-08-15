;;; pkg.el --- Package system bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; package.el + use-package bootstrap. load this first.

;;; Code:

(setq use-package-expand-minimally t)
(setq use-package-minimum-reported-time 0.1)
(setq use-package-enable-imenu-support t)

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("melpa"        . 99)
                                   ("gnu"          . 80)
                                   ("nongnu"       . 79)
                                   ("melpa-stable" . 50)))

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)

;; Keep package installation from stealing a window.
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(use-package exec-path-from-shell :ensure t
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
  (exec-path-from-shell-initialize))

(use-package compile-angel :ensure t
  :demand t
  :config
  ;; compile-angel handles compilation, so package.el should not.
  (setq package-native-compile nil)

  ;; Set to nil to stop compile-angel from reporting what it compiles.
  (setq compile-angel-verbose t)

  (dolist (file '("/init.el" "/early-init.el"))
    (push file compile-angel-excluded-path-suffixes))

  ;; Compiles .el files prior to loading them via `load' or `require'.
  (compile-angel-on-load-mode 1))

;;; pkg.el ends here
