;;; pkg.el --- Package system bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded first by init.el.  Everything after this point may assume that
;; `use-package' is available and that ELPA packages are on `load-path'.

;;; Code:

(setq use-package-expand-minimally t)
(setq use-package-minimum-reported-time 0.1)
(setq use-package-enable-imenu-support t)

(setq package-quickstart-file
      (expand-file-name "package-quickstart.el" user-emacs-directory))

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
(when (and (version< emacs-version "29.1")
           (not (package-installed-p 'use-package)))
  (package-install 'use-package))
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

  ;; `compile-angel-excluded-files' is obsolete as of compile-angel 1.2.1.
  (dolist (file '("/init.el" "/early-init.el"))
    (push file compile-angel-excluded-path-suffixes))

  ;; Compiles .el files prior to loading them via `load' or `require', and
  ;; compiles everything already loaded when the mode is switched on.
  (compile-angel-on-load-mode 1))

;;; pkg.el ends here
