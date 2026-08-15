;;; persist.el --- State that survives restarts -*- lexical-binding: t; -*-

;;; Commentary:
;; history, cursor positions, recent files, auto-revert.

;;; Code:

;;;; Minibuffer and kill-ring history

(use-package savehist :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     ;; Not `mark-ring': m/edit.el clears it on every `deactivate-mark'.
     global-mark-ring                 ; marks
     search-ring regexp-search-ring
     optex-macro-history              ; m/optex.el: recent macros after \
     my/tex-symbol-history            ; m/tex-symbols.el
     my/tex-surround-history
     my/task-selection                ; m/task.el: which command Run/Build start
     my/task-user-commands            ; m/task.el: commands added from the picker
     my/python-env-selection          ; m/py.el: the virtualenv chosen per project
     my/sar-style                     ; m/sar.el: plain, wildcard or regex
     my/sar-case-sensitive            ; m/sar.el: the Aa toggle
     my/sar-whole-word                ; m/sar.el: the ab| toggle
     my/sar-max-depth                 ; m/sar.el: how far down a folder search goes
     my/sar-search-file-regexp        ; m/sar.el: which files are read
     my/sar-replace-file-regexp       ; m/sar.el: which of those are rewritten
     corfu-history))                  ; m/cmpl.el: candidate ordering
  :init
  (setq history-length 300)
  (put 'kill-ring 'history-length 25))

;;;; Cursor position per file

(use-package saveplace :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :init
  (setq save-place-limit 400))

;;;; Recent files

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
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;;;; Reverting

(setq auto-revert-use-notify t)
(setq auto-revert-avoid-polling nil)
(setq auto-revert-verbose t)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))
(global-auto-revert-mode t)

;;; persist.el ends here
