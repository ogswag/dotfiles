;;; k.el --- Keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded last, so every command bound here already exists.

;;; Code:

(use-package no-wheel
  :load-path "~/.emacs.d/m/p")

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; UNBINDS
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; Wheel/trackpad zooming — no-wheel-mode also shadows these, but unbinding
;; them keeps the global map honest.
(keymap-global-unset "C-M-<wheel-down>" t) ; mouse-wheel-global-text-scale
(keymap-global-unset "C-M-<wheel-up>" t)   ; mouse-wheel-global-text-scale
(keymap-global-unset "C-<wheel-down>" t)   ; mouse-wheel-text-scale
(keymap-global-unset "C-<wheel-up>" t)     ; mouse-wheel-text-scale
(keymap-global-unset "C-<mouse-5>")        ; mouse-wheel-text-scale down
(keymap-global-unset "C-<mouse-4>")        ; mouse-wheel-text-scale up
(keymap-global-unset "C-M-<mouse-5>")      ; mouse-wheel-global-text-scale down
(keymap-global-unset "C-M-<mouse-4>")      ; mouse-wheel-global-text-scale up

;; Secondary selection — too easy to trigger by accident.
(keymap-global-unset "<mouse-2>")          ; middle click secondary yank
(keymap-global-unset "M-<mouse-1>")        ; set secondary selection start
(keymap-global-unset "M-<mouse-3>")        ; set secondary selection end
(keymap-global-unset "M-<drag-mouse-1>")
(keymap-global-unset "C-M-<down-mouse-1>")

;; Suspend-frame, in all its spellings.
(keymap-global-unset "C-z" t)
(keymap-global-unset "C-Z" t)
(keymap-global-unset "C-S-z" t)

(keymap-global-unset "s-L")

(keymap-global-unset "C-<SPC>")
(keymap-global-unset "C-@")

(keymap-global-unset "C-M-/" t)

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; GENERAL
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; Case conversion: dwim variants act on the region when there is one.
(keymap-global-set "M-l" #'downcase-dwim)    ; downcase-word
(keymap-global-set "M-u" #'upcase-dwim)      ; upcase-word
(keymap-global-set "M-c" #'capitalize-dwim)  ; capitalize-word

;;;; macOS-standard Super (Cmd) bindings
(keymap-global-set "s-c" #'kill-ring-save)
(keymap-global-set "s-x" #'kill-region)
(keymap-global-set "s-v" #'yank)
(keymap-global-set "s-V" #'yank-pop)
(keymap-global-set "s-z" #'undo-fu-only-undo)
(keymap-global-set "s-Z" #'undo-fu-only-redo)
(keymap-global-set "s-a" #'mark-whole-buffer)
(keymap-global-set "s-s" #'save-buffer)
(keymap-global-set "s-S" #'write-file)
(keymap-global-set "s-l" #'my/mark-line)
(keymap-global-set "s-L" #'goto-line)
(keymap-global-set "s-f" #'isearch-forward)
(keymap-global-set "s-F" #'isearch-backward)
(keymap-global-set "s-g" #'isearch-repeat-forward)
(keymap-global-set "s-w" #'delete-window)
(keymap-global-set "s-W" #'delete-frame)
(keymap-global-set "s-n" #'make-frame-command)
(keymap-global-set "s-m" #'iconify-frame)
(keymap-global-set "s-`" #'other-frame)
(keymap-global-set "C-s-f" #'toggle-frame-fullscreen)

(keymap-global-set "C-=" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)
(keymap-global-set "C-0" #'text-scale-adjust)

(keymap-global-set "M-[" #'backward-paragraph)
(keymap-global-set "M-]" #'forward-paragraph)

;; Beginning/end of code before beginning/end of line.
(keymap-global-set "C-a" #'mwim-beginning)
(keymap-global-set "C-e" #'mwim-end)

(keymap-global-set "M-S-<down-mouse-1>" #'mouse-drag-region-rectangle)

(keymap-global-set "M-j" #'join-line)

(keymap-global-unset "C-/")
(keymap-global-set "C-/" #'comment-line)

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; SET ARROW KEYS IN ISEARCH
;; left/right is backward/forward.
;; up/down is search history.
;; This way, searching forward/backward is just one single key press, no key combination.
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
(keymap-set isearch-mode-map "<up>" #'isearch-ring-retreat)
(keymap-set isearch-mode-map "<down>" #'isearch-ring-advance)

(keymap-set isearch-mode-map "<left>" #'isearch-repeat-backward)
(keymap-set isearch-mode-map "<right>" #'isearch-repeat-forward)

(keymap-set isearch-mode-map "C-p" #'isearch-repeat-backward)
(keymap-set isearch-mode-map "C-n" #'isearch-repeat-forward)

(keymap-set minibuffer-local-isearch-map "<left>" #'isearch-reverse-exit-minibuffer)
(keymap-set minibuffer-local-isearch-map "<right>" #'isearch-forward-exit-minibuffer)

(keymap-set minibuffer-local-map "C-p" #'previous-line-or-history-element)
(keymap-set minibuffer-local-map "C-n" #'next-line-or-history-element)

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; WORD MOTION
;; viper's notion of a word stops at punctuation boundaries the way vi does,
;; which is less jumpy than `forward-word'.  Autoloaded rather than required,
;; so viper is only pulled in on first use.
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
(autoload 'viper-forward-word "viper-cmd" nil t)
(autoload 'viper-backward-word "viper-cmd" nil t)

(defun my/backward-word (arg)
  "`viper-backward-word' with shift-selection support."
  (interactive "^P")
  (viper-backward-word arg))

(defun my/forward-word (arg)
  "`viper-forward-word' with shift-selection support."
  (interactive "^P")
  (viper-forward-word arg))

(keymap-global-set "M-<left>"  #'my/backward-word)
(keymap-global-set "M-<right>" #'my/forward-word)
(keymap-global-set "C-<left>"  #'my/backward-word)
(keymap-global-set "C-<right>" #'my/forward-word)
(keymap-global-set "M-b" #'my/backward-word)
(keymap-global-set "M-f" #'my/forward-word)

(defun my/backward-delete-word (arg)
  "Delete (not kill) ARG words backward."
  (interactive "p")
  (delete-region (point) (progn (viper-backward-word arg) (point))))

(defun my/forward-delete-word (arg)
  "Delete (not kill) ARG words forward."
  (interactive "p")
  (delete-region (point) (progn (viper-forward-word arg) (point))))

;; Without this `delete-selection-mode' ignores them, so with a region up they
;; would eat a word and leave the selection behind.  `shift-select-mode' is
;; `permanent' (see m/edit.el), so regions linger and that case is common.
(put 'my/backward-delete-word 'delete-selection 'supersede)
(put 'my/forward-delete-word 'delete-selection 'supersede)

(keymap-global-set "M-<backspace>" #'my/backward-delete-word)
(keymap-global-set "C-<backspace>" #'my/backward-delete-word)
(keymap-global-set "M-d" #'my/forward-delete-word)
(keymap-global-set "C-<delete>" #'my/forward-delete-word)

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; COMMANDS
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
(defun my/mark-line (&optional arg allow-extend)
  "Mark ARG whole lines, trailing newline included.

Mark goes to the beginning of the current line, point to the beginning
of the line after the last one marked.  A negative ARG marks upward.

Repeating the command -- e.g. hitting its key again -- or invoking it
with an active region grows the selection by ARG lines instead of
starting over, snapping both ends to line boundaries."
  (interactive "P\np")
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (use-region-p)))
      (let* ((backward (< (point) (mark)))
             (n (if arg (prefix-numeric-value arg) (if backward -1 1))))
        ;; Keep the anchor on a line boundary, in case the region came
        ;; from somewhere else (mouse drag, C-SPC, ...).
        (set-mark (save-excursion
                    (goto-char (mark))
                    (line-beginning-position (if backward 2 1))))
        (goto-char (line-beginning-position (1+ n))))
    (let ((n (prefix-numeric-value arg)))
      (push-mark (line-beginning-position (if (> n 0) 1 2)) nil t)
      (goto-char (line-beginning-position (if (> n 0) (1+ n) (+ 2 n)))))))

(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(keymap-global-set "C-g" #'my/keyboard-quit-dwim)

;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
;; TURN FORWARD SLASH INTO A LEADER KEY
;; "/" itself is rebound to self-insert inside the map, so typing a literal
;; slash is "//".
;;>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
;;
(define-prefix-command 'my-fwdslash-map)
(keymap-global-set "/" 'my-fwdslash-map)

(keymap-set my-fwdslash-map "/" #'self-insert-command)

(keymap-set my-fwdslash-map "a" #'align-regexp)

(keymap-set my-fwdslash-map "b b" #'switch-to-buffer)
(keymap-set my-fwdslash-map "b k" #'kill-current-buffer)

(keymap-set my-fwdslash-map "c c" #'compile)
(keymap-set my-fwdslash-map "c r" #'recompile)

(keymap-set my-fwdslash-map "f f" #'find-file)
(keymap-set my-fwdslash-map "f r" #'recentf-open)

(keymap-set my-fwdslash-map "g" #'my/keyboard-quit-dwim)

(keymap-set my-fwdslash-map "r r" #'replace-regexp)
(keymap-set my-fwdslash-map "r s" #'replace-string)

(keymap-set my-fwdslash-map "m m" #'set-mark-command)
(keymap-set my-fwdslash-map "m b" #'bookmark-set)
(keymap-set my-fwdslash-map "m j" #'bookmark-jump)

(keymap-set my-fwdslash-map "x" #'execute-extended-command)

(keymap-set my-fwdslash-map "1" #'shell-command)
(keymap-set my-fwdslash-map "2" #'async-shell-command)

(keymap-set my-fwdslash-map "3" #'my/open-curdir)  ; defined in m/mac.el

;;; k.el ends here
