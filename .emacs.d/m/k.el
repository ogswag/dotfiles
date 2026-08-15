;;; k.el --- Keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; keybindings.

;;; Code:

(use-package no-wheel
  :load-path my-vendor-directory)

;; Bound below but defined in packages that load earlier; named here so.
(declare-function my/speedbar-detach "tree" ())
(declare-function my/speedbar-toggle "tree" ())
(declare-function my/sar-find "sar" (&optional backward))
(declare-function my/sar-replace "sar" ())
(declare-function my/sar-panel-project "sar" ())
(declare-function my/sar-panel-folder "sar" ())
(declare-function my/sar-panel-file "sar" ())
(declare-function my/sar-panel-replace "sar" ())
(declare-function my/sar-panel-toggle "sar" ())
(declare-function my/speedbar-reveal "tree" ())
(declare-function which-key-add-keymap-based-replacements "which-key"
                  (keymap key replacement &rest more))
(declare-function my/open-curdir "mac" ())
(declare-function homebrew-dispatch "homebrew" ())
(declare-function my/tab-prev "tabs" (&optional n))
(declare-function my/tab-next "tabs" (&optional n))
(declare-function my/tab-select "tabs" ())
(declare-function flymake-show-buffer-diagnostics "flymake" ())
(declare-function flymake-show-diagnostic "flymake" (pos &optional other-window))
(declare-function my/format-dwim "fmt" ())
(declare-function my/add-project "proj" (dir))
(declare-function my/delete-project "proj" (root))
(declare-function my/project-discover "proj" (&optional quiet))
(declare-function my/edit-projects "proj" (&optional generated))
(declare-function my/term-toggle "term" ())
(declare-function my/term-new "term" (&optional force-ask))
(declare-function my/term-new-here "term" ())
(declare-function my/term-close "term" (&optional session))
(declare-function my/term-rename "term" (session new-name))
(declare-function my/term-next "term" (&optional n))
(declare-function my/term-prev "term" (&optional n))
(declare-function my/term-select "term" (session))
(declare-function my/term-focus-list "term" ())
(declare-function my/term-set-backend "term" (backend))
(declare-function my/term-cd "term" (directory))
(declare-function my/task-run "task" (&optional pick))
(declare-function my/task-build "task" (&optional pick))
(declare-function my/task-stop "task" ())
(declare-function my/task-select "task" (kind))
(declare-function my/task-add "task" (kind &optional ask-directory))
(declare-function my/task-env-select "task" ())
(declare-function my/tool-bar-toggle "bar" ())
(declare-function neofile-new-file-fast "neofile" ())
(declare-function neofile-new-file-with-type "neofile" ())
(declare-function imgdrop-insert-image "imgdrop" ())
(declare-function imgdrop-insert-image-gui "imgdrop" ())
(declare-function imgdrop-mode "imgdrop" (&optional arg))

(keymap-global-unset "C-M-<wheel-down>" t) ; mouse-wheel-global-text-scale
(keymap-global-unset "C-M-<wheel-up>" t)   ; mouse-wheel-global-text-scale
(keymap-global-unset "C-<wheel-down>" t)   ; mouse-wheel-text-scale
(keymap-global-unset "C-<wheel-up>" t)     ; mouse-wheel-text-scale
(keymap-global-unset "C-<mouse-5>")        ; mouse-wheel-text-scale down
(keymap-global-unset "C-<mouse-4>")        ; mouse-wheel-text-scale up
(keymap-global-unset "C-M-<mouse-5>")      ; mouse-wheel-global-text-scale down
(keymap-global-unset "C-M-<mouse-4>")      ; mouse-wheel-global-text-scale up

;; Secondary selection - too easy to trigger by accident.
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
(keymap-global-set "s-f" #'my/sar-find)            ; isearch, with m/sar.el's styles
(keymap-global-set "s-F" #'my/sar-panel-project)   ; the side panel, over the project
(keymap-global-set "s-g" #'isearch-repeat-forward)
(keymap-global-set "s-r" #'my/sar-replace)         ; the side panel, over this buffer
(keymap-global-set "s-R" #'my/sar-panel-replace)   ; the side panel, replacement focused
(keymap-global-set "s-t" #'neofile-new-file-fast)
(keymap-global-set "s-w" #'tab-close)
(keymap-global-set "s-W" #'delete-frame)
(keymap-global-set "s-q" #'save-buffers-kill-emacs)
(keymap-global-set "s-Q" #'restart-emacs)
(keymap-global-set "s-n" #'neofile-new-file-fast)
(keymap-global-set "s-N" #'neofile-new-file-with-type)
(keymap-global-set "s-m" #'iconify-frame)
(keymap-global-set "s-`" #'other-frame)
(keymap-global-set "C-s-f" #'toggle-frame-fullscreen)

;;;; Undo

(keymap-global-set "<remap> <undo>" #'undo-fu-only-undo)
(keymap-global-set "<remap> <undo-redo>" #'undo-fu-only-redo)

(dotimes (i 9)
  (keymap-global-set (format "s-%d" (1+ i)) #'my/tab-select))

(keymap-global-set "C-<tab>" #'my/tab-next)
(keymap-global-set "C-S-<tab>" #'my/tab-prev)

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

(put 'my/backward-delete-word 'delete-selection 'supersede)
(put 'my/forward-delete-word 'delete-selection 'supersede)

(keymap-global-set "M-<backspace>" #'my/backward-delete-word)
(keymap-global-set "C-<backspace>" #'my/backward-delete-word)
(keymap-global-set "M-d" #'my/forward-delete-word)
(keymap-global-set "C-<delete>" #'my/forward-delete-word)

(defun my/mark-line (&optional arg allow-extend)
  "Mark ARG whole lines, trailing newline included."
  (interactive "P\np")
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (use-region-p)))
      (let* ((backward (< (point) (mark)))
             (n (if arg (prefix-numeric-value arg) (if backward -1 1))))
        ;; Keep the anchor on a line boundary.
        (set-mark (save-excursion
                    (goto-char (mark))
                    (line-beginning-position (if backward 2 1))))
        (goto-char (line-beginning-position (1+ n))))
    (let ((n (prefix-numeric-value arg)))
      (push-mark (line-beginning-position (if (> n 0) 1 2)) nil t)
      (goto-char (line-beginning-position (if (> n 0) (1+ n) (+ 2 n)))))))

(defun my/keyboard-quit-dwim ()
  "DWIM `keyboard-quit': deactivate the region, close an unfocused minibuffer, close the Completions buffer, or fall back to `keyboard-quit'."
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

(define-prefix-command 'my-leader-map)

(keymap-global-set "<escape>" 'my-leader-map)

(keymap-set minibuffer-local-map "<escape>" #'abort-minibuffers)

;;;; Meta
(keymap-set my-leader-map "SPC" #'execute-extended-command)
(keymap-set my-leader-map "x" #'execute-extended-command)
(keymap-set my-leader-map "g" #'my/keyboard-quit-dwim)
(keymap-set my-leader-map "a" #'align-regexp)
(keymap-set my-leader-map "/" #'isearch-forward)
(keymap-set my-leader-map "L" #'list-packages)

;;;; Explorer (speedbar)
(keymap-set my-leader-map "e" #'my/speedbar-toggle)  ; open docked / close
(keymap-set my-leader-map "E" #'my/speedbar-reveal)  ; reveal file

;;;; Find / files
(keymap-set my-leader-map "f f" #'find-file)
(keymap-set my-leader-map "f r" #'recentf-open)
(keymap-set my-leader-map "f b" #'switch-to-buffer)
(keymap-set my-leader-map "f h" #'describe-symbol)
(keymap-set my-leader-map "f d" #'flymake-show-buffer-diagnostics)
(keymap-set my-leader-map "f n" #'neofile-new-file-fast)
(keymap-set my-leader-map "f t" #'neofile-new-file-with-type)  ; asks for a type

;;;; Images (m/p/imgdrop.el)
(keymap-set my-leader-map "i i" #'imgdrop-insert-image)      ; picker, with preview
(keymap-set my-leader-map "i g" #'imgdrop-insert-image-gui)  ; macOS file panel
(keymap-set my-leader-map "i d" #'imgdrop-mode)              ; arm/disarm dropping

;;;; Buffers
(keymap-set my-leader-map "b b" #'switch-to-buffer)
(keymap-set my-leader-map "b p" #'switch-to-buffer)
(keymap-set my-leader-map "b d" #'kill-current-buffer)
(keymap-set my-leader-map "b k k" #'kill-current-buffer)
(keymap-set my-leader-map "b k w" #'kill-buffer-and-window)

;;;; Projects  (see m/proj.el)
(keymap-set my-leader-map "p p" #'project-switch-project)
(keymap-set my-leader-map "p f" #'project-find-file)
(keymap-set my-leader-map "p g" #'project-find-regexp)
(keymap-set my-leader-map "p b" #'project-switch-to-buffer)
(keymap-set my-leader-map "p d" #'project-find-dir)
(keymap-set my-leader-map "p c" #'project-compile)
(keymap-set my-leader-map "p a" #'my/add-project)
(keymap-set my-leader-map "p r" #'my/delete-project)
(keymap-set my-leader-map "p s" #'my/project-discover)  ; scan
(keymap-set my-leader-map "p e" #'my/edit-projects)

;;;; Code
(keymap-set my-leader-map "c c" #'compile)
(keymap-set my-leader-map "c r" #'recompile)
(keymap-set my-leader-map "c f" #'my/format-dwim)
(keymap-set my-leader-map "c d" #'flymake-show-diagnostic)
(keymap-set my-leader-map "c a" #'xref-find-apropos)

;; Run/build per language (m/task.el).  `c c' and `c r' above stay what they
(keymap-set my-leader-map "c R" #'my/task-run)
(keymap-set my-leader-map "c b" #'my/task-build)
(keymap-set my-leader-map "c s" #'my/task-stop)
(keymap-set my-leader-map "c t" #'my/task-select)
(keymap-set my-leader-map "c n" #'my/task-add)      ; new command
(keymap-set my-leader-map "c e" #'my/task-env-select) ; m/py.el answers this

;;;; Replace / rename
(keymap-set my-leader-map "r r" #'replace-regexp)
(keymap-set my-leader-map "r s" #'replace-string)
(keymap-set my-leader-map "r n" #'xref-find-references)
;; m/sar.el's panel: p over the project.
(keymap-set my-leader-map "r p" #'my/sar-panel-project)
(keymap-set my-leader-map "r f" #'my/sar-panel-folder)
(keymap-set my-leader-map "r b" #'my/sar-panel-file)
(keymap-set my-leader-map "r t" #'my/sar-panel-toggle)

;;;; Mark and bookmarks
(keymap-set my-leader-map "m m" #'set-mark-command)
(keymap-set my-leader-map "m b" #'bookmark-set)
(keymap-set my-leader-map "m j" #'bookmark-jump)

;;;; Windows
(keymap-set my-leader-map "w d d" #'delete-window)
(keymap-set my-leader-map "w d o" #'delete-other-windows)
(keymap-set my-leader-map "w s" #'split-window-below)
(keymap-set my-leader-map "w v" #'split-window-right)
(keymap-set my-leader-map "w t" #'my/tool-bar-toggle)  ; m/bar.el

;;;; Shell
(keymap-set my-leader-map "1" #'shell-command)
(keymap-set my-leader-map "2" #'async-shell-command)
(keymap-set my-leader-map "3" #'my/open-curdir)  ; defined in m/mac.el

;;;; Terminal panel (m/term.el)
(keymap-set my-leader-map "t t" #'my/term-toggle)
(keymap-set my-leader-map "t n" #'my/term-new)
(keymap-set my-leader-map "t N" #'my/term-new-here)  ; no project question
(keymap-set my-leader-map "t k" #'my/term-close)
(keymap-set my-leader-map "t r" #'my/term-rename)
(keymap-set my-leader-map "t s" #'my/term-select)
(keymap-set my-leader-map "t l" #'my/term-focus-list)
(keymap-set my-leader-map "t ]" #'my/term-next)
(keymap-set my-leader-map "t [" #'my/term-prev)
(keymap-set my-leader-map "t b" #'my/term-set-backend)
(keymap-set my-leader-map "t d" #'my/term-cd)

(keymap-global-set "C-`" #'my/term-toggle)

;;;; Homebrew (macOS)
(keymap-set my-leader-map "o h" #'homebrew-dispatch)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements my-leader-map
    "b" "buffer"
    "c" "code"
    "f" "find/files"
    "i" "image"
    "m" "mark/bookmark"
    "o" "os"
    "o h" "Homebrew menu"
    "p" "project"
    "r" "replace"
    "t" "terminal"
    "w" "window"))

;;; k.el ends here
