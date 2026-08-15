;;; modeline.el --- Mode line -*- lexical-binding: t; -*-

;;; Commentary:
;; mode line.

;;; Code:

(require 'subr-x)

(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

;;;; Faces

(defface my/modeline-normal-state '((t :inherit font-lock-keyword-face :weight bold))
  "Evil normal state indicator." :group 'mode-line)
(defface my/modeline-insert-state '((t :inherit font-lock-string-face :weight bold))
  "Evil insert state indicator." :group 'mode-line)
(defface my/modeline-visual-state '((t :inherit font-lock-constant-face :weight bold))
  "Evil visual state indicator." :group 'mode-line)
(defface my/modeline-replace-state '((t :inherit font-lock-warning-face :weight bold))
  "Evil replace state indicator." :group 'mode-line)
(defface my/modeline-operator-state '((t :inherit font-lock-builtin-face :weight bold))
  "Evil operator-pending state indicator." :group 'mode-line)
(defface my/modeline-emacs-state '((t :inherit font-lock-preprocessor-face :weight bold))
  "Emacs state indicator." :group 'mode-line)

(defface my/modeline-diff-added '((t :inherit diff-refine-added))
  "Added-line counter." :group 'mode-line)
(defface my/modeline-diff-changed '((t :inherit diff-refine-changed))
  "Changed-line counter." :group 'mode-line)
(defface my/modeline-diff-removed '((t :inherit diff-refine-removed))
  "Removed-line counter." :group 'mode-line)

;;;; Section a: Evil state

(defconst my/modeline-state-alist
  '((normal   "NORMAL"   my/modeline-normal-state)
    (insert   "INSERT"   my/modeline-insert-state)
    (visual   "VISUAL"   my/modeline-visual-state)
    (replace  "REPLACE"  my/modeline-replace-state)
    (operator "OP-PEND"  my/modeline-operator-state)
    (motion   "MOTION"   my/modeline-normal-state)
    (emacs    "EMACS"    my/modeline-emacs-state)))

(defun my/modeline-state ()
  "Return the editing-state indicator."
  (let* ((state (if (bound-and-true-p evil-local-mode)
                    (bound-and-true-p evil-state)
                  'emacs))
         (entry (or (assq state my/modeline-state-alist)
                    (assq 'emacs my/modeline-state-alist))))
    (propertize (format " %-7s " (nth 1 entry)) 'face (nth 2 entry))))

;;;; Section b: branch, diff, diagnostics

(defun my/modeline-shorten-vc (vc)
  "Shorten VC to at most 20 characters, `Git-' replaced by a branch glyph."
  (let ((vc (replace-regexp-in-string
             ;; U+E0A0 as an escape rather than a literal: private-use glyphs do not survive every editor and.
             "^ Git[:-]" (if (char-displayable-p ?\N{U+E0A0})
                             " \N{U+E0A0} " " Git: ")
             vc)))
    (if (> (length vc) 20)
        (concat (substring vc 0 20) (if (char-displayable-p ?...) "..." "..."))
      vc)))

(defvar-local my/modeline--diff nil
  "Cached (ADDED CHANGED REMOVED) for this buffer, or nil.")

(defvar-local my/modeline--diff-process nil
  "Running `git diff' process for this buffer, if any.")

(defun my/modeline--parse-hunks (output)
  "Count added, changed and removed lines from the hunk headers in OUTPUT."
  (let ((added 0) (changed 0) (removed 0) (pos 0))
    (while (string-match
            "^@@ -[0-9]+\\(?:,\\([0-9]+\\)\\)? \\+[0-9]+\\(?:,\\([0-9]+\\)\\)? @@"
            output pos)
      (setq pos (match-end 0))
      (let ((old (if (match-string 1 output)
                     (string-to-number (match-string 1 output)) 1))
            (new (if (match-string 2 output)
                     (string-to-number (match-string 2 output)) 1)))
        (setq changed (+ changed (min old new))
              added   (+ added (max 0 (- new old)))
              removed (+ removed (max 0 (- old new))))))
    (list added changed removed)))

(defun my/modeline-update-diff (&rest _)
  "Refresh this buffer's git counters, asynchronously."
  (when-let* ((file buffer-file-name)
              ((not my/modeline--diff-process))
              ((executable-find "git"))
              ((file-exists-p file)))
    (let* ((target (current-buffer))
           (out (generate-new-buffer " *my-modeline-diff*"))
           (default-directory (file-name-directory file)))
      (setq my/modeline--diff-process
            (make-process
             :name "my-modeline-diff"
             :buffer out
             :noquery t
             :connection-type 'pipe
             :command (list "git" "--no-pager" "diff" "-U0" "--no-color"
                            "--" (file-name-nondirectory file))
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (let ((text (with-current-buffer (process-buffer proc)
                               (buffer-string))))
                   (kill-buffer (process-buffer proc))
                   (when (buffer-live-p target)
                     (with-current-buffer target
                       (setq my/modeline--diff-process nil)
                       (setq my/modeline--diff
                             (and (= (process-exit-status proc) 0)
                                  (not (string-empty-p text))
                                  (my/modeline--parse-hunks text)))
                       (force-mode-line-update)))))))))))

(defun my/modeline-diff ()
  "Return the cached `+a ~c -r' counters, omitting the zero ones."
  (when-let* ((d my/modeline--diff))
    (let ((parts (delq nil
                       (list (unless (zerop (nth 0 d))
                               (propertize (format "+%d" (nth 0 d))
                                           'face 'my/modeline-diff-added))
                             (unless (zerop (nth 1 d))
                               (propertize (format "~%d" (nth 1 d))
                                           'face 'my/modeline-diff-changed))
                             (unless (zerop (nth 2 d))
                               (propertize (format "-%d" (nth 2 d))
                                           'face 'my/modeline-diff-removed))))))
      (when parts (concat "  " (string-join parts " "))))))

(defun my/modeline--severity (diag)
  "Return `error', `warning' or `note' for DIAG."
  (let* ((type (flymake-diagnostic-type diag))
         (type (or (get type 'flymake-category) type)))
    (cond ((memq type '(:error error flymake-error)) 'error)
          ((memq type '(:warning warning flymake-warning)) 'warning)
          (t 'note))))

(defun my/modeline-diagnostics ()
  "Return Flymake counters, styled like lualine's `diagnostics' section."
  (when (bound-and-true-p flymake-mode)
    (let ((err 0) (warn 0) (note 0))
      (dolist (d (flymake-diagnostics))
        (pcase (my/modeline--severity d)
          ('error (setq err (1+ err)))
          ('warning (setq warn (1+ warn)))
          (_ (setq note (1+ note)))))
      (let ((parts (delq nil
                         (list (unless (zerop err)
                                 (propertize (format "%d" err)
                                             'face 'error))
                               (unless (zerop warn)
                                 (propertize (format "%d" warn)
                                             'face 'warning))
                               (unless (zerop note)
                                 (propertize (format "%d" note)
                                             'face 'success))))))
        (when parts (concat "  " (string-join parts " ")))))))

;;;; Section c: modes, both kinds behind a button

(defconst my/modeline-major-mode-lighter
  (if (char-displayable-p ?☰) " ☰" " M")
  "Lighter standing in for the major mode name.")

(defvar my/modeline-major-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'my/modeline-major-mode-menu)
    (define-key map [mode-line mouse-2] #'describe-mode)
    (define-key map [mode-line down-mouse-3]
                `(menu-item "Minor Modes" ,mode-line-mode-menu
                            :filter bindings--sort-menu-keymap))
    map)
  "Keymap of the collapsed major-mode button.")

(defun my/modeline--mode-name ()
  "Return the major mode's name as plain text."
  (string-trim (substring-no-properties (format-mode-line mode-name))))

(defun my/modeline-major-mode-menu (event)
  "Pop up a menu for the current major mode at EVENT."
  (interactive "@e")
  (let* ((name (my/modeline--mode-name))
         (own (and (current-local-map)
                   (local-key-binding [menu-bar])
                   (mouse-menu-major-mode-map)))
         (menu (make-sparse-keymap (format "%s mode" name))))
    ;; Later definitions come first in a sparse keymap.
    (define-key menu [my/modeline-describe-mode]
                (list 'menu-item "Help for major mode"
                      (lambda () (interactive) (describe-mode))))
    (when own
      (define-key menu [my/modeline-mode-menu]
                  (list 'menu-item (format "%s menu" name) own)))
    (popup-menu menu event)))

(defun my/modeline--major-mode-help (window _object _pos)
  "Tooltip for the collapsed major-mode button in WINDOW."
  (with-current-buffer (if (window-live-p window)
                           (window-buffer window)
                         (current-buffer))
    (format "%s (major mode)\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
            (my/modeline--mode-name))))

(defun my/modeline-major-mode ()
  "Return the button that stands in for the major mode name."
  (propertize my/modeline-major-mode-lighter
              'mouse-face 'mode-line-highlight
              'help-echo #'my/modeline--major-mode-help
              'local-map my/modeline-major-mode-keymap))

(defvar my/modeline-modes
  (list '(:eval (my/modeline-major-mode))
        '("" mode-line-process)
        (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                    'mouse-face 'mode-line-highlight
                    'local-map (make-mode-line-mouse-map
                                'mouse-2 #'mode-line-widen))
        '("" mode-line-minor-modes))
  "`mode-line-modes' with the major mode collapsed into a button. Everything else that construct carries -- process.")
(put 'my/modeline-modes 'risky-local-variable t)

;;;; Section d: file name, project-relative

(defvar-local my/modeline--name nil
  "Cached project-relative name for this buffer.")

(defun my/modeline-update-name (&rest _)
  "Recompute the cached buffer name."
  (setq my/modeline--name
        (or (when-let* ((file buffer-file-name)
                        (proj (project-current nil)))
              (file-relative-name file (project-root proj)))
            (buffer-name))))

(defun my/modeline-name ()
  "Return the buffer's name, relative to its project when it has one."
  (concat " " (or my/modeline--name (buffer-name))))

;;;; Assembly

(setq-default
 mode-line-format
 '("%e"
   (:eval (my/modeline-state))
   (:propertize ("" mode-line-mule-info mode-line-client
                 mode-line-modified mode-line-remote))
   mode-line-frame-identification
   (vc-mode (:eval (my/modeline-shorten-vc vc-mode)))
   (:eval (my/modeline-diff))
   (:eval (my/modeline-diagnostics))
   "  "
   (:eval (my/modeline-name))
   mode-line-format-right-align
   "  "
   my/modeline-modes
   "  "
   mode-line-percent-position
   " "
   (:eval (propertize " %l:%c " 'face 'my/modeline-normal-state))))

(setq mode-line-modes-delimiters '("" . ""))

;;;; EMACS-31: every minor mode behind the button
(setq mode-line-collapse-minor-modes t)

(add-hook 'find-file-hook #'my/modeline-update-diff)
(add-hook 'after-save-hook #'my/modeline-update-diff)
(add-hook 'find-file-hook #'my/modeline-update-name)
(add-hook 'after-save-hook #'my/modeline-update-name)

;;; modeline.el ends here
