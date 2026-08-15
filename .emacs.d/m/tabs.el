;;; tabs.el --- One tab per buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; tab-bar as a buffer bar (browser-style), not window-config tabs.

;;; Code:

;;;; The bar

(use-package tab-bar :ensure nil
  :demand t
  :custom
  (tab-bar-define-keys nil)
  (tab-bar-select-tab-modifiers '())

  (tab-bar-show t)                ; always on, never appearing and vanishing
  (tab-bar-tab-hints t)           ; number the tabs, giving s-1 ... s-9 targets
  (tab-bar-new-tab-to 'rightmost) ; append, so those numbers stay put
  ;; No "+": a new tab would clone the current buffer.
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show t)
  ;; `my/tab-bar-format-rows' is `tab-bar-format-tabs' with the row breaks put in - see Rows below.
  (tab-bar-format '(my/tab-bar-format-rows))
  :config
  (tab-bar-mode 1)

  ;; `tab-bar-map' binds <mouse-2> to `tab-bar-mouse-close-tab'.
  (keymap-unset tab-bar-map "<mouse-2>" t))

;;;; Appearance

(declare-function my/theme-shade "theme" (percent))
(declare-function icon-string "icons" (name))
(eval-when-compile (require 'icons))
(declare-function icons--register "icons")

(defvar tab-bar-button-margin)


(defface my/tab-bar-close '((t :inherit shadow))
  "Face the tab bar's close button is drawn in.
Colour only -- the size is pixels, set by `my/tab-bar--refresh-close-button'."
  :group 'tab-bar-faces)

(defun my/tab-bar--close-button-height ()
  "Pixel height for the close button, from the height of a line in the bar."
  (max 8 (- (frame-char-height) (* 2 (or tab-bar-button-margin 1)))))

(defun my/tab-bar--refresh-close-button (&rest _)
  "Re-cut the close button at `my/tab-bar--close-button-height' pixels."
  (require 'icons)
  (define-icon tab-bar-close nil
    `((image "symbols/cross_16.svg" "tabs/close.xpm"
             :face my/tab-bar-close
             :height ,(my/tab-bar--close-button-height)
             :margin ,tab-bar-button-margin
             :ascent center)
      (text " x"))
    "Icon for closing the clicked tab, at a fixed pixel height."
    :version "29.1"
    :help-echo "Click to close tab")
  (setq tab-bar-close-button
        (propertize (icon-string 'tab-bar-close) 'close-tab t)))

(defconst my/tab-bar--shades '(8 14 20)
  "How far the tabs, the strip, and a hovered tab sit off the page.")

(defun my/tab-bar--refresh-faces (&rest _)
  "Derive the tab bar's colours and family from the theme now in force."
  (let* ((background (face-attribute 'default :background nil t))
         (foreground (face-attribute 'default :foreground nil t))
         (family (or (face-attribute 'variable-pitch :family nil t)
                     'unspecified))
         (raised '(:line-width 1 :style released-button))
         (tab (my/theme-shade (nth 0 my/tab-bar--shades)))
         (strip (my/theme-shade (nth 1 my/tab-bar--shades)))
         (hover (my/theme-shade (nth 2 my/tab-bar--shades))))
    ;; The strip the tabs sit in.
    (set-face-attribute 'tab-bar nil
                        :background strip :foreground foreground
                        :family family :height 1.0 :weight 'normal
                        :box nil :underline nil :overline nil
                        :inverse-video nil :inherit 'unspecified)
    ;; The selected tab is the page, continued upwards.
    (set-face-attribute 'tab-bar-tab nil
                        :inherit 'tab-bar :inverse-video nil
                        :background background :foreground foreground
                        :weight 'normal :underline nil :box raised)
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :inherit 'tab-bar :inverse-video nil
                        :background tab
                        :foreground (face-attribute 'shadow :foreground nil t)
                        :weight 'normal :underline nil :box raised)
    (set-face-attribute 'tab-bar-tab-highlight nil
                        :inherit 'tab-bar :inverse-video nil
                        :background hover :foreground foreground
                        :weight 'normal :underline nil :box raised)
    ;; Sized in pixels off the character cell, so it takes no argument and cares about none of the.
    (my/tab-bar--refresh-close-button))
  ;; The family just changed, so every width measured under the old one is wrong -- see below.
  (my/tab--remeasure))

;;;; Width


(defvar my/tab-width '((150) 18)
  "How wide every tab is, in the shape `tab-bar-auto-width-max' documents: a pixel width for graphical frames -- in a.")

(setopt auto-resize-tab-bars t
        tab-bar-auto-width t
        ;; The same value twice is the whole trick -- see above.
        tab-bar-auto-width-min my/tab-width
        tab-bar-auto-width-max my/tab-width)

;;;; Rows


(defun my/tab-bar--width ()
  "The width every tab is pinned to, in the units this frame measures in."
  (if (window-system)
      (tab-bar-auto-width-1 (nth 0 my/tab-width))
    (nth 1 my/tab-width)))

(defun my/tab-bar--tabs-per-row ()
  "How many tabs fit across the frame.  Never less than one."
  (max 1 (floor (frame-inner-width)
                (+ (my/tab-bar--width)
                   (string-pixel-width
                    (propertize (tab-bar-separator) 'face 'tab-bar))))))

(defun my/tab-bar--row-end (i)
  "An item running the row out to the frame edge, after tab I."
  (list (intern (format "row-%i" i)) 'menu-item
        (propertize " " 'display
                    ;; `right' is the tab bar's own right edge, and the one `tab-bar-format-align-right' reaches for.
                    (if (window-system)
                        '(space :align-to (- right (1)))
                      `(space :align-to (,(1- (frame-inner-width))))))
        'ignore))

(defun my/tab-bar-format-rows ()
  "Produce all the tabs, in rows that break between one tab and the next."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (last (length tabs))
         (per-row (my/tab-bar--tabs-per-row))
         (i 0))
    (mapcan
     (lambda (tab)
       (setq i (1+ i))
       (append (tab-bar--format-tab tab i)
               ;; After the tab that fills a row, and after the last tab whether it fills one or not.
               (when (or (zerop (% i per-row)) (= i last))
                 (list (my/tab-bar--row-end i)))))
     tabs)))

;;;; Width, across a theme change


(defvar tab-bar--auto-width-hash)

(defun my/tab--remeasure (&rest _)
  "Forget the tab widths measured under the theme being replaced."
  (setq tab-bar--auto-width-hash nil)
  (force-mode-line-update t))

(add-hook 'disable-theme-functions #'my/tab--remeasure)

(my/tab-bar--refresh-faces)
(add-hook 'enable-theme-functions #'my/tab-bar--refresh-faces)

;;;; Which buffers earn a tab

(defvar my/tab-exempt-regexps
  '("\\`[[:space:]]"                    ; " SPEEDBAR", " *which-key*", " *corfu*"
    "\\`\\*Completions\\*\\'"
    "\\`\\*fontdue\\*\\'")                ; a download log, not a document
  "Buffer-name regexps that never get a tab of their own.")

(defun my/tab--side-window-p (action)
  "Non-nil when ACTION asks for a side window or a child frame."
  (when (consp action)
    (let ((fns (car action)))
      (or (assq 'side (cdr action))
          (seq-intersection (if (listp fns) fns (list fns))
                            '(display-buffer-in-side-window
                              display-buffer-in-child-frame
                              display-buffer-no-window))))))

(defun my/tab-worthy-p (buffer &optional action)
  "Non-nil when BUFFER should be shown in a tab of its own."
  ;; BUFFER may be a name whose buffer is already dead.
  (when-let* ((buffer (get-buffer buffer))
              (name (buffer-name buffer)))
    (and (not (my/tab--side-window-p action))
         (not (seq-some (lambda (re) (string-match-p re name))
                        my/tab-exempt-regexps)))))

;;;; Naming


(defun my/tab-name-current ()
  "Name the current tab after the tab-worthy buffer it holds."
  (buffer-name (or (seq-find #'my/tab-worthy-p
                             (mapcar #'window-buffer
                                     (window-list nil 'no-minibuf)))
                   (current-buffer))))

(setq tab-bar-tab-name-function #'my/tab-name-current)

(defun my/tab-documents (tab)
  "Return the names of the tab-worthy buffers TAB displays."
  (let ((buffers (if (eq (car tab) 'current-tab)
                     (mapcar #'window-buffer (window-list nil 'no-minibuf))
                   (when-let* ((ws (alist-get 'ws tab)))
                     (window-state-buffers ws)))))
    (delete-dups
     (delq nil (mapcar (lambda (b)
                         (and (my/tab-worthy-p b)
                              (buffer-name (get-buffer b))))
                       buffers)))))

;;;; Routing

(setq switch-to-buffer-obey-display-actions t)

(defun my/tab-dired-step-p (buffer &optional _action)
  "Non-nil when BUFFER is a directory listing opened from another one."
  (and (derived-mode-p 'dired-mode)
       (when-let* ((buffer (get-buffer buffer)))
         (and (with-current-buffer buffer (derived-mode-p 'dired-mode))
              ;; A listing that already has a tab is somewhere you are going back to.
              (not (tab-bar-get-buffer-tab buffer))))))

(defvar my/tab-dired-single-buffer t
  "When non-nil, walking Dired retires the listing it steps out of.")

(defvar dired-kill-when-opening-new-dired-buffer)
(setq dired-kill-when-opening-new-dired-buffer nil)

(defun my/tab-dired-step (buffer alist)
  "Show BUFFER in the selected window and retire the listing it replaces."
  (let* ((previous (current-buffer))
         (buffer (get-buffer buffer))
         (window (or (display-buffer-same-window buffer alist)
                     (display-buffer-in-tab buffer alist))))
    (when (and window
               my/tab-dired-single-buffer
               (not (eq previous buffer))
               (buffer-live-p previous)
               (with-current-buffer previous (derived-mode-p 'dired-mode))
               (not (get-buffer-window previous t))
               (not (tab-bar-get-buffer-tab previous)))
      (kill-buffer previous))
    window))

(add-to-list 'display-buffer-alist
             ;; `inhibit-same-window' is why this is a rule rather than just an exemption from the catch-all.
             '(my/tab-dired-step-p
               (my/tab-dired-step)
               (inhibit-same-window . nil)
               (reusable-frames . selected))
             t)

(add-to-list 'display-buffer-alist
             '(my/tab-worthy-p
               (display-buffer-in-tab)
               ;; No `tab-name' - see Naming above. With none.
               (reusable-frames . selected))
             t)

;;;; Closing, from either end


(defvar my/tab--closing nil
  "Non-nil while a tab and its buffer are being closed together.")

(defun my/tab--shown-elsewhere-p (buffer tab)
  "Non-nil when a tab other than TAB also displays BUFFER."
  (let ((index (tab-bar--tab-index tab)))
    (seq-some (lambda (other) (not (eql (alist-get 'index other) index)))
              (tab-bar-get-buffer-tab buffer nil nil t))))

(defun my/tab-close-for-buffer ()
  "Close every tab the buffer being killed had to itself."
  (when (and tab-bar-mode (not my/tab--closing))
    (let ((name (buffer-name))
          (my/tab--closing t)
          (tabs (sort (tab-bar-get-buffer-tab (current-buffer) nil nil t)
                      (lambda (a b) (> (alist-get 'index a)
                                       (alist-get 'index b))))))
      (dolist (tab tabs)
        (when (and (equal (my/tab-documents tab) (list name))
                   (> (length (funcall tab-bar-tabs-function)) 1))
          (tab-bar-close-tab (1+ (alist-get 'index tab))))))))

(defun my/tab--kill-buffer-asking (buffer)
  "Kill BUFFER, asking what to do with unsaved changes."
  (if (not (and (buffer-modified-p buffer) (buffer-file-name buffer)))
      (kill-buffer buffer)
    (pcase (car (read-multiple-choice
                 (format "Buffer %s has unsaved changes"
                         (buffer-name buffer))
                 '((?s "save and close" "Save the buffer, then close it")
                   (?d "discard and close" "Close it, losing the changes")
                   (?c "cancel" "Leave the buffer, and its tab, open"))))
      (?s (with-current-buffer buffer (save-buffer)) (kill-buffer buffer))
      (?d (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
      (_ nil))))

(defun my/tab-kill-buffer-for-tab (tab last-tab-p)
  "Kill the buffers TAB displays, so the close button closes the file."
  (unless (or my/tab--closing last-tab-p)
    (let ((my/tab--closing t)
          (refused nil))
      (dolist (name (my/tab-documents tab))
        (when-let* ((buffer (get-buffer name))
                    ((not (my/tab--shown-elsewhere-p buffer tab))))
          (unless (my/tab--kill-buffer-asking buffer)
            (setq refused t))))
      refused)))

(add-hook 'kill-buffer-hook #'my/tab-close-for-buffer)
(add-hook 'tab-bar-tab-prevent-close-functions #'my/tab-kill-buffer-for-tab)

;;;; Duplicates


(defvar my/tab--sweep-timer nil
  "Timer for the sweep that is due, or nil when none is.")

(defun my/tab-sweep-duplicates ()
  "Retire every tab showing nothing a tab before it is not already showing."
  (interactive)
  (setq my/tab--sweep-timer nil)
  (when (and tab-bar-mode (not my/tab--closing))
    (let ((seen (make-hash-table :test #'equal))
          (index 0)
          (doomed nil))
      (dolist (tab (funcall tab-bar-tabs-function))
        (let ((documents (my/tab-documents tab)))
          (if (and documents
                   (seq-every-p (lambda (name) (gethash name seen)) documents))
              (push index doomed)
            (dolist (name documents) (puthash name t seen))))
        (setq index (1+ index)))
      (when doomed
        (let ((my/tab--closing t))
          (dolist (i doomed)
            (when (> (length (funcall tab-bar-tabs-function)) 1)
              (tab-bar-close-tab (1+ i)))))))))

(defun my/tab--sweep-later (&rest _)
  "Ask for a sweep once redisplay has finished with the frame."
  (unless (or my/tab--closing my/tab--sweep-timer)
    (setq my/tab--sweep-timer
          (run-with-idle-timer 0 nil #'my/tab-sweep-duplicates))))

;; A window changing buffer is how a duplicate appears without a tab being
(add-hook 'window-buffer-change-functions #'my/tab--sweep-later)
(add-hook 'tab-bar-tab-post-open-functions #'my/tab--sweep-later)

;;;; Quitting


(defvar my/tab-transient-modes '(special-mode compilation-mode)
  "Major modes whose buffers `q' dismisses outright, tab and all. Read-only views that are cheap to regenerate -- Help, apropos, the message log, compilation output. These are the buffers that bind `q' in the first place, and a tab per dismissed one would pile up fast.")

(defun my/tab-transient-p (buffer)
  "Non-nil when BUFFER is a view to dismiss rather than a place to be in."
  (with-current-buffer buffer
    (and (derived-mode-p my/tab-transient-modes)
         (not (derived-mode-p 'dired-mode))
         (not buffer-file-name))))

(defun my/tab--document-window-p (window)
  "Non-nil when WINDOW is the one window in its tab holding a document."
  (and (my/tab-worthy-p (window-buffer window))
       (= 1 (seq-count (lambda (w) (my/tab-worthy-p (window-buffer w)))
                       (window-list (window-frame window) 'no-minibuf)))))

(defun my/tab--bury-does-nothing (fn &optional window bury-or-kill)
  "Drop FN on the floor when it would retire a tab just to bury a buffer."
  (let ((window (or window (selected-window))))
    (cond
     ((not (and tab-bar-mode
                (not (eq bury-or-kill 'killing))
                (my/tab--document-window-p window)))
      (funcall fn window bury-or-kill))
     ((or (eq bury-or-kill 'kill)
          (my/tab-transient-p (window-buffer window)))
      (kill-buffer (window-buffer window))))))

(advice-add 'quit-restore-window :around #'my/tab--bury-does-nothing)

;;;; Commands


(defun my/tab-next (&optional n)
  "Select the Nth next tab, wrapping around at the end.  N defaults to 1."
  (interactive "p")
  (tab-next (or n 1)))

(defun my/tab-prev (&optional n)
  "Select the Nth previous tab, wrapping around at the start.  N defaults to 1."
  (interactive "p")
  (tab-previous (or n 1)))

(defun my/tab-select ()
  "Select the tab named by the digit in the key that invoked this command."
  (interactive)
  (let* ((digit (- (event-basic-type last-command-event) ?0))
         (count (length (funcall tab-bar-tabs-function)))
         (n (if (= digit 9) count digit)))
    (when (and (> n 0) (<= n count))
      (tab-bar-select-tab n))))

;;; tabs.el ends here
