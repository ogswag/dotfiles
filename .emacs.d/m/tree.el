;;; tree.el --- File tree sidebar (speedbar) -*- lexical-binding: t; -*-

;;; Commentary:
;; speedbar sidebar, one for all tabs. attach or float.

;;; Code:

;; m/p is not on `load-path' by default.
(eval-and-compile
  (add-to-list 'load-path
               (or (bound-and-true-p my-vendor-directory)
                   (expand-file-name "m/p" user-emacs-directory))))
(require 'sidepanel)

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function speedbar-frame-mode "speedbar" (&optional arg))
(declare-function speedbar-frame-or-window "speedbar" ())
(declare-function speedbar-update-contents "speedbar" ())
(declare-function speedbar-expand-line "speedbar" (&optional arg))
(declare-function speedbar-goto-this-file "speedbar" (file))
(declare-function speedbar-find-selected-file "speedbar" (file))
(declare-function speedbar-position-cursor-on-line "speedbar" ())
(declare-function speedbar-mode "speedbar" ())
(declare-function speedbar-reconfigure-keymaps "speedbar" ())
(declare-function speedbar-set-timer "speedbar" (timeout))
(declare-function nerd-icons-devicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-faicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-octicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))

(defvar speedbar-buffer)
(defvar speedbar-frame)
(defvar speedbar-cached-frame)
(defvar speedbar--window)
(defvar speedbar--window-width)
(defvar speedbar-window-side)
(defvar speedbar-window-default-width)
(defvar speedbar-window-dedicated-window)
(defvar speedbar-shown-directories)
(defvar speedbar-full-text-cache)
(defvar speedbar-last-selected-file)
(defvar speedbar-indicator-regex)
(defvar speedbar-select-frame-method)
(defvar speedbar-vc-indicator)
(defvar speedbar-obj-indicator)
(defvar speedbar-object-read-only-indicator)
(defvar speedbar-indicator-separator)
(defvar speedbar-indicator-regex)
(defvar speedbar-expand-image-button-alist)
(defvar dframe-attached-frame)
(defvar dframe-update-speed)

;;;; State

(defvar my/speedbar-width 32
  "Columns for the speedbar, attached (side window) or detached (frame).")

(defvar my/speedbar-kind 'frame
  "Last presentation kind: `frame' (detached) or `window' (attached/docked).")

(defun my/speedbar-docked-p ()
  "Non-nil when a docked speedbar is wanted, whether or not it is on screen.

The flag lives in the `speedbar' panel now -- `sidepanel-show' and
`sidepanel-hide' are what set and clear it, and the tab guard is what reads it."
  (sidepanel-wanted-p 'speedbar))

(defvar my/speedbar-glue t
  "Non-nil means a detached speedbar frame tracks its attached frame.")

(defvar my/speedbar-glue-gap 0
  "Pixel gap between the main frame and a glued speedbar frame.")

(defvar my/speedbar-glue-side 'left
  "Preferred side for a glued speedbar frame: `left' or `right'.")

(defvar my/speedbar-attached-frame nil
  "Main frame the speedbar frame is glued to.")

(defvar my/speedbar--tab-guard nil
  "Non-nil while suppressing recursive tab/speedbar work.")

(defvar my/speedbar--glue-guard nil
  "Non-nil while we are moving the speedbar frame ourselves.")

(defvar my/speedbar--restore-timer nil
  "Timer handle for deferred restore after tab changes.")

(defvar my/speedbar--pre-fs-kind nil
  "When non-nil, speedbar kind to restore after leaving maximized/fullscreen.")

;;;; Live display queries

(defun my/speedbar--is-speedbar-frame-p (&optional frame)
  "Non-nil when FRAME is a detached speedbar frame."
  (let ((frame (or frame (selected-frame))))
    (and (frame-live-p frame)
         (frame-parameter frame 'my/speedbar-frame)
         t)))

(defun my/speedbar--attached-frame ()
  "Return the live main frame speedbar should follow, or nil."
  (let ((parent (or my/speedbar-attached-frame
                    (and (buffer-live-p speedbar-buffer)
                         (buffer-local-value 'dframe-attached-frame
                                             speedbar-buffer)))))
    (and parent (frame-live-p parent) parent)))

(defun my/speedbar--main-frame ()
  "Frame that owns the document windows (never a detached speedbar frame)."
  (or (my/speedbar--attached-frame)
      (let ((sel (selected-frame)))
        (if (my/speedbar--is-speedbar-frame-p sel)
            (or (car (filtered-frame-list
                      (lambda (fr)
                        (and (frame-live-p fr)
                             (not (my/speedbar--is-speedbar-frame-p fr))))))
                sel)
          sel))))

(defun my/speedbar--speedbar-frames ()
  "All live detached speedbar frames, including orphans from failed toggles."
  (let ((main (my/speedbar--main-frame))
        found)
    (dolist (fr (frame-list))
      (when (and (frame-live-p fr)
                 (or (my/speedbar--is-speedbar-frame-p fr)
                     (and (buffer-live-p speedbar-buffer)
                          (not (eq fr main))
                          (when-let* ((win (get-buffer-window speedbar-buffer
                                                              fr)))
                            (not (window-parameter win 'window-side))))))
        (push fr found)))
    (nreverse found)))

(defun my/speedbar--drop-cached-frame (&optional keep)
  "Clear `speedbar-cached-frame', deleting the frame only when it is ours."
  (when (and (boundp 'speedbar-cached-frame) speedbar-cached-frame)
    (when (and (frame-live-p speedbar-cached-frame)
               (not (eq speedbar-cached-frame keep))
               (my/speedbar--is-speedbar-frame-p speedbar-cached-frame))
      (delete-frame speedbar-cached-frame))
    ;; Unconditionally: a stale cache is what poisons the next pass.
    (setq speedbar-cached-frame nil)))

(defun my/speedbar--reconcile-frame ()
  "Return the one detached speedbar frame, deleting orphans."
  (let* ((frames (my/speedbar--speedbar-frames))
         (keep (car frames)))
    (dolist (fr (cdr frames))
      (when (frame-live-p fr)
        (delete-frame fr)))
    (unless (and (null keep) (my/speedbar-docked-p))
      (setq speedbar-frame keep))
    ;; Hidden cache left by dframe; drop it so we never resurrect a twin.
    (my/speedbar--drop-cached-frame keep)
    keep))

(defun my/speedbar--frame-live-p ()
  "Non-nil when a detached speedbar frame is showing."
  (and (my/speedbar--reconcile-frame) t))

(defun my/speedbar--reconcile-window (&optional frame)
  "Point `speedbar--window' at a live speedbar side window, or nil.

The window itself is `m/p/sidepanel.el's business; what is left here is keeping
speedbar's own `speedbar--window' in step with it, which `speedbar-window-p'
and friends read."
  (sidepanel-reconcile frame)
  (setq speedbar--window (sidepanel-window 'speedbar)))

(defun my/speedbar--current ()
  "Return `frame', `window', or nil for what is actually showing."
  (cond ((my/speedbar--frame-live-p) 'frame)
        ((my/speedbar--reconcile-window) 'window)
        (t nil)))

;;;; Glue

(defun my/speedbar--reposition (sb parent)
  "Place SB flush against PARENT.  Height only; never rewrite width."
  (let* ((gap (or my/speedbar-glue-gap 0))
         (pos (frame-position parent))
         (px (car pos))
         (py (cdr pos))
         (pw (frame-pixel-width parent))
         (ph (frame-pixel-height parent))
         (sw (frame-pixel-width sb))
         (disp-w (display-pixel-width (frame-parameter parent 'display)))
         (left-x (- px gap sw))
         (right-x (+ px pw gap))
         (left-ok (>= left-x 0))
         (right-ok (<= (+ right-x sw) disp-w))
         (prefer (if (eq my/speedbar-glue-side 'right) 'right 'left))
         (side (cond
                ((eq prefer 'left) (if left-ok 'left (if right-ok 'right 'left)))
                (t (if right-ok 'right (if left-ok 'left 'right)))))
         (nx (if (eq side 'left)
                 (max 0 left-x)
               (min right-x (max 0 (- disp-w sw)))))
         (ny (max 0 py)))
    (set-frame-height sb ph nil t)
    (set-frame-position sb nx ny)))

(defun my/speedbar--follow-frame (frame)
  "Reposition the speedbar frame beside FRAME when soft-glue is on."
  (when (and my/speedbar-glue
             (not my/speedbar--glue-guard)
             (frame-live-p frame)
             (not (eq frame (my/speedbar--reconcile-frame)))
             (my/speedbar--frame-live-p)
             (or (eq frame (my/speedbar--attached-frame))
                 (eq frame (my/speedbar--main-frame))))
    (let ((my/speedbar--glue-guard t)
          (sel (selected-frame))
          (sb speedbar-frame))
      (setq my/speedbar-attached-frame frame)
      (unwind-protect
          (my/speedbar--reposition sb frame)
        (when (frame-live-p sel)
          (select-frame sel))))))

(defun my/speedbar-toggle-glue ()
  "Toggle soft-glue of a detached speedbar frame."
  (interactive)
  (setq my/speedbar-glue (not my/speedbar-glue))
  (when (and my/speedbar-glue (my/speedbar--frame-live-p))
    (when-let* ((parent (my/speedbar--attached-frame)))
      (my/speedbar--follow-frame parent)))
  (message "Speedbar glue %s" (if my/speedbar-glue "on" "off")))

;;;; Buffer / display primitives (switch modes without killing the buffer)

(defun my/speedbar--ensure-buffer ()
  "Return a live `speedbar-buffer', creating it if needed."
  (unless (buffer-live-p speedbar-buffer)
    (setq speedbar-buffer (get-buffer-create " SPEEDBAR")))
  (with-current-buffer speedbar-buffer
    (unless (derived-mode-p 'speedbar-mode)
      (speedbar-mode))
    (setq-local mode-line-format nil)
    (setq-local cursor-type nil)
    (setq-local auto-hscroll-mode nil))
  speedbar-buffer)

(defun my/speedbar--delete-speedbar-frames ()
  "Delete every detached speedbar frame (live, cached, orphan).  Keep buffer."
  (save-current-buffer
    (let ((speedbar--window nil)
          (speedbar-frame (car (my/speedbar--speedbar-frames))))
      (ignore-errors (speedbar-frame-mode -1))))
  (dolist (fr (my/speedbar--speedbar-frames))
    (when (and (frame-live-p fr)
               (not (eq fr (my/speedbar--main-frame))))
      (delete-frame fr)))
  (my/speedbar--drop-cached-frame)
  (setq speedbar-frame nil
        speedbar-cached-frame nil
        my/speedbar-attached-frame nil))

(defun my/speedbar--fixed-width ()
  "Character width of the attached speedbar side window."
  (or speedbar-window-default-width my/speedbar-width))

;; The docked speedbar is a `sidepanel': that file owns making the window.

(defun my/speedbar--panel-buffer ()
  "The speedbar buffer, ready to be put in a side window.

`sidepanel' calls this immediately before displaying, which is where the frame
bookkeeping has to happen: the buffer has to know its parent frame before
speedbar draws into it."
  (my/speedbar--ensure-buffer)
  (setq speedbar-frame (selected-frame)
        my/speedbar-attached-frame (selected-frame)
        speedbar-select-frame-method 'attached)
  ;; `dframe-attached-frame' is `defvar-local'.
  (with-current-buffer speedbar-buffer
    (setq dframe-attached-frame (selected-frame)))
  speedbar-buffer)

(defun my/speedbar--panel-shown (panel)
  "Start speedbar up in PANEL's new window."
  (setq speedbar--window (sidepanel-win panel))
  (speedbar-reconfigure-keymaps)
  (speedbar-set-timer dframe-update-speed)
  (when (and (buffer-live-p speedbar-buffer)
             (zerop (buffer-size speedbar-buffer)))
    (with-current-buffer speedbar-buffer
      (speedbar-update-contents))))

(defun my/speedbar--panel-hidden (&rest _)
  "Forget the window speedbar was in."
  (setq speedbar--window nil)
  (when (and (boundp 'speedbar-frame)
             speedbar-frame
             (eq speedbar-frame (selected-frame)))
    (setq speedbar-frame nil)))

(defun my/speedbar--panel-restored (&rest _)
  "Re-sync speedbar after a tab change put it back."
  (my/speedbar--sync (or (my/speedbar--tab-buffer) (current-buffer)) nil))

(sidepanel-define 'speedbar
		  :buffer-function #'my/speedbar--panel-buffer
		  :owner-p (lambda (buffer) (eq buffer (bound-and-true-p speedbar-buffer)))
		  ;; A function, not a value: this runs long before the `use-package speedbar' block below sets.
		  :side (lambda () (or (bound-and-true-p speedbar-window-side) 'left))
		  :slot 0
		  :size #'my/speedbar--fixed-width
		  :fixed 'width
		  :on-show #'my/speedbar--panel-shown
		  :on-hide #'my/speedbar--panel-hidden
		  :on-restore #'my/speedbar--panel-restored)

(defun my/speedbar--hide-side-window ()
  "Remove the docked speedbar; keep its buffer."
  (sidepanel-hide 'speedbar))

(defun my/speedbar--show-side-window ()
  "Show speedbar as a fixed-width side window on the selected frame."
  (sidepanel-show 'speedbar))

(defun my/speedbar--to-frame ()
  "Show speedbar detached (own frame)."
  (save-current-buffer
    (let ((parent (my/speedbar--main-frame)))
      (unless (frame-live-p parent)
        (setq parent (selected-frame)))
      (my/speedbar--hide-side-window)
      ;; Collapse any existing speedbar frames to one before create-or-reuse.
      (let ((existing (my/speedbar--reconcile-frame)))
        ;; `dframe-attached-frame' is `defvar-local'.
        (setq my/speedbar-attached-frame parent
              my/speedbar-kind 'frame)
        (my/speedbar--ensure-buffer)
        (if existing
            (progn
              (make-frame-visible existing)
              (raise-frame existing)
              (setq speedbar-frame existing))
          (when (and (boundp 'speedbar-cached-frame)
                     speedbar-cached-frame
                     (frame-live-p speedbar-cached-frame))
            (delete-frame speedbar-cached-frame)
            (setq speedbar-cached-frame nil))
          (setq speedbar-frame nil)
          (speedbar-frame-mode 1)
          (my/speedbar--reconcile-frame)))
      (when (buffer-live-p speedbar-buffer)
        (with-current-buffer speedbar-buffer
          (setq dframe-attached-frame parent)))
      (when (frame-live-p speedbar-frame)
        (my/speedbar--follow-frame parent)))))

(defun my/speedbar--to-window ()
  "Show speedbar attached (side window).  Keep buffer; drop every frame."
  (my/speedbar--delete-speedbar-frames)
  (setq my/speedbar-kind 'window)
  ;; `sidepanel-show' is what marks it docked.
  (my/speedbar--show-side-window))

(defun my/speedbar--hide ()
  "Hide speedbar completely (frame and/or side window).  Kill the buffer."
  (my/speedbar--delete-speedbar-frames)
  (my/speedbar--hide-side-window)
  (speedbar-set-timer nil)
  (when (and (boundp 'speedbar-buffer) (buffer-live-p speedbar-buffer))
    (kill-buffer speedbar-buffer))
  (setq speedbar-buffer nil
        speedbar--window nil
        speedbar-frame nil
        dframe-attached-frame nil))

(defun my/speedbar--show (kind &optional buffer steal-focus)
  "Show speedbar as KIND (`frame' or `window') and sync to BUFFER."
  (pcase kind
    ('window (my/speedbar--to-window))
    (_ (my/speedbar--to-frame)))
  (my/speedbar--sync (or buffer (my/speedbar--tab-buffer) (current-buffer))
                     steal-focus))

;;;; Root / sync

(defun my/speedbar--root-for-buffer (&optional buffer)
  "Directory speedbar should root at for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((derived-mode-p 'dired-mode)
      default-directory)
     (buffer-file-name
      (or (when-let* ((proj (project-current nil)))
            (project-root proj))
          (file-name-directory buffer-file-name)))
     (t (expand-file-name "~")))))

(defun my/speedbar--tab-buffer ()
  "Buffer of the selected window on the main (non-speedbar) frame."
  (let* ((main (my/speedbar--main-frame))
         (win (and (frame-live-p main) (frame-selected-window main))))
    (when (and win (window-live-p win)
               (not (window-parameter win 'window-side)))
      (window-buffer win))))

(defun my/speedbar--expand-to-file (file)
  "Expand directory nodes so FILE is reachable.  Current buffer is speedbar."
  (let* ((root (expand-file-name default-directory))
         (file (expand-file-name file))
         (dir (file-name-directory file)))
    (when (string-prefix-p root dir)
      (let ((rel (file-relative-name dir root)))
        (unless (member rel '("" "./"))
          (dolist (part (split-string rel "/" t))
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "> " (regexp-quote part)
                           "\\(?:" speedbar-indicator-regex "\\)?$")
                   nil t)
              (speedbar-expand-line))))))))

(defun my/speedbar--clear-highlight ()
  "Remove `speedbar-selected-face' from the last highlighted file."
  (when (and speedbar-last-selected-file
             (speedbar-find-selected-file speedbar-last-selected-file))
    (let ((inhibit-read-only t))
      (put-text-property (match-beginning 1) (match-end 1)
                         'face 'speedbar-file-face))
    (setq speedbar-last-selected-file nil)))

(defun my/speedbar--highlight-file (file)
  "Select and face FILE in the speedbar buffer if it is shown."
  (let ((file (expand-file-name file))
        (inhibit-read-only t))
    (unless (and speedbar-last-selected-file
                 (string= speedbar-last-selected-file file))
      (my/speedbar--clear-highlight))
    (when (speedbar-goto-this-file file)
      (when (speedbar-find-selected-file file)
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'speedbar-selected-face)
        (setq speedbar-last-selected-file file))
      (speedbar-position-cursor-on-line)
      t)))

(defun my/speedbar--sync (&optional buffer steal-focus)
  "Point speedbar at BUFFER's root and highlight its file."
  (let ((buffer (or buffer (my/speedbar--tab-buffer) (current-buffer))))
    (when (and (boundp 'speedbar-buffer)
               (buffer-live-p speedbar-buffer)
               (my/speedbar--current)
               (buffer-live-p buffer)
               (not (eq buffer speedbar-buffer)))
      (let* ((file (and (not (with-current-buffer buffer
                               (derived-mode-p 'dired-mode)))
                        (buffer-file-name buffer)))
             (root (file-name-as-directory
                    (expand-file-name (my/speedbar--root-for-buffer buffer))))
             (root-abs (directory-file-name (expand-file-name root))))
        (with-current-buffer speedbar-buffer
          (let* ((cur (directory-file-name
                       (expand-file-name default-directory)))
                 (empty (zerop (buffer-size)))
                 (root-changed (not (string= cur root-abs))))
            (when (or empty root-changed)
              (setq default-directory (file-name-as-directory root-abs)
                    speedbar-shown-directories
                    (list (expand-file-name default-directory))
                    speedbar-full-text-cache nil
                    speedbar-last-selected-file nil)
              (speedbar-update-contents))
            (when file
              (my/speedbar--expand-to-file file)
              (my/speedbar--highlight-file file))))
        (when steal-focus
          (pcase (my/speedbar--current)
            ('window
             (when (and speedbar--window (window-live-p speedbar--window))
               (select-window speedbar--window)))
            ('frame
             (when (frame-live-p speedbar-frame)
               (select-frame-set-input-focus speedbar-frame)))))))))

;;;; Tab integration

(defun my/speedbar--do-restore ()
  "Re-sync a detached speedbar after a tab change."
  (setq my/speedbar--restore-timer nil)
  (unless my/speedbar--tab-guard
    (let ((my/speedbar--tab-guard t))
      (when (my/speedbar--frame-live-p)
        (my/speedbar--sync (my/speedbar--tab-buffer) nil)))))

(defun my/speedbar--after-tab-change (&rest _)
  "Schedule the detached speedbar's re-sync after tab select/open."
  (when (my/speedbar--frame-live-p)
    (when (timerp my/speedbar--restore-timer)
      (cancel-timer my/speedbar--restore-timer))
    (setq my/speedbar--restore-timer
          (run-at-time 0 nil #'my/speedbar--do-restore))))

;;;; Maximize / fullscreen - auto-attach while chrome is expanded

(defvar my/speedbar--chrome-timer nil
  "Timer for deferred chrome (fullscreen/maximize) reaction.")

(defun my/speedbar--fullscreen-value-p (value)
  "Non-nil when VALUE is a maximized/fullscreen `fullscreen' parameter."
  (memq value '(fullboth fullscreen maximized fullwidth fullheight)))

(defun my/speedbar--fullscreen-p (&optional frame)
  "Non-nil when FRAME is maximized or fullscreen."
  (my/speedbar--fullscreen-value-p (frame-parameter frame 'fullscreen)))

(defun my/speedbar--forced-kind ()
  "The one presentation the main frame's chrome leaves on offer, or nil."
  (and (my/speedbar--fullscreen-p (my/speedbar--main-frame)) 'window))

(defun my/speedbar--react-chrome (frame entering)
  "Dock or float the speedbar after FRAME entered or left expanded chrome."
  (setq my/speedbar--chrome-timer nil)
  (cond
   ((not (frame-live-p frame))
    (setq my/speedbar--pre-fs-kind nil))
   ;; Expanded while floating - dock it, and remember to undo.
   (entering
    (when (eq (my/speedbar--current) 'frame)
      (setq my/speedbar--pre-fs-kind 'frame)
      (with-selected-frame frame
        (my/speedbar--show 'window))))
   ;; Came back, and the dock was ours to undo.
   ((eq my/speedbar--pre-fs-kind 'frame)
    (setq my/speedbar--pre-fs-kind nil)
    (when (or (eq (my/speedbar--current) 'window) (my/speedbar-docked-p))
      (with-selected-frame frame
        (my/speedbar--show 'frame))))))

(defun my/speedbar--watch-chrome (frame)
  "React to FRAME entering or leaving maximized/fullscreen."
  (when (frame-live-p frame)
    (if (my/speedbar--is-speedbar-frame-p frame)
        (when (frame-parameter frame 'fullscreen)
          (set-frame-parameter frame 'fullscreen nil))
      (let ((now (and (my/speedbar--fullscreen-p frame) t))
            (was (frame-parameter frame 'my/speedbar--fs-state)))
        (unless (eq now was)
          ;; Record before acting: docking and floating both re-enter here.
          (set-frame-parameter frame 'my/speedbar--fs-state now)
          (when (timerp my/speedbar--chrome-timer)
            (cancel-timer my/speedbar--chrome-timer))
          (setq my/speedbar--chrome-timer
                (run-at-time (if now 0.3 0.15) nil
                             #'my/speedbar--react-chrome frame now)))))))

(defun my/speedbar--around-frame-chrome (fn &rest args)
  "Block maximize/fullscreen commands aimed at the speedbar frame."
  (if (my/speedbar--is-speedbar-frame-p (or (car args) (selected-frame)))
      (message "Speedbar cannot be maximized or fullscreen")
    (apply fn args)))

;;;; Commands

(defun my/speedbar-toggle ()
  "Show the sidebar docked, or close it if anything is already showing.

Docked whichever way it was last seen: opening is one thing and floating it is
another, and the second is `my/speedbar-detach'.  Closing does not care which
kind was up -- a detached frame goes as readily as the side window."
  (interactive)
  (my/speedbar--reconcile-frame)
  ;; An explicit choice outranks the pending auto-restore: picking by hand while expanded should.
  (setq my/speedbar--pre-fs-kind nil)
  (if-let* ((current (my/speedbar--current)))
      (progn
        (setq my/speedbar-kind current)
        (my/speedbar--hide))
    (my/speedbar--show (or (my/speedbar--forced-kind) 'window))))

(defun my/speedbar-detach ()
  "Float the sidebar out into its own frame, or dock it again if it is out.

With the sidebar closed this opens it floating, which is the one way to get
there -- `my/speedbar-toggle' always opens it docked.  While the main frame is
expanded there is nowhere sensible for a frame to go, so this docks instead."
  (interactive)
  (my/speedbar--reconcile-frame)
  (setq my/speedbar--pre-fs-kind nil)
  (if-let* ((forced (my/speedbar--forced-kind)))
      (if (eq (my/speedbar--current) forced)
          (my/speedbar--sync (my/speedbar--tab-buffer))
        (my/speedbar--show forced))
    (pcase (my/speedbar--current)
      ('frame (my/speedbar--show 'window))
      (_ (my/speedbar--show 'frame)))))

(defun my/speedbar-reveal ()
  "Show the sidebar at the project root and highlight the current file."
  (interactive)
  (let ((buffer (current-buffer)))
    (my/speedbar--reconcile-frame)
    (unless (my/speedbar--current)
      (my/speedbar--show (or (my/speedbar--forced-kind) 'window) buffer nil))
    (my/speedbar--sync buffer t)))

;;;; Family

(defconst my/speedbar-faces
  '(speedbar-directory-face speedbar-file-face speedbar-selected-face
			    speedbar-highlight-face speedbar-button-face speedbar-tag-face
			    speedbar-separator-face)
  "The speedbar faces `my/speedbar--refresh-faces' sets the family on.")

(defun my/speedbar--refresh-faces (&rest _)
  "Set the sidebar's family, for whichever theme is now in force."
  (let ((family (face-attribute 'variable-pitch :family nil t)))
    (unless (eq family 'unspecified)
      (dolist (face my/speedbar-faces)
        (when (facep face)
          (set-face-attribute face nil :family family))))))

;;;; Icons - nerd-icons for tags/status; stock XPMs for dirs and files

(defvar my/speedbar--img-box-plus nil)
(defvar my/speedbar--img-box-minus nil)
(defvar my/speedbar--img-mail nil)
(defvar my/speedbar--img-doc nil)
(defvar my/speedbar--img-info nil)
(defvar my/speedbar--img-tag nil)
(defvar my/speedbar--img-tag-gt nil)
(defvar my/speedbar--img-tag-v nil)
(defvar my/speedbar--img-tag-type nil)
(defvar my/speedbar--img-checkout nil)
(defvar my/speedbar--img-object nil)
(defvar my/speedbar--img-object-ood nil)
(defvar my/speedbar--img-label nil)
(defvar my/speedbar--img-lock nil)

(defun my/speedbar--ni (fn name &optional face height)
  "Nerd-icons glyph for NAME via FN, safe as a `display' property value."
  (let* ((raw (funcall fn name
                       :height (or height 0.9)
                       :face (or face 'nerd-icons-silver)))
         (face (or (get-text-property 0 'face raw) 'default))
         (ch (substring-no-properties raw)))
    (propertize ch 'face face 'font-lock-face face)))

(defun my/speedbar--setup-nerd-icons ()
  "Wire nerd-icons into speedbar buttons and status indicators."
  (require 'nerd-icons)
  (let ((h 0.9))
    (setq my/speedbar--img-box-plus
          (my/speedbar--ni #'nerd-icons-faicon "nf-fa-plus_square"
                           'nerd-icons-green h)
          my/speedbar--img-box-minus
          (my/speedbar--ni #'nerd-icons-faicon "nf-fa-minus_square"
                           'nerd-icons-green h)
          my/speedbar--img-mail
          (my/speedbar--ni #'nerd-icons-faicon "nf-fa-envelope"
                           'nerd-icons-lblue h)
          my/speedbar--img-doc
          (my/speedbar--ni #'nerd-icons-faicon "nf-fa-book"
                           'nerd-icons-lcyan h)
          my/speedbar--img-info
          (my/speedbar--ni #'nerd-icons-faicon "nf-fa-info_circle"
                           'nerd-icons-blue h)
          my/speedbar--img-tag
          (my/speedbar--ni #'nerd-icons-octicon "nf-oct-tag"
                           'nerd-icons-purple h)
          my/speedbar--img-tag-gt
          (my/speedbar--ni #'nerd-icons-octicon "nf-oct-chevron_right"
                           'nerd-icons-purple h)
          my/speedbar--img-tag-v
          (my/speedbar--ni #'nerd-icons-octicon "nf-oct-chevron_down"
                           'nerd-icons-purple h)
          my/speedbar--img-tag-type
          (my/speedbar--ni #'nerd-icons-mdicon "nf-md-tag_outline"
                           'nerd-icons-lpink h)
          my/speedbar--img-checkout
          (my/speedbar--ni #'nerd-icons-devicon "nf-dev-git_branch"
                           'nerd-icons-lgreen h)
          my/speedbar--img-object
          (my/speedbar--ni #'nerd-icons-octicon "nf-oct-file_binary"
                           'nerd-icons-dsilver h)
          my/speedbar--img-object-ood
          (my/speedbar--ni #'nerd-icons-faicon "nf-fa-exclamation_triangle"
                           'nerd-icons-orange h)
          my/speedbar--img-label
          (my/speedbar--ni #'nerd-icons-mdicon "nf-md-label_outline"
                           'nerd-icons-silver h)
          my/speedbar--img-lock
          (my/speedbar--ni #'nerd-icons-faicon "nf-fa-lock"
                           'nerd-icons-yellow h))

    ;; Status line suffixes (inserted as real text, not button overlays).
    (setq speedbar-vc-indicator my/speedbar--img-checkout
          speedbar-obj-indicator (cons my/speedbar--img-object
                                       my/speedbar--img-object-ood)
          speedbar-object-read-only-indicator my/speedbar--img-lock
          speedbar-indicator-regex
          (concat (regexp-quote (or (bound-and-true-p
                                     speedbar-indicator-separator)
                                    " "))
                  "\\("
                  (regexp-quote speedbar-vc-indicator) "\\|"
                  (regexp-quote (car speedbar-obj-indicator)) "\\|"
                  (regexp-quote (cdr speedbar-obj-indicator)) "\\|"
                  (regexp-quote speedbar-object-read-only-indicator)
                  "\\)*"))

    ;; Button overlays: stock dir/file images + nerd-icons for the rest.
    (setq speedbar-expand-image-button-alist
          `(;; directories - keep ezimage
            ("<+>" . ezimage-directory-plus)
            ("<->" . ezimage-directory-minus)
            ("< >" . ezimage-directory)
            ;; files - keep ezimage
            ("[+]" . ezimage-page-plus)
            ("[-]" . ezimage-page-minus)
            ("[?]" . ezimage-page)
            ("[ ]" . ezimage-page)
            ;; tags / misc - nerd-icons (vars hold display strings)
            ("{+}" . my/speedbar--img-box-plus)
            ("{-}" . my/speedbar--img-box-minus)
            ("<M>" . my/speedbar--img-mail)
            ("<d>" . my/speedbar--img-doc)
            ("<i>" . my/speedbar--img-info)
            (" =>" . my/speedbar--img-tag)
            (" +>" . my/speedbar--img-tag-gt)
            (" ->" . my/speedbar--img-tag-v)
            (">"   . my/speedbar--img-tag)
            ("@"   . my/speedbar--img-tag-type)
            ("  @" . my/speedbar--img-tag-type)
            ("*"   . my/speedbar--img-checkout)
            ("#"   . my/speedbar--img-object)
            ("!"   . my/speedbar--img-object-ood)
            ("//"  . my/speedbar--img-label)
            ("%"   . my/speedbar--img-lock)))))

;;;; Package setup

(use-package speedbar
  :ensure nil
  :demand t
  :bind (("M-e" . my/speedbar-detach))
  :custom
  (speedbar-prefer-window nil)
  (speedbar-window-side 'left)
  (speedbar-window-default-width my/speedbar-width)
  (speedbar-window-max-width my/speedbar-width)
  (speedbar-window-dedicated-window t)
  ;; Stock alist, but 32 columns instead of 20.
  (speedbar-frame-parameters `((minibuffer . nil)
                               (width . ,my/speedbar-width)
                               (border-width . 0)
                               (menu-bar-lines . 0)
                               (tool-bar-lines . 0)
                               (tab-bar-lines . 0)
                               (unsplittable . t)
                               (left-fringe . 0)
                               (my/speedbar-frame . t)))
  (speedbar-show-unknown-files t)
  (speedbar-directory-unshown-regexp
   "^\\(\\.git\\|\\.zig-cache\\|\\.DS_Store\\)\\'")
  (speedbar-indentation-width 2)
  (speedbar-use-images t)
  (speedbar-update-flag nil)
  (speedbar-default-position 'left)
  :config
  (my/speedbar--setup-nerd-icons)
  (my/speedbar--refresh-faces)
  (add-hook 'enable-theme-functions #'my/speedbar--refresh-faces)

  (add-hook 'move-frame-functions #'my/speedbar--follow-frame)
  (add-hook 'window-size-change-functions #'my/speedbar--watch-chrome)
  (add-hook 'tab-bar-tab-post-select-functions #'my/speedbar--after-tab-change)
  (add-hook 'tab-bar-tab-post-open-functions #'my/speedbar--after-tab-change)

  ;; Button overlays: stock dir/file images + nerd-icons for the rest.
  (advice-add 'display-buffer-in-new-tab :after
              (lambda (&rest _) (my/speedbar--after-tab-change)))
  (advice-add 'toggle-frame-maximized :around #'my/speedbar--around-frame-chrome)
  (advice-add 'toggle-frame-fullscreen :around #'my/speedbar--around-frame-chrome)
  ;; emacs-mac native fullscreen (what F11 / C-s-f often end up calling).
  (when (fboundp 'mac-toggle-frame-fullscreen)
    (advice-add 'mac-toggle-frame-fullscreen :around
                #'my/speedbar--around-frame-chrome))

  (add-hook 'speedbar-mode-hook
            (lambda ()
              (setq-local mode-line-format nil)
              (setq-local cursor-type nil)
              ;; Navigator only: no M-x, no eval, no accidental chrome keys.
              (keymap-local-set "M-x" #'undefined)
              (keymap-local-set "M-X" #'undefined)
              (keymap-local-set "M-`" #'undefined)
              (keymap-local-set "M-:" #'undefined)
              (keymap-local-set "M-!" #'undefined)
              (keymap-local-set "M-&" #'undefined)
              (keymap-local-set "<f11>" #'undefined)
              (keymap-local-set "C-x C-c" #'undefined)
              (keymap-local-set "C-x 5 2" #'undefined)
              (keymap-local-set "C-x 5 0" #'undefined)))

  (keymap-set speedbar-mode-map "G" #'my/speedbar-toggle-glue)
  (keymap-set speedbar-mode-map "M-x" #'undefined)
  (keymap-set speedbar-mode-map "M-X" #'undefined)
  (keymap-set speedbar-mode-map "M-:" #'undefined)
  (keymap-set speedbar-mode-map "<f11>" #'undefined)
  (keymap-set speedbar-mode-map
              "<remap> <execute-extended-command>" #'undefined)
  (keymap-set speedbar-mode-map
              "<remap> <toggle-frame-fullscreen>" #'undefined)
  (keymap-set speedbar-mode-map
              "<remap> <mac-toggle-frame-fullscreen>" #'undefined)
  (keymap-set speedbar-mode-map
              "<remap> <toggle-frame-maximized>" #'undefined))

;;; tree.el ends here
