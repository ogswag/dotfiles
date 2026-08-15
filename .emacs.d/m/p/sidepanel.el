;;; sidepanel.el --- Side windows that survive a tab change -*- lexical-binding: t; -*-

;;; Commentary:
;; one registry, one tab guard.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tab-bar)

;;;; The registry

(cl-defstruct (sidepanel (:constructor sidepanel--create) (:copier nil))
  "One side window this file owns."
  name                ; symbol; the registry key
  buffer-function     ; called for the buffer, immediately before displaying it
  owner-p             ; optional (BUFFER -> bool): is this buffer ours
  side                ; `left', `right', `top' or `bottom'
  slot                ; `display-buffer-in-side-window' slot
  size                ; number, float fraction, or a function returning one
  fixed               ; `width', `height', or nil for a panel with no own axis
  resizable           ; non-nil to leave the fixed axis draggable by hand
  reachable           ; non-nil to let `C-x o' land here
  group               ; panels shown and hidden together
  order               ; restore order across groups
  predicate           ; optional (-> bool): does this member apply right now
  wanted              ; should it be on screen -- what the tab guard reads
  buffer              ; the buffer last displayed, for `sidepanel-reconcile'
  win                 ; the live window, or nil
  on-show on-hide on-restore)

(defvar sidepanel--registry nil
  "Alist of (NAME . `sidepanel'), in registration order.")

(defconst sidepanel--side-order '((left . 0) (right . 10) (top . 20) (bottom . 30))
  "Default `order' per side.  Sides are restored before the ends.")

(defun sidepanel--panels ()
  "Every registered panel."
  (mapcar #'cdr sidepanel--registry))

(defun sidepanel-get (name)
  "The panel registered as NAME, or nil."
  (cdr (assq name sidepanel--registry)))

(defun sidepanel--need (name)
  "The panel registered as NAME, or signal."
  (or (sidepanel-get name) (error "No side panel named `%s'" name)))

;;;###autoload
(cl-defun sidepanel-define (name &key buffer-function owner-p (side 'left) (slot 0)
                                 size fixed resizable reachable group order predicate
                                 on-show on-hide on-restore)
  "Register NAME as a side panel and return it.

BUFFER-FUNCTION is called for the buffer to display, immediately before the
window is made -- late enough that a panel can do its own setup there.
OWNER-P recognises a buffer as this panel's when the buffer varies, as it does
for a panel showing one of several sessions.

SIDE and SLOT go to `display-buffer-in-side-window'.  Either may be a function,
so that a panel whose edge is a user option follows it without being redefined;
ORDER is still taken from SIDE once, at registration.
SIZE is a column or line
count, a float fraction, or a function returning one.  FIXED names the axis the
panel owns -- `width' or `height' -- and the axis also decides which way SIZE is
read, which is why a bottom panel can still be a fixed-width one.  RESIZABLE
leaves that axis draggable by hand rather than locked, for a panel that should
remember a size the user chose.  REACHABLE lets `C-x o' land in the panel, for
one the point is meant to enter.

GROUP names panels shown and hidden together, ORDER their rank against other
groups, and PREDICATE decides whether a member takes part this time round.

ON-SHOW, ON-HIDE and ON-RESTORE are called with the panel."
  (let ((panel (sidepanel--create
                :name name :buffer-function buffer-function :owner-p owner-p
                :side side :slot slot :size size :fixed fixed
                :resizable resizable :reachable reachable
                :group (or group name)
                :order (or order (alist-get side sidepanel--side-order 0))
                :predicate predicate
                :on-show on-show :on-hide on-hide :on-restore on-restore))
        (cell (assq name sidepanel--registry)))
    ;; Redefining keeps the live window and the wanted flag.
    (when cell
      (setf (sidepanel-win panel) (sidepanel-win (cdr cell))
            (sidepanel-buffer panel) (sidepanel-buffer (cdr cell))
            (sidepanel-wanted panel) (sidepanel-wanted (cdr cell))))
    (if cell
        (setcdr cell panel)
      (setq sidepanel--registry
            (append sidepanel--registry (list (cons name panel)))))
    panel))

;;;; Geometry

(defun sidepanel--side (panel)
  "PANEL's side now, resolving a side function."
  (let ((side (sidepanel-side panel)))
    (if (functionp side) (funcall side) side)))

(defun sidepanel--axis (panel)
  "The axis PANEL owns: `width' or `height'.

Its own `fixed' axis when it has one, else the one its side implies.  A panel
sharing the bottom edge with another is a width panel even though the edge is a
horizontal one, which is why this is not read off SIDE alone."
  (or (sidepanel-fixed panel)
      (if (memq (sidepanel--side panel) '(left right)) 'width 'height)))

(defun sidepanel--size (panel)
  "PANEL's requested size now, resolving a size function."
  (let ((size (sidepanel-size panel)))
    (if (functionp size) (funcall size) size)))

(defun sidepanel--slot (panel)
  "PANEL's slot now, resolving a slot function."
  (let ((slot (sidepanel-slot panel)))
    (if (functionp slot) (funcall slot) slot)))

(defun sidepanel--action (panel)
  "The `display-buffer' action alist for PANEL."
  (let ((width (eq (sidepanel--axis panel) 'width))
        (size (sidepanel--size panel)))
    `((side . ,(sidepanel--side panel))
      (slot . ,(sidepanel--slot panel))
      (dedicated . t)
      ,@(when size
          (list (cons (if width 'window-width 'window-height) size)))
      (preserve-size . ,(if width '(t . nil) '(nil . t))))))

(defun sidepanel--harden (panel window)
  "Apply PANEL's window parameters to WINDOW."
  (when (window-live-p window)
    (let ((width (eq (sidepanel--axis panel) 'width)))
      (set-window-parameter window 'no-delete-other-windows t)
      (set-window-dedicated-p window t)
      (when (sidepanel-fixed panel)
        ;; Furniture: C-x o should never land here.
        (unless (sidepanel-reachable panel)
          (set-window-parameter window 'no-other-window t))
        ;; No fringes, no margins.
        (set-window-fringes window 0 0)
        (set-window-margins window 0 0))
      ;; `window-preserved-size' is (BUFFER WIDTH HEIGHT) and is only honoured when the car is `eq' to.
      (window-preserve-size window width t)
      (when (sidepanel-fixed panel)
        (let ((want (sidepanel--size panel)))
          (with-selected-window window
            ;; Off first: a window already fixed refuses to be resized.
            (setq-local window-size-fixed nil)
            (when (natnump want)
              (window-resize window
                             (- want (if width
                                         (window-total-width window)
                                       (window-total-height window)))
                             width t))
            ;; Locked only when the panel is not meant to be dragged.
            (setq-local window-size-fixed
                        (unless (sidepanel-resizable panel)
                          (if width 'width 'height)))))))))

(defun sidepanel--unfix (window)
  "Let WINDOW be resized and deleted again."
  (when (window-live-p window)
    (with-selected-window window
      (setq-local window-size-fixed nil))))

;;;; Showing and hiding

(defun sidepanel--applies-p (panel)
  "Non-nil when PANEL is wanted and its `predicate' agrees."
  (and (sidepanel-wanted panel)
       (let ((predicate (sidepanel-predicate panel)))
         (or (null predicate) (funcall predicate)))))

(defun sidepanel--before-p (a b)
  "Non-nil when panel A is shown before panel B.

By `order' across groups, then by distance from slot 0 within one.  Slot 0
first is not a preference: it becomes the major side window and takes the whole
edge, which the other slots then split.  Reversed, the first one shown is left
with half the space."
  (let ((oa (sidepanel-order a)) (ob (sidepanel-order b)))
    (if (= oa ob)
        (< (abs (sidepanel--slot a)) (abs (sidepanel--slot b)))
      (< oa ob))))

(defun sidepanel--group-of (panel)
  "Every registered panel in PANEL's group, in show order."
  (sort (seq-filter (lambda (p) (eq (sidepanel-group p) (sidepanel-group panel)))
                    (sidepanel--panels))
        #'sidepanel--before-p))

(defun sidepanel--wanted ()
  "Every wanted panel, in show order."
  (sort (seq-filter #'sidepanel-wanted (sidepanel--panels))
        #'sidepanel--before-p))

(defun sidepanel--owns-p (panel buffer)
  "Non-nil when BUFFER belongs to PANEL."
  (or (and (sidepanel-buffer panel) (eq buffer (sidepanel-buffer panel)))
      (when-let* ((owner-p (sidepanel-owner-p panel)))
        (funcall owner-p buffer))))

(defun sidepanel--show-1 (panel)
  "Display PANEL's buffer in its side window and return the window."
  (when-let* ((buffer-function (sidepanel-buffer-function panel))
              (buffer (funcall buffer-function))
              ((buffer-live-p buffer)))
    (let ((window (display-buffer-in-side-window buffer (sidepanel--action panel))))
      (dolist (other (sidepanel--panels))
        (when (and (not (eq other panel)) (eq (sidepanel-win other) window))
          (setf (sidepanel-win other) nil)))
      (setf (sidepanel-win panel) window
            (sidepanel-buffer panel) buffer)
      (sidepanel--harden panel window)
      (when-let* ((on-show (sidepanel-on-show panel)))
        (funcall on-show panel))
      window)))

(defun sidepanel--hide-1 (panel)
  "Take PANEL's window off the frame, leaving its buffer alone."
  (let ((window (sidepanel-win panel)))
    (when (window-live-p window)
      (when-let* ((on-hide (sidepanel-on-hide panel)))
        (funcall on-hide panel))
      (sidepanel--unfix window)
      (ignore-errors (delete-window window)))
    (setf (sidepanel-win panel) nil)))

(defun sidepanel--show-these (panels)
  "Display PANELS, which must already be in show order."
  ;; Unlimited slots.
  (let ((window-sides-slots '(nil nil nil nil)))
    ;; Retire what is not wanted this time *before* asking for the rest.
    (dolist (panel (reverse panels))
      (unless (sidepanel--applies-p panel) (sidepanel--hide-1 panel)))
    (dolist (panel panels)
      (when (sidepanel--applies-p panel) (sidepanel--show-1 panel)))))

;;;###autoload
(defun sidepanel-show (name &optional select)
  "Put NAME's group on the frame.  SELECT focuses NAME's own window.

Returns NAME's window, which is nil when its `predicate' kept it off."
  (let* ((panel (sidepanel--need name))
         (group (sidepanel--group-of panel))
         (return-to (selected-window)))
    (dolist (member group)
      (setf (sidepanel-wanted member) t))
    (sidepanel-reconcile)
    (sidepanel--show-these group)
    (if (and select (window-live-p (sidepanel-win panel)))
        (select-window (sidepanel-win panel))
      (when (window-live-p return-to) (select-window return-to)))
    (sidepanel-win panel)))

;;;###autoload
(defun sidepanel-hide (name)
  "Take NAME's group off the frame, leaving its buffers alone."
  (let ((panel (sidepanel--need name)))
    (dolist (member (reverse (sidepanel--group-of panel)))
      (setf (sidepanel-wanted member) nil)
      (sidepanel--hide-1 member))))

;;;###autoload
(defun sidepanel-conceal (name)
  "Take NAME's group off the frame but leave it wanted.

Unlike `sidepanel-hide', the group has not been dismissed: the next tab change
puts it back.  For a panel with nothing to show at the moment."
  (let ((panel (sidepanel--need name)))
    (dolist (member (reverse (sidepanel--group-of panel)))
      (sidepanel--hide-1 member))))

;;;###autoload
(defun sidepanel-toggle (name &optional select)
  "Show NAME's group if it is off the frame, hide it if it is on."
  (if (sidepanel-visible-p name)
      (sidepanel-hide name)
    (sidepanel-show name select)))

(defun sidepanel-visible-p (name)
  "Non-nil when NAME is actually on screen."
  (when-let* ((panel (sidepanel-get name)))
    (and (window-live-p (sidepanel-win panel)) t)))

(defun sidepanel-window (name)
  "NAME's live window, or nil."
  (when-let* ((panel (sidepanel-get name))
              (window (sidepanel-win panel))
              ((window-live-p window)))
    window))

(defun sidepanel-wanted-p (name)
  "Non-nil when NAME should be on screen, whether or not it is."
  (when-let* ((panel (sidepanel-get name)))
    (and (sidepanel-wanted panel) t)))

(defun sidepanel-reconcile (&optional frame)
  "Sort out side windows on FRAME that this file did not put there.

A restored window configuration can hand back a window a panel has forgotten,
or a second copy of one it still holds.  The forgotten one is adopted, the
duplicate deleted, and a window belonging to a panel nobody wants is deleted
too."
  (dolist (panel (sidepanel--panels))
    (unless (window-live-p (sidepanel-win panel))
      (setf (sidepanel-win panel) nil)))
  (dolist (window (window-list frame 'no-minibuf))
    (when (and (window-live-p window)
               (window-parameter window 'window-side))
      (when-let* ((buffer (window-buffer window))
                  (owner (seq-find (lambda (panel) (sidepanel--owns-p panel buffer))
                                   (sidepanel--panels)))
                  ((not (eq window (sidepanel-win owner)))))
        (if (and (sidepanel-wanted owner) (null (sidepanel-win owner)))
            (setf (sidepanel-win owner) window)
          (sidepanel--unfix window)
          (ignore-errors (delete-window window)))))))

;;;; Surviving a tab change

(defvar sidepanel--tab-guard nil
  "Bound non-nil while this file is itself moving panels around.")

(defvar sidepanel--restore-timer nil
  "Timer running `sidepanel--do-restore', or nil.")

(defun sidepanel--peel ()
  "Take every wanted panel off the frame, keeping it wanted."
  (dolist (panel (reverse (sidepanel--wanted)))
    (sidepanel--hide-1 panel)))

(defun sidepanel--before-tab-change (&rest _)
  "Peel the panels off before tab-bar snapshots the windows."
  (unless sidepanel--tab-guard
    (let ((sidepanel--tab-guard t))
      (sidepanel--peel))
    ;; Schedule the restore here as well as from the post-select hook.
    (sidepanel--after-tab-change)))

(defun sidepanel--do-restore ()
  "Put every wanted panel back after a tab change."
  (setq sidepanel--restore-timer nil)
  (unless sidepanel--tab-guard
    (let ((sidepanel--tab-guard t)
          (return-to (selected-window))
          (panels (sidepanel--wanted)))
      (sidepanel--peel)
      (sidepanel--show-these panels)
      (dolist (panel panels)
        (when-let* (((window-live-p (sidepanel-win panel)))
                    (on-restore (sidepanel-on-restore panel)))
          (funcall on-restore panel)))
      (when (window-live-p return-to) (select-window return-to)))))

(defun sidepanel--after-tab-change (&rest _)
  "Schedule the panels' return after a tab is selected or opened."
  (when (sidepanel--wanted)
    (when (timerp sidepanel--restore-timer)
      (cancel-timer sidepanel--restore-timer))
    (setq sidepanel--restore-timer (run-at-time 0 nil #'sidepanel--do-restore))))

(defun sidepanel--around-tab--tab (fn &rest args)
  "Serialize a tab with no panels in it.  FN is called with ARGS."
  (if (or sidepanel--tab-guard (null (sidepanel--wanted)))
      (apply fn args)
    (let ((sidepanel--tab-guard t))
      (sidepanel--peel)
      (apply fn args))))

(add-hook 'tab-bar-tab-post-select-functions #'sidepanel--after-tab-change 90)
(add-hook 'tab-bar-tab-post-open-functions #'sidepanel--after-tab-change 90)

(advice-add 'tab-bar-select-tab :before #'sidepanel--before-tab-change)
(advice-add 'tab-bar-new-tab :before #'sidepanel--before-tab-change)
(advice-add 'tab-bar-new-tab-to :before #'sidepanel--before-tab-change)
(advice-add 'tab-bar--tab :around #'sidepanel--around-tab--tab)
(advice-add 'display-buffer-in-new-tab :after #'sidepanel--after-tab-change)

;;;; Furniture buffers

(cl-defun sidepanel-setup-buffer (&key keep-cursor keep-mode-line undo)
  "Harden the current buffer as panel furniture.

Call this from a panel major mode's body.  KEEP-CURSOR and KEEP-MODE-LINE
leave those alone, for a panel the point really does enter.  UNDO switches
undo back on, which a buffer whose name starts with a space needs: Emacs turns
it off at birth for those, and a panel the user types into wants it."
  (unless keep-mode-line (setq-local mode-line-format nil))
  (unless keep-cursor (setq-local cursor-type nil))
  (setq-local truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              left-margin-width 0
              right-margin-width 0)
  ;; No "$" at the right edge, and none of the "\" a wrapped line would leave: a row that does not.
  (let ((table (make-display-table)))
    (set-display-table-slot table 'truncation ?\s)
    (set-display-table-slot table 'wrap ?\s)
    (setq-local buffer-display-table table))
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1))
  (when (and undo (eq buffer-undo-list t))
    (setq buffer-undo-list nil)))

;;;; Self-test

(defun sidepanel--self-test ()
  "Exercise the registry and the window lifecycle.  Signals on the first failure."
  (let ((sidepanel--registry nil)
        (main (get-buffer-create " *sidepanel test main*"))
        (aside (get-buffer-create " *sidepanel test aside*"))
        (failures 0))
    (cl-flet ((check (label got want)
                (unless (equal got want)
                  (setq failures (1+ failures))
                  (princ (format "FAIL %-38s got %S want %S\n" label got want)))))
      (sidepanel-define 'test-main
        :buffer-function (lambda () main)
        :side 'bottom :slot 0 :size 8 :group 'test)
      (sidepanel-define 'test-aside
        :buffer-function (lambda () aside)
        :side 'bottom :slot 1 :size 20 :fixed 'width :group 'test)

      (check "axis of a plain bottom panel"
             (sidepanel--axis (sidepanel-get 'test-main)) 'height)
      (check "axis of a fixed-width bottom panel"
             (sidepanel--axis (sidepanel-get 'test-aside)) 'width)
      (check "slot 0 shows first"
             (mapcar #'sidepanel-name (sidepanel--group-of (sidepanel-get 'test-aside)))
             '(test-main test-aside))

      (sidepanel-show 'test-main)
      (check "group member 1 is up" (sidepanel-visible-p 'test-main) t)
      (check "group member 2 is up" (sidepanel-visible-p 'test-aside) t)
      (check "side parameter"
             (window-parameter (sidepanel-window 'test-main) 'window-side) 'bottom)
      (check "fixed member took its width"
             (window-total-width (sidepanel-window 'test-aside)) 20)
      (check "fixed member is fixed"
             (buffer-local-value 'window-size-fixed aside) 'width)
      (check "draggable member is not fixed"
             (buffer-local-value 'window-size-fixed main) nil)

      (sidepanel-hide 'test-main)
      (check "group member 1 is down" (sidepanel-visible-p 'test-main) nil)
      (check "group member 2 is down" (sidepanel-visible-p 'test-aside) nil)
      (check "nothing is wanted" (sidepanel-wanted-p 'test-main) nil)
      (check "one window left" (length (window-list nil 'no-minibuf)) 1)

      ;; A member whose predicate says no stays off, and the other still opens.
      (setf (sidepanel-predicate (sidepanel-get 'test-aside)) #'ignore)
      (sidepanel-show 'test-main)
      (check "predicate kept member 2 off" (sidepanel-visible-p 'test-aside) nil)
      (check "member 1 opened anyway" (sidepanel-visible-p 'test-main) t)
      (setf (sidepanel-predicate (sidepanel-get 'test-aside)) nil)

      ;; A stray window a configuration restore left behind is deleted, and a forgotten one is adopted.
      (let ((stray (display-buffer-in-side-window
                    aside '((side . bottom) (slot . 2) (dedicated . t)))))
        (check "stray exists" (window-live-p stray) t)
        (sidepanel-reconcile)
        (check "stray adopted for a wanted panel" (sidepanel-window 'test-aside) stray))

      (sidepanel-hide 'test-main)
      (sidepanel-toggle 'test-main)
      (check "toggle showed it" (sidepanel-visible-p 'test-main) t)
      (sidepanel-toggle 'test-main)
      (check "toggle hid it" (sidepanel-visible-p 'test-main) nil)

      (with-current-buffer aside
        (sidepanel-setup-buffer :undo t)
        (check "mode line gone" mode-line-format nil)
        (check "cursor gone" cursor-type nil)
        (check "lines truncated" truncate-lines t)
        (check "undo switched back on" buffer-undo-list nil))

      (dolist (buffer (list main aside)) (kill-buffer buffer))
      (if (zerop failures)
          (princ "sidepanel: all checks passed\n")
        (error "sidepanel: %d check(s) failed" failures)))))

(provide 'sidepanel)
;;; sidepanel.el ends here
