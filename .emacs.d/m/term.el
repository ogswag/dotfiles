;;; term.el --- Integrated terminal panel -*- lexical-binding: t; -*-

;;; Commentary:
;; bottom terminal panel, multi-session. backends noted in doc/terminals.org.

;;; Code:

(require 'cl-lib)

;; m/p is not on `load-path' by default.
(eval-and-compile
  (add-to-list 'load-path
               (or (bound-and-true-p my-vendor-directory)
                   (expand-file-name "m/p" user-emacs-directory))))
(require 'sidepanel)

;;;; Forward declarations

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function term-mode "term" ())
(declare-function term-char-mode "term" ())
(declare-function term-send-raw-string "term" (chars))
(declare-function term-ansi-make-term "term"
                  (name program &optional startfile &rest switches))
(declare-function eshell-mode "esh-mode" ())
(declare-function shell "shell" (&optional buffer file-name))
(declare-function vterm-mode "vterm" ())
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile switches))
(declare-function eat-term-parameter "eat" (terminal parameter))
(declare-function mistty-create "mistty"
                  (&optional command other-window accept-shell-command))
(declare-function ghostel "ghostel" (&optional arg))
(declare-function corfu-mode "corfu" (&optional arg))
(declare-function ligature-mode "ligature" (&optional arg))

(defvar term-raw-map)
(defvar eshell-process-list)
(defvar eat-terminal)
(defvar ghostel-buffer-name)
(defvar ghostel--command-running)
(defvar my/tab-exempt-regexps)

;;;; State

(defconst my/term--list-buffer-name " *term sessions*"
  "Name of the session-list buffer.")

;;;; Options

(defgroup my/term nil
  "A VS Code-style terminal panel docked at the bottom of the frame."
  :group 'convenience
  :prefix "my/term-")

(defcustom my/term-backend 'term
  "Backend used for new terminal sessions."
  :type '(choice (const :tag "term (built-in VT100 emulator)" term)
                 (const :tag "eshell (built-in Lisp shell)" eshell)
                 (const :tag "shell (built-in comint)" shell)
                 (const :tag "vterm (C module, needs cmake)" vterm)
                 (const :tag "ghostel (libghostty module, no build)" ghostel)
                 (const :tag "eat (Lisp emulator)" eat)
                 (const :tag "mistty (term.el hybrid)" mistty)))

(defcustom my/term-panel-height 0.3
  "Height of the panel: a line count, or a fraction of the frame if a float."
  :type '(choice (integer :tag "Lines") (float :tag "Fraction of frame")))

(defcustom my/term-list-width 24
  "Columns for the session list beside the terminal."
  :type 'integer)

(defcustom my/term-list-side 'right
  "Which edge of the panel the session list sits on."
  :type '(choice (const :tag "Right (VS Code)" right) (const :tag "Left" left)))

(defcustom my/term-list-threshold 0
  "Show the session list once more than this many sessions exist.

Zero, so a single terminal is listed too: the list is how the panel says which
terminal is on screen and what else is running, and that is worth having from
the first one.  Set it to 1 for the old behaviour, where one terminal got the
whole panel to itself."
  :type 'integer)

(defcustom my/term-list-show-index t
  "Whether session lines are numbered, as in \"1: zsh\"."
  :type 'boolean)

(defcustom my/term-list-close-on-hover nil
  "Whether the close button stays hidden until the pointer is over it.

Off, because the tabs the rows are drawn to match always show theirs.  Turning
it on also drops the list back to a text glyph for the button: hiding works by
painting the glyph in the colour behind it, and `tab-bar-close-button' is an
image in a graphical frame, which no foreground colour can hide."
  :type 'boolean)

(defcustom my/term-list-line-spacing 3
  "Pixels between session rows, so the boxed rows do not touch."
  :type 'natnum)

(defcustom my/term-project-prompt 'ask-once
  "Where a new terminal opens when the current buffer is inside a project."
  :type '(choice (const :tag "Ask once per project" ask-once)
                 (const :tag "Ask every time" ask)
                 (const :tag "Always the project root" root)
                 (const :tag "Always the file's directory" file)))

(defcustom my/term-confirm-kill t
  "Ask before closing a terminal that is running something."
  :type 'boolean)

(defcustom my/term-close-panel-on-last-session t
  "Whether closing the last session hides the panel, as in VS Code."
  :type 'boolean)

;;;; Faces
(defface my/term-list-name '((t :inherit tab-bar-tab-inactive :extend t))
  "Session line in the terminal list, drawn as an inactive tab.")

(defface my/term-list-current '((t :inherit tab-bar-tab :extend t))
  "The session line the panel is currently showing, drawn as the selected tab.")

(defface my/term-list-hover '((t :inherit tab-bar-tab-highlight :extend t))
  "Session line under the pointer.")

(defface my/term-list-strip '((t :inherit tab-bar :extend t))
  "The gutters either side of a row, and the rest of the line past it.
The strip the tabs sit in, so the rows read as tabs on it rather than as
bands of colour running edge to edge.")

(defface my/term-list-close '((t :inherit shadow))
  "The close button at rest, on an ordinary row.
With `my/term-list-close-on-hover' set its foreground is overwritten with
the frame background by `my/term--refresh-faces', hiding it until hover.")

(defface my/term-list-close-current '((t :inherit shadow))
  "The close button at rest, on the current row.
A second face is needed because hiding works by matching the background the
glyph sits on, and the current row has one of its own -- painted with the
frame background it would show up as a dark mark on that row alone.")

(defface my/term-list-busy '((t :inherit warning))
  "Marker on a session that is running something.")

(defun my/term--refresh-faces (&rest _)
  "Keep the close faces in step with the theme in force.

On `enable-theme-functions' at a positive depth, so it runs *after*
`my/tab-bar--refresh-faces': the backgrounds read below are the ones that
function has just derived, and prepending this one would read the old theme's."
  (if my/term-list-close-on-hover
      (progn
        (set-face-attribute 'my/term-list-close nil
                            :foreground (face-attribute 'default :background nil t))
        (set-face-attribute 'my/term-list-close-current nil
                            :foreground (face-attribute 'my/term-list-current
                                                        :background nil t)))
    (set-face-attribute 'my/term-list-close nil :foreground 'unspecified)
    (set-face-attribute 'my/term-list-close-current nil :foreground 'unspecified)))

;;;; Backends

(defvar my/term-backends
  '((term   :label "term"   :package nil    :raw t
            :launch my/term--launch-term   :busy my/term--busy-process)
    (eshell :label "eshell" :package nil    :raw nil
            :launch my/term--launch-eshell :busy my/term--busy-eshell)
    (shell  :label "shell"  :package nil    :raw nil
            :launch my/term--launch-shell  :busy my/term--busy-process)
    (vterm  :label "vterm"  :package vterm  :raw t
            :launch my/term--launch-vterm  :busy my/term--busy-process)
    (ghostel :label "ghostel" :package ghostel :raw t
             :launch my/term--launch-ghostel :busy my/term--busy-ghostel)
    (eat    :label "eat"    :package eat    :raw t
            :launch my/term--launch-eat    :busy my/term--busy-eat)
    (mistty :label "mistty" :package mistty :raw t
            :launch my/term--launch-mistty :busy my/term--busy-process))
  "Terminal backends, in the order \\[my/term-set-backend] offers them.")

(defun my/term--spec (&optional backend)
  "Return the plist for BACKEND, loading its package first."
  (let* ((backend (or backend my/term-backend))
         (spec (alist-get backend my/term-backends)))
    (unless spec
      (user-error "Unknown terminal backend `%s'" backend))
    (when-let* ((feature (plist-get spec :package)))
      (unless (require feature nil t)
        (user-error "Backend `%s' needs the `%s' package (M-x package-install)"
                    backend feature)))
    spec))

(defun my/term-available-backends ()
  "Backends whose package is built in or installed."
  (seq-filter (lambda (entry)
                (let ((feature (plist-get (cdr entry) :package)))
                  (or (null feature)
                      (featurep feature)
                      (locate-library (symbol-name feature)))))
              my/term-backends))

(defun my/term--program ()
  "The shell to run.  m/edit.el already points `shell-file-name' at zsh."
  (or (bound-and-true-p explicit-shell-file-name)
      (getenv "ESHELL")
      shell-file-name))

(defmacro my/term--without-display (&rest body)
  "Evaluate BODY with `display-buffer' turned off."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-alist
          '((".*" (display-buffer-no-window) (allow-no-window . t)))))
     ,@body))

;;;;; Launchers

(defun my/term--launch-term (name dir)
  "Start a `term' session called NAME in DIR."
  (require 'term)
  (let* ((default-directory dir)
         (program (split-string-shell-command (my/term--program)))
         ;; `term-ansi-make-term' is the entry point that takes a buffer name and does not display.
         (buffer (get-buffer (apply #'term-ansi-make-term
                                    (generate-new-buffer-name
                                     (my/term--buffer-name name))
                                    (car program) nil (cdr program)))))
    (with-current-buffer buffer
      (term-mode)
      (term-char-mode))
    buffer))

(defun my/term--launch-eshell (name dir)
  "Start an Eshell session called NAME in DIR."
  (require 'eshell)
  (let* ((default-directory dir)
         (buffer (generate-new-buffer (my/term--buffer-name name))))
    (with-current-buffer buffer (eshell-mode))
    buffer))

(defun my/term--launch-shell (name dir)
  "Start a comint `shell' session called NAME in DIR."
  (require 'shell)
  (let* ((default-directory dir)
         (buffer (generate-new-buffer (my/term--buffer-name name))))
    (my/term--without-display (shell buffer))
    buffer))

(defun my/term--launch-vterm (name dir)
  "Start a vterm session called NAME in DIR."
  (let* ((default-directory dir)
         (buffer (generate-new-buffer (my/term--buffer-name name))))
    (with-current-buffer buffer (vterm-mode))
    buffer))

(defun my/term--launch-ghostel (name dir)
  "Start a ghostel session called NAME in DIR."
  (let* ((default-directory dir)
         (ghostel-buffer-name (my/term--buffer-name name)))
    ;; It pops the buffer up on the way; the panel owns display.
    (my/term--without-display (ghostel '(4)))))

(defun my/term--launch-eat (name dir)
  "Start an eat session called NAME in DIR."
  (let* ((default-directory dir)
         (buffer (generate-new-buffer (my/term--buffer-name name))))
    (with-current-buffer buffer
      (eat-mode)
      (eat-exec buffer name (my/term--program) nil nil))
    buffer))

(defun my/term--launch-mistty (name dir)
  "Start a mistty session called NAME in DIR."
  (let ((default-directory dir))
    (let ((buffer (my/term--without-display
                    (mistty-create (my/term--program) nil t))))
      (with-current-buffer buffer
        (rename-buffer (my/term--buffer-name name) t))
      buffer)))

;;;; Sessions

(cl-defstruct (my/term-session (:constructor my/term--session-make)
                               (:copier nil))
  "One terminal running in the panel."
  buffer                                ; the live buffer; the truth about liveness
  name                                  ; display name, bare: "zsh", "zsh (2)"
  backend                               ; key into `my/term-backends'
  directory)                            ; where it was started

(defvar-local my/term--session nil
  "The `my/term-session' this buffer belongs to, or nil. Buffer-local, so that `kill-buffer-query-functions' -- which runs with the doomed buffer current -- can find it without a search, and so that the back-reference dies with the buffer.")

(defvar my/term--sessions nil
  "Live sessions, oldest first.  This list is the order the list window draws.")

(defvar my/term--current nil
  "The session the panel is showing.")

(defun my/term--buffer-name (name)
  "Buffer name for a session called NAME."
  (format "*term: %s*" name))

(defun my/term--gc ()
  "Drop sessions whose buffer has died, and repair `my/term--current'."
  (let ((live (seq-filter (lambda (s) (buffer-live-p (my/term-session-buffer s)))
                          my/term--sessions)))
    (unless (= (length live) (length my/term--sessions))
      (setq my/term--sessions live)))
  (unless (memq my/term--current my/term--sessions)
    (setq my/term--current (car my/term--sessions))))

(defun my/term--default-name (backend)
  "The name a new BACKEND session starts with, before uniquifying."
  (if (eq backend 'eshell)
      "eshell"
    (file-name-nondirectory
     (car (split-string-shell-command (my/term--program))))))

(defun my/term--unique-name (base &optional except)
  "BASE, or \"BASE (2)\", \"BASE (3)\"..."
  (let ((taken (mapcar #'my/term-session-name
                       (remq except my/term--sessions))))
    (if (not (member base taken))
        base
      (let ((n 2))
        (while (member (format "%s (%d)" base n) taken)
          (setq n (1+ n)))
        (format "%s (%d)" base n)))))

(defun my/term--adopt (buffer session)
  "Tie BUFFER to SESSION and make it fit to live in the panel."
  (setf (my/term-session-buffer session) buffer)
  (with-current-buffer buffer
    (setq my/term--session session)
    (rename-buffer (my/term--buffer-name (my/term-session-name session)) t)
    (setq-local mode-line-format nil)
    ;; Both of these are global minor modes and both are wrong here.
    (when (plist-get (my/term--spec (my/term-session-backend session)) :raw)
      (when (bound-and-true-p corfu-mode) (corfu-mode -1))
      (when (bound-and-true-p ligature-mode) (ligature-mode -1)))
    (add-hook 'kill-buffer-hook #'my/term--forget-session nil t))
  session)

(defun my/term--make-session (backend directory)
  "Create and register a session running BACKEND in DIRECTORY."
  (let* ((spec (my/term--spec backend))
         (name (my/term--unique-name (my/term--default-name backend)))
         (session (my/term--session-make :name name :backend backend
                                         :directory directory))
         (buffer (funcall (plist-get spec :launch) name directory)))
    (my/term--adopt buffer session)
    (setq my/term--sessions (append my/term--sessions (list session)))
    session))

(defun my/term--neighbour (session)
  "The session to fall back on when SESSION goes away."
  (let ((rest (cdr (memq session my/term--sessions))))
    (or (car rest)
        (car (last (remq session my/term--sessions))))))

(defun my/term--forget-session ()
  "Retire the session whose buffer is being killed.  On `kill-buffer-hook'."
  (when-let* ((session my/term--session))
    (let ((next (my/term--neighbour session)))
      (setq my/term--sessions (remq session my/term--sessions))
      (when (eq my/term--current session)
        (setq my/term--current next)))
    (run-at-time 0 nil #'my/term--after-close)))

(defun my/term--after-close ()
  "Rebuild or hide the panel once a killed session's window is gone."
  (my/term--gc)
  (cond
   (my/term--sessions (when (sidepanel-wanted-p 'term) (my/term--show-panel)))
   (my/term-close-panel-on-last-session (my/term-hide))
   ;; Off the frame but not dismissed: nothing to show right now.
   (t (sidepanel-conceal 'term))))

;;;; Where a new terminal opens

(defvar my/term--project-answers (make-hash-table :test #'equal)
  "Project root -> whether to open at the root.  For `ask-once'.")

(defun my/term--directory (&optional force-ask)
  "The directory a new terminal should start in."
  (let* ((file (or buffer-file-name
                   (and (derived-mode-p 'dired-mode) default-directory)))
         (dir (and file (file-name-directory (expand-file-name file))))
         ;; Ask about DIR rather than letting `project-current' fall back on `default-directory'.
         (root (and dir (require 'project nil t)
                    (when-let* ((project (project-current nil dir)))
                      (expand-file-name (project-root project))))))
    (file-name-as-directory
     (cond
      ((null dir) (expand-file-name "~"))
      ((null root) dir)
      ((eq my/term-project-prompt 'root) root)
      ((eq my/term-project-prompt 'file) dir)
      (t
       (let ((known (if (and (eq my/term-project-prompt 'ask-once)
                             (not force-ask))
                        (gethash root my/term--project-answers 'miss)
                      'miss)))
         (if (not (eq known 'miss))
             (if known root dir)
           (let ((answer (y-or-n-p
                          (format "Project detected at %s, open terminal at project root? "
                                  (abbreviate-file-name root)))))
             (when (eq my/term-project-prompt 'ask-once)
               (puthash root answer my/term--project-answers))
             (if answer root dir)))))))))

;;;; The panel

(defvar my/term--saved-height nil
  "Panel height in lines as last seen, so a hand-dragged size survives.")

(defun my/term--height ()
  "Height to ask for: whatever it was last, else `my/term-panel-height'."
  (or my/term--saved-height my/term-panel-height))

(defun my/term--remember-height (&rest _)
  "Record the panel's current height for the next time it is shown."
  (when-let* ((window (sidepanel-window 'term)))
    (setq my/term--saved-height (window-total-height window))))

(defun my/term-visible-p ()
  "Non-nil when the panel is actually on screen."
  (sidepanel-visible-p 'term))

(defun my/term--session-buffer-p (buffer)
  "Non-nil when BUFFER is a terminal session's."
  (and (buffer-local-value 'my/term--session buffer) t))

(defun my/term--list-buffer-p (buffer)
  "Non-nil when BUFFER is the session list's."
  (eq buffer (get-buffer my/term--list-buffer-name)))

(defun my/term--panel-buffer-p (buffer &optional _action)
  "Non-nil when BUFFER belongs to the panel."
  (when-let* ((buffer (get-buffer buffer)))
    (or (my/term--session-buffer-p buffer)
        (my/term--list-buffer-p buffer))))

(defun my/term--panel-buffer ()
  "The buffer of the session the panel should be showing, or nil."
  (when-let* ((session my/term--current)
              (buffer (my/term-session-buffer session))
              ((buffer-live-p buffer)))
    buffer))

(defun my/term--list-wanted-p ()
  "Non-nil when there are enough sessions to be worth listing."
  (> (length my/term--sessions) my/term-list-threshold))

;; Two panels in one group, so they open and close together.
(sidepanel-define 'term
  :buffer-function #'my/term--panel-buffer
  :owner-p #'my/term--session-buffer-p
  :side 'bottom :slot 0 :group 'term
  :size #'my/term--height
  ;; Furniture: C-x o should never land here.
  :on-hide #'my/term--remember-height)

(sidepanel-define 'term-list
  :buffer-function #'my/term--list-buffer
  :owner-p #'my/term--list-buffer-p
  :side 'bottom :group 'term
  ;; Furniture: C-x o should never land here.
  :slot (lambda () (if (eq my/term-list-side 'left) -1 1))
  :size (lambda () my/term-list-width)
  :fixed 'width
  :predicate #'my/term--list-wanted-p)

(defun my/term--show-panel (&optional select)
  "Show the panel for `my/term--current'.  SELECT focuses the terminal."
  (my/term--gc)
  (when (my/term--panel-buffer)
    (prog1 (sidepanel-show 'term select)
      ;; No fringes, no margins.
      (my/term--list-refresh))))

;;;; The session list

(defvar my/term--menu-session nil
  "Session the context menu was last built for.")

(defconst my/term--close-glyph
  (if (char-displayable-p ?\N{U+2715}) "\N{U+2715}" "x")
  "Text fallback for the close button, used when it hides until hover.")

(defun my/term--close-button ()
  "The close button to draw on a row, as a string.

`tab-bar-close-button' where it can be: the rows are drawn as tabs, and this is
the very button the tabs carry -- m/tabs.el re-cuts it to the line height on
every theme change, so the list gets that for nothing.  Its `close-tab' property
is meaningless off the tab bar and comes off; the keymap that replaces it goes
on in `my/term--list-insert'."
  (if (or my/term-list-close-on-hover
          (not (bound-and-true-p tab-bar-close-button)))
      my/term--close-glyph
    (let ((button (copy-sequence tab-bar-close-button)))
      (remove-text-properties 0 (length button) '(close-tab nil) button)
      button)))

(defconst my/term--ellipsis
  (if (char-displayable-p ?\N{U+2026}) "\N{U+2026}" "...")
  "Marker for a session name too long to fit the list.")

(defun my/term--list-width ()
  "Columns available inside the session list."
  (if-let* ((window (sidepanel-window 'term-list)))
      (window-body-width window)
    my/term-list-width))

(defvar-keymap my/term-list-line-map
  :doc "Keymap on the name part of a session line."
  "<mouse-1>" #'my/term-list-mouse-select)

(defvar-keymap my/term-list-close-map
  :doc "Keymap on the close button.
A separate map on a separate span: mouse events consult the `keymap' text
property where the click landed, not where point is, so one line can mean
two things."
  "<mouse-1>" #'my/term-list-mouse-close)

(defvar-keymap my/term-list-mode-map
  :doc "Keymap for `my/term-list-mode'."
  "RET" #'my/term-list-select
  "SPC" #'my/term-list-select
  "n" #'next-line
  "p" #'previous-line
  "d" #'my/term-list-close-session
  "k" #'my/term-list-close-session
  "r" #'my/term-rename
  "c" #'my/term-new
  "+" #'my/term-new
  "g" #'my/term--list-refresh
  "q" #'my/term-hide)

(define-derived-mode my/term-list-mode special-mode "Terminals"
  "Session list for the terminal panel."
  ;; Furniture: no mode line, no cursor, no fringes or margins.
  (sidepanel-setup-buffer)
  ;; Room to breathe between the rows, which are drawn as boxed tabs.
  (setq-local line-spacing my/term-list-line-spacing))

(defun my/term--list-buffer ()
  "The session-list buffer, created if need be."
  (or (get-buffer my/term--list-buffer-name)
      (with-current-buffer (get-buffer-create my/term--list-buffer-name)
        (my/term-list-mode)
        (current-buffer))))

(defun my/term--session-at (position)
  "The session named on the line at POSITION, or nil."
  (get-text-property position 'my/term-session))

(defun my/term--line-context-menu (menu click)
  "Add the session commands to MENU for the line CLICK landed on."
  (setq my/term--menu-session (my/term--session-at (posn-point (event-start click))))
  (when my/term--menu-session
    (define-key-after menu [my/term-rename]
      '(menu-item "Rename Terminal..." my/term-menu-rename))
    (define-key-after menu [my/term-duplicate]
      '(menu-item "New Terminal Here" my/term-menu-duplicate))
    (define-key-after menu [my/term-separator] menu-bar-separator)
    (define-key-after menu [my/term-close]
      '(menu-item "Close Terminal" my/term-menu-close)))
  menu)

(defun my/term--list-insert (session index)
  "Draw one line for SESSION, numbered INDEX, as a tab on a strip.

The line is a one-column gutter, the row itself, and a one-column gutter, the
gutters and the newline in `my/term-list-strip' so the row reads as a tab
sitting on the bar rather than as a band of colour running edge to edge.  The
row's own face carries the raised box it inherits from the tab faces."
  (let* ((current (eq session my/term--current))
         (row-face (if current 'my/term-list-current 'my/term-list-name))
         (button (propertize (my/term--close-button)
                             'face (list (if current
                                             'my/term-list-close-current
                                           'my/term-list-close)
                                         row-face)))
         ;; The buffer-local half of the fringe.
         (char-width (max 1 (frame-char-width)))
         (button-pixels (string-pixel-width button))
         (button-columns (max 1 (ceiling button-pixels char-width)))
         ;; A row is: two gutters + marker (2) + label + a gap of at least one + the button.
         (room (max 4 (- (my/term--list-width) 5 button-columns)))
         (label (truncate-string-to-width
                 (if my/term-list-show-index
                     (format "%d: %s" index (my/term-session-name session))
                   (my/term-session-name session))
                 room 0 nil my/term--ellipsis))
         (gutter (propertize " " 'face 'my/term-list-strip))
         (start (point))
         row-start button-start)
    (insert gutter)
    (setq row-start (point))
    (insert (propertize (concat (if current "\N{U+25B8} " "  ") label) 'face row-face))
    ;; Pad the row out to where the button starts.
    (insert (propertize " " 'display
                        `(space :align-to (- right (,(+ button-pixels char-width))))
                        'face row-face))
    (setq button-start (point))
    (insert button)
    (insert gutter)
    ;; The newline carries the strip face.
    (insert (propertize "\n" 'face 'my/term-list-strip))
    ;; The buffer-local half of the fringe.
    (add-text-properties
     start (1- (point))
     `( my/term-session ,session
        context-menu-function my/term--line-context-menu
        keymap ,my/term-list-line-map
        help-echo ,(format "%s  (%s in %s)\nmouse-1: select   mouse-3: menu"
                           (my/term-session-name session)
                           (my/term-session-backend session)
                           (abbreviate-file-name (my/term-session-directory session)))))
    ;; The buffer-local half of the fringe.
    (put-text-property row-start (1- (point)) 'mouse-face 'my/term-list-hover)
    ;; The button takes its own keymap and tooltip back.
    (add-text-properties button-start (+ button-start (length button))
                         `( keymap ,my/term-list-close-map
                            help-echo "mouse-1: close this terminal"))))

(defun my/term--list-refresh ()
  "Redraw the session list."
  (interactive)
  (when-let* ((buffer (get-buffer my/term--list-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (seq-do-indexed (lambda (session index)
                          (my/term--list-insert session (1+ index)))
                        my/term--sessions)
        (goto-char (point-min))
        (forward-line (1- line))))))

;;;;; Clicking

(defun my/term-list-mouse-select (event)
  "Show the session on the line EVENT landed on."
  (interactive "e")
  (when-let* ((session (my/term--session-at (posn-point (event-start event)))))
    (my/term-select session)))

(defun my/term-list-mouse-close (event)
  "Close the session whose close button EVENT landed on."
  (interactive "e")
  (when-let* ((session (my/term--session-at (posn-point (event-start event)))))
    (my/term-close session)))

(defun my/term-list-select ()
  "Show the session named on the current line."
  (interactive)
  (if-let* ((session (my/term--session-at (line-beginning-position))))
      (my/term-select session)
    (user-error "No terminal on this line")))

(defun my/term-list-close-session ()
  "Close the session named on the current line."
  (interactive)
  (if-let* ((session (my/term--session-at (line-beginning-position))))
      (my/term-close session)
    (user-error "No terminal on this line")))

(defun my/term-menu-rename ()
  "Rename the session the context menu was opened on."
  (interactive)
  (when my/term--menu-session
    (call-interactively
     (lambda (name)
       (interactive (list (read-string "Terminal name: "
                                       (my/term-session-name my/term--menu-session))))
       (my/term-rename my/term--menu-session name)))))

(defun my/term-menu-close ()
  "Close the session the context menu was opened on."
  (interactive)
  (when my/term--menu-session (my/term-close my/term--menu-session)))

(defun my/term-menu-duplicate ()
  "Open a new terminal in the same directory as the menu's session."
  (interactive)
  (when my/term--menu-session
    (my/term--open (my/term-session-directory my/term--menu-session) my/term-backend)))

;;;; Is it running something

(defvar my/term--name-the-command t
  "Whether `my/term--busy' may spend a subprocess naming what is running.")

(defun my/term--child-command (pgid)
  "Best-effort name of the process group PGID, or nil."
  (when (and my/term--name-the-command (executable-find "ps"))
    (ignore-errors
      (car (last (split-string
                  (shell-command-to-string (format "ps -o comm= -g %d 2>/dev/null" pgid))
                  "\n" t))))))

(defun my/term--busy-process (buffer)
  "What BUFFER's process has handed its terminal to, or nil."
  (when-let* ((process (get-buffer-process buffer))
              ((memq (process-status process)
                     '(run stop open listen connect)))
              (child (process-running-child-p process)))
    (cond ((integerp child)
           (or (when-let* ((name (my/term--child-command child)))
                 (file-name-nondirectory name))
               "a command"))
          ((eq child t) nil)
          (t "a command"))))

(defun my/term--busy-eshell (buffer)
  "What Eshell is running in BUFFER, or nil."
  (when (buffer-local-value 'eshell-process-list buffer) "a command"))

(defun my/term--busy-ghostel (buffer)
  "What ghostel is running in BUFFER, or nil."
  (when (buffer-local-value 'ghostel--command-running buffer) "a command"))

(defun my/term--busy-eat (buffer)
  "What eat is running in BUFFER, or nil."
  (with-current-buffer buffer
    (when-let* ((terminal (bound-and-true-p eat-terminal))
                (process (eat-term-parameter terminal 'eat--process))
                ((process-live-p process)))
      (my/term--busy-process buffer))))

(defun my/term--busy (session)
  "What SESSION is running, or nil."
  (let ((buffer (my/term-session-buffer session)))
    (when (buffer-live-p buffer)
      (funcall (plist-get (my/term--spec (my/term-session-backend session)) :busy)
               buffer))))

(defun my/term--kill-buffer-query ()
  "Confirm before a busy terminal is killed.  On `kill-buffer-query-functions'."
  (or (null my/term--session)
      (not my/term-confirm-kill)
      (let ((busy (ignore-errors (my/term--busy my/term--session))))
        (or (null busy)
            (yes-or-no-p (format "%s is running %s; close it anyway? "
                                 (my/term-session-name my/term--session) busy))))))

;;;; Commands

(defun my/term--open (directory backend)
  "Open a new session running BACKEND in DIRECTORY and show it."
  (setq my/term--current (my/term--make-session backend directory))
  (my/term--show-panel 'select)
  my/term--current)

(defun my/term-new (&optional force-ask)
  "Open a new terminal."
  (interactive "P")
  (when (minibufferp)
    (user-error "Not from the minibuffer"))
  (my/term--open (my/term--directory force-ask) my/term-backend))

(defun my/term-new-here ()
  "Open a new terminal in `default-directory', without asking anything."
  (interactive)
  (my/term--open (expand-file-name default-directory) my/term-backend))

(defun my/term-select (session)
  "Show SESSION in the panel and focus it."
  (interactive (list (my/term--read-session "Terminal: ")))
  (setq my/term--current session)
  (my/term--show-panel 'select))

(defun my/term--read-session (prompt)
  "Read one of the live sessions, with PROMPT."
  (my/term--gc)
  (unless my/term--sessions (user-error "No terminals are open"))
  (let* ((names (mapcar (lambda (s) (cons (my/term-session-name s) s))
                        my/term--sessions))
         (choice (completing-read prompt names nil t)))
    (cdr (assoc choice names))))

(defun my/term-close (&optional session)
  "Close SESSION, or the one the panel is showing."
  (interactive)
  (let ((session (or session my/term--current)))
    (unless session (user-error "No terminals are open"))
    (let ((buffer (my/term-session-buffer session)))
      (if (buffer-live-p buffer)
          ;; Ours is the only question worth asking.
          (let ((kill-buffer-query-functions (list #'my/term--kill-buffer-query)))
            (kill-buffer buffer))
        (setq my/term--sessions (remq session my/term--sessions))
        (my/term--after-close)))))

(defun my/term-rename (session new-name)
  "Rename SESSION to NEW-NAME."
  (interactive
   (let ((session (or (and (derived-mode-p 'my/term-list-mode)
                           (my/term--session-at (line-beginning-position)))
                      my/term--session
                      my/term--current)))
     (unless session (user-error "No terminal to rename"))
     (list session (read-string "Terminal name: " (my/term-session-name session)))))
  (let ((name (string-trim new-name)))
    (when (string-empty-p name) (user-error "The name cannot be empty"))
    (setf (my/term-session-name session) (my/term--unique-name name session))
    (when (buffer-live-p (my/term-session-buffer session))
      (with-current-buffer (my/term-session-buffer session)
        (rename-buffer (my/term--buffer-name (my/term-session-name session)) t)))
    (my/term--list-refresh)))

(defun my/term-next (&optional n)
  "Show the Nth next session, wrapping around.  N defaults to 1."
  (interactive "p")
  (my/term--gc)
  (unless my/term--sessions (user-error "No terminals are open"))
  (let* ((count (length my/term--sessions))
         (at (or (seq-position my/term--sessions my/term--current) 0)))
    (my/term-select (nth (mod (+ at (or n 1)) count) my/term--sessions))))

(defun my/term-prev (&optional n)
  "Show the Nth previous session, wrapping around.  N defaults to 1."
  (interactive "p")
  (my/term-next (- (or n 1))))

(defun my/term-focus-list ()
  "Move point into the session list."
  (interactive)
  (unless (sidepanel-wanted-p 'term) (my/term-toggle))
  (if-let* ((window (sidepanel-window 'term-list)))
      (select-window window)
    (user-error "The session list is hidden; raise `my/term-list-threshold'")))

(defun my/term-hide ()
  "Hide the panel, leaving every session running."
  (interactive)
  (sidepanel-hide 'term))

(defun my/term-toggle ()
  "Show the terminal panel, or hide it if it is already up."
  (interactive)
  (my/term--gc)
  (cond
   ((and (my/term-visible-p)
         (or (eq (selected-window) (sidepanel-window 'term))
             (mouse-event-p last-command-event)))
    (my/term-hide))
   ((my/term-visible-p) (select-window (sidepanel-window 'term)))
   (my/term--sessions (my/term--show-panel 'select))
   (t (my/term-new))))

(defun my/term-set-backend (backend)
  "Set `my/term-backend' to BACKEND for terminals opened from now on."
  (interactive
   (let ((names (mapcar (lambda (entry)
                          (cons (plist-get (cdr entry) :label) (car entry)))
                        (my/term-available-backends))))
     (list (cdr (assoc (completing-read
                        (format "Backend (now %s): " my/term-backend) names nil t)
                       names)))))
  (my/term--spec backend)
  (setq my/term-backend backend)
  (message "New terminals will use %s" backend))

(defun my/term-cd (directory)
  "Point the current session at DIRECTORY."
  (interactive "DDirectory: ")
  (unless my/term--current (user-error "No terminals are open"))
  (let ((buffer (my/term-session-buffer my/term--current))
        (directory (file-name-as-directory (expand-file-name directory))))
    (with-current-buffer buffer
      (setq default-directory directory)
      (when-let* ((process (get-buffer-process buffer)))
        (process-send-string process (format " cd %s\n" (shell-quote-argument directory)))))
    (setf (my/term-session-directory my/term--current) directory)
    (my/term--list-refresh)))

(defun my/term-send-escape ()
  "Send a literal ESC to the terminal."
  (interactive)
  (term-send-raw-string "\e"))

;;;; Setup

(setq window-sides-vertical t)

(when (boundp 'my/tab-exempt-regexps)
  (add-to-list 'my/tab-exempt-regexps "\\`\\*term: "))

(add-hook 'kill-buffer-query-functions #'my/term--kill-buffer-query)
(add-hook 'enable-theme-functions #'my/term--refresh-faces 90)
(my/term--refresh-faces)

;; Char-mode backends swallow ASCII, so the panel's own keys have to be taken
(with-eval-after-load 'term
  (keymap-set term-raw-map "<escape>" #'my/term-send-escape)
  (keymap-set term-raw-map "C-`" #'my/term-toggle))

;;;; Terminal Backends
(use-package vterm :ensure t :defer t)

(use-package ghostel :ensure t :defer t)

(setopt my/term-backend 'ghostel)


;;; term.el ends here
