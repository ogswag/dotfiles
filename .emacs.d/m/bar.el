;;; bar.el --- Frame tool bar -*- lexical-binding: t; -*-

;;; Commentary:
;; frame tool bar. run/build come from task.el.

;;; Code:

;;;; Forward declarations

(declare-function my/theme-rgb "theme" (colour))
(declare-function my/theme-hex "theme" (colour))
(declare-function my/theme-dark-p "theme" ())
(declare-function my/system-dark-p "theme" ())
(declare-function my/theme-shade "theme" (percent))
(declare-function my/speedbar-toggle "tree" ())
(declare-function my/term-toggle "term" ())
(declare-function my/term-visible-p "term" ())
(declare-function my/term-new "term" (&optional force-ask))
(declare-function my/task-run "task" (&optional pick))
(declare-function my/task-build "task" (&optional pick))
(declare-function my/task-stop "task" ())
(declare-function my/task-select-run "task" ())
(declare-function my/task-select-build "task" ())
(declare-function my/task-available-p "task" (kind))
(declare-function my/task-running-p "task" ())
(declare-function my/task-label "task" (kind))
(declare-function my/task-env-available-p "task" ())
(declare-function my/task-env-label "task" ())
(declare-function my/task-env-select "task" ())
(declare-function my/format-dwim "fmt" ())
(declare-function my/sar-toggle-case "sar" ())
(declare-function my/sar-toggle-whole-word "sar" ())
(declare-function my/sar-set-style "sar" (style))
(declare-function my/sar-style-label "sar" ())
(declare-function my/sar-open-in-panel "sar" ())
(declare-function my/tex-live-mode "l" (&optional arg))
(declare-function TeX-view "tex" ())
(declare-function TeX-clean "tex" (&optional arg))
(declare-function flymake-show-buffer-diagnostics "flymake" (&optional diagnostic))
(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-name "project" (project))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-octicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-faicon "nerd-icons" (icon-name &rest args))

(defvar my/nerd-mono-font)
(defvar my-config-directory)
(defvar my/task-state-change-hook)
(defvar speedbar-buffer)
(defvar isearch-tool-bar-map)
(defvar isearch-string)
(defvar isearch-success)

;;;; Icons

(defcustom my/tool-bar-icon-source 'emacs
  "Where the button icons come from: `emacs', `nerd', or `text'."
  :type '(choice (const :tag "SF Symbol SVGs in icons/" emacs)
                 (const :tag "Nerd Font glyphs, drawn into SVGs" nerd)
                 (const :tag "No images: a one-line text bar" text))
  :group 'my/task)

(defcustom my/tool-bar-icon-size 16
  "Pixel height every icon is drawn or scaled to, whatever it came as."
  :type 'natnum
  :group 'my/task)

(defcustom my/tool-bar-icon-directory
  (expand-file-name "icons" (or (bound-and-true-p my-config-directory)
                                user-emacs-directory))
  "Directory of button icon files."
  :type 'directory
  :group 'my/task)

(defcustom my/tool-bar-emacs-icons
  ;; Every button leads with its own name, so a file called after the button.
  '((sidebar     "sidebar_left")
    (terminal    "terminal")
    (terminal+   "terminal+" "terminal-new" "new")
    (run         "run" "gud/go")
    (run-menu    "run-menu")
    (build       "build")
    (build-menu  "build-menu")
    (stop        "stop" "gud/stop")
    (env         "env")
    (live        "live" "show")         ; an eye, for the live preview
    (pdf         "pdf")
    (clean       "clean" "delete")
    (save        "save")
    (format      "format")
    (diagnostics "diagnostics")
    (find        "find_file" "open")
    (search      "search_project")
    (project     "project" "diropen")

    (isearch-repeat-backward "isearch-repeat-backward" "isearch-backward" "left-arrow")
    (isearch-repeat-forward  "isearch-repeat-forward" "isearch-forward" "right-arrow")
    (isearch-cancel          "isearch-cancel" "isearch-abort" "close")
    (isearch-exit            "isearch-exit" "isearch-finish" "exit")
    (isearch-delete-char     "isearch-delete-char" "isearch-undo" "undo")
    (isearch-query-replace   "isearch-query-replace" "isearch-replace" "search-replace")
    (isearch-occur           "isearch-occur" "isearch-show-hits" "index")
    (isearch-describe-mode   "isearch-describe-mode" "isearch-help" "help")
    ;; m/sar.el's additions.
    (sar-case                "text_case")
    (sar-word                "whole_word")
    (sar-style               "paintbrush")
    (sar-panel               "sidebar_right")
    ;; The search panel draws its own buttons with these.  Each falls back to
    ;; an icon this directory already has, so they resolve without new artwork.
    (sar-find                "search_magnifier" "search")
    (sar-replace             "isearch-replace" "search-replace")
    (sar-undo                "isearch-undo" "undo")
    (sar-save                "save")
    (sar-revert              "sar-revert" "isearch-undo" "undo")
    ;; (sar-more                "show_detail")
    (sar-close               "close")
    (sar-file                "search_in_file")
    (sar-folder              "find_file")
    (sar-project             "search_project"))
  "Image file names for each button, best first, without directory or extension."
  :type '(alist :key-type symbol
                :value-type (choice string (repeat string)))
  :group 'my/task)

(defconst my/tool-bar-sf-symbols
  '(("sidebar_left"            . "sidebar.left")
    ("sidebar_right"           . "sidebar.right")
    ("terminal"                . "terminal")
    ("terminal+"               . "plus")
    ("run"                     . "play.fill")
    ("run-menu"                . "chevron.down")
    ("build"                   . "hammer.fill")
    ("build-menu"              . "chevron.down")
    ("stop"                    . "stop.fill")
    ("env"                     . "cube")
    ("live"                    . "eye")
    ("pdf"                     . "doc.richtext")
    ("clean"                   . "trash")
    ("save"                    . "square.and.arrow.down")
    ("format"                  . "text.alignleft")
    ("diagnostics"             . "exclamationmark.triangle")
    ("find_file"               . "doc.text.magnifyingglass")
    ("search_project"          . "magnifyingglass")
    ("project"                 . "folder")
    ("isearch-repeat-backward" . "chevron.backward")
    ("isearch-repeat-forward"  . "chevron.forward")
    ("isearch-cancel"          . "xmark")
    ("isearch-exit"            . "checkmark")
    ("isearch-undo"            . "arrow.uturn.backward")
    ("isearch-replace"         . "text.magnifyingglass")
    ("isearch-occur"           . "list.bullet.rectangle")
    ("isearch-help"            . "questionmark.circle")
    ("text_case"               . "textformat")
    ("whole_word"              . "textformat.abc")
    ("paintbrush"              . "paintbrush")
    ("close"                   . "xmark")
    ("search_magnifier"        . "magnifyingglass")
    ("search_in_file"          . "doc.text")
    ("sar-revert"              . "arrow.counterclockwise"))
  "SF Symbol for each file in `my/tool-bar-icon-directory', sans extension.
emacs-mac maps the image `:file' through `mac-icon-map-list' and draws these
as system symbols, which follow light/dark on their own.")

(defvar my/tool-bar-mac-icon-map nil
  "Alist for `mac-icon-map-list', keys as `mac-map-system-symbol' wants them.")

(defconst my/tool-bar-icons
  '((sidebar     . "nf-cod-layout_sidebar_left")
    (terminal    . "nf-cod-terminal")
    (terminal+   . "nf-cod-add")
    (run         . "nf-md-play")
    (run-menu    . "nf-md-chevron_down")
    (build       . "nf-md-hammer")
    (build-menu  . "nf-md-chevron_down")
    (stop        . "nf-md-stop")
    (env         . "nf-md-cube_outline")
    (live        . "nf-md-eye")
    (pdf         . "nf-md-file_pdf_box")
    (clean       . "nf-md-broom")
    (save        . "nf-md-content_save")
    (format      . "nf-md-format_align_left")
    (diagnostics . "nf-md-alert_circle_outline")
    (find        . "nf-md-file_find")
    (search      . "nf-md-magnify")
    (project     . "nf-md-folder_open")

    (isearch-repeat-backward . "nf-md-arrow_left")
    (isearch-repeat-forward  . "nf-md-arrow_right")
    (isearch-cancel          . "nf-md-close")
    (isearch-exit            . "nf-md-check")
    (isearch-delete-char     . "nf-md-undo")
    (isearch-query-replace   . "nf-md-find_replace")
    (isearch-occur           . "nf-md-format_list_bulleted")
    (isearch-describe-mode   . "nf-md-help_circle_outline")
    (sar-case                . "nf-md-format_letter_case")
    (sar-word                . "nf-md-format_letter_matches")
    (sar-style               . "nf-md-regex")
    (sar-panel               . "nf-md-dock_right")
    (sar-find                . "nf-md-magnify")
    (sar-replace             . "nf-md-find_replace")
    (sar-undo                . "nf-md-undo")
    (sar-save                . "nf-md-content_save")
    (sar-revert              . "nf-md-backup_restore")
    (sar-more                . "nf-md-chevron_down")
    (sar-close               . "nf-md-close")
    (sar-file                . "nf-md-file_outline")
    (sar-folder              . "nf-md-folder_outline")
    (sar-project             . "nf-md-source_repository"))
  "Nerd Font glyph names for the buttons, keyed by what they are for.")

(defconst my/tool-bar-pick-glyph "▾"
  "Marker on the buttons that open a menu.")

(defvar my/tool-bar--icons nil
  "`my/tool-bar-icons' with the names resolved to characters.")

(defvar my/tool-bar--images nil
  "Image descriptor per button, or nil when the bar carries no images.")

(defun my/tool-bar--images-p ()
  "Non-nil when the buttons can carry images."
  (and (display-graphic-p)
       (pcase my/tool-bar-icon-source
         ('emacs (seq-some #'image-type-available-p '(svg png xpm pbm)))
         ('nerd (image-type-available-p 'svg))
         (_ nil))))

(defun my/tool-bar--icon-font ()
  "The family the glyphs are drawn in."
  (or (bound-and-true-p nerd-icons-font-family)
      my/nerd-mono-font
      (face-attribute 'default :family nil t)))

(defun my/tool-bar--icon-colour ()
  "The colour to draw the glyphs in: one that reads on the button."
  (cond ((my/theme-dark-p) "#ffffff")
        ((my/theme-rgb (face-attribute 'default :background nil t)) "#000000")
        ((my/theme-hex (face-attribute 'tab-line :foreground nil t)))
        (t "black")))

(defun my/tool-bar--svg (glyph)
  "An image of GLYPH at `my/tool-bar-icon-size', or nil."
  (unless (string-empty-p glyph)
    (let ((size my/tool-bar-icon-size))
      (ignore-errors
        (create-image
         (format (concat "<svg xmlns=\"http://www.w3.org/2000/svg\""
                         " width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
                         "<text x=\"%s\" y=\"%s\" font-family=\"%s\""
                         " font-size=\"%s\" fill=\"%s\" text-anchor=\"middle\""
                         " dominant-baseline=\"central\">%s</text></svg>")
                 size size size size
                 (/ size 2.0) (/ size 2.0)
                 (my/tool-bar--icon-font)
                  (* size 0.9)
                   (my/tool-bar--icon-colour)
                   glyph)
          'svg t :ascent 'center)))))

(defun my/tool-bar--dark-icons-p ()
  "Non-nil when the dark icon variants should be used."
  (if (fboundp 'my/system-dark-p)
      (my/system-dark-p)
    (my/theme-dark-p)))

(defun my/tool-bar--stock (name)
  "The icon file called NAME, or nil when there is none."
  (when (stringp name)
    (let ((size my/tool-bar-icon-size)
          (try (if (my/tool-bar--dark-icons-p)
                   (list (concat name "-dark") name)
                 (list name))))
      (seq-some
       (lambda (n)
         (seq-some
          (lambda (ext)
            (let ((file (expand-file-name (concat n ext)
                                          my/tool-bar-icon-directory)))
              (when (file-readable-p file)
                (create-image file nil nil
                              :height size :ascent 'center))))
           '(".svg" ".png" ".xpm" ".pbm")))
        try))))

(defun my/tool-bar--icon-file (names)
  "The first of NAMES that answers to a file, or nil."
  (seq-some #'my/tool-bar--stock (if (listp names) names (list names))))

(defun my/tool-bar--resolve-icons ()
  "Resolve every button's icon, as a character and as an image."
  (setq my/tool-bar--icons
        (when (require 'nerd-icons nil t)
          (mapcar
           (lambda (cell)
             (let* ((name (cdr cell))
                    (fn (cond ((string-prefix-p "nf-md-" name) #'nerd-icons-mdicon)
                              ((string-prefix-p "nf-cod-" name) #'nerd-icons-codicon)
                              ((string-prefix-p "nf-oct-" name) #'nerd-icons-octicon)
                              (t #'nerd-icons-faicon))))
               (cons (car cell)
                     (or (ignore-errors
                           (substring-no-properties (funcall fn name)))
                         ""))))
           my/tool-bar-icons)))
  (setq my/tool-bar--images
        (when (my/tool-bar--images-p)
          (delq nil
                (if (eq my/tool-bar-icon-source 'emacs)
                    (mapcar (lambda (cell)
                              (when-let* ((image (my/tool-bar--icon-file (cdr cell))))
                                (cons (car cell) image)))
                            my/tool-bar-emacs-icons)
                  (mapcar (lambda (cell)
                            (when-let* ((image (my/tool-bar--svg (cdr cell))))
                              (cons (car cell) image)))
                          my/tool-bar--icons))))))

(defun my/tool-bar--mac-icon-key (name)
  "NAME as the last three path components of its file in the icon directory."
  (let ((sans (expand-file-name name my/tool-bar-icon-directory)))
    (and (string-match "/\\([^/]+/[^/]+/[^/]+$\\)" sans)
         (match-string 1 sans))))

(defun my/tool-bar--install-mac-icons ()
  "Teach emacs-mac to draw our tool-bar files as SF Symbols."
  (when (boundp 'mac-icon-map-list)
    (let (map)
      (dolist (cell my/tool-bar-sf-symbols)
        (let ((symbol (cdr cell)))
          (dolist (name (list (car cell) (concat (car cell) "-dark")))
            (when-let* ((key (my/tool-bar--mac-icon-key name)))
              (push (cons key symbol) map)))))
      (setq my/tool-bar-mac-icon-map (nreverse map)))
    (add-to-list 'mac-icon-map-list 'my/tool-bar-mac-icon-map)
    (when (and (boundp 'mac-system-symbol-cache)
               (hash-table-p mac-system-symbol-cache))
      (clrhash mac-system-symbol-cache))))

(defun my/tool-bar--icon (key)
  "The character for KEY, or the empty string."
  (or (alist-get key my/tool-bar--icons) ""))

(defun my/tool-bar--image (key)
  "The image for KEY, or nil."
  (alist-get key my/tool-bar--images))

;;;; Labels

(defun my/tool-bar--pad (string)
  "STRING with a space at each end."
  (concat " " string " "))

(defun my/tool-bar--label (key text &optional icon-only)
  "A button label for KEY reading TEXT."
  (let ((icon (my/tool-bar--icon key)))
    (my/tool-bar--pad
     (cond ((my/tool-bar--images-p) text)
           ((string-empty-p icon) text)
           (icon-only icon)
           (t (concat icon " " text))))))

(defun my/tool-bar-pick-label (kind)
  "Label for the picker button of KIND: what Run or Build would start."
  (my/tool-bar--pad
   (concat (or (my/task-label kind) "none") " " my/tool-bar-pick-glyph)))

(defun my/tool-bar-env-label ()
  "Label for the environment button: what this buffer's commands resolve in."
  (my/tool-bar--pad
   (concat (or (my/task-env-label) "env") " " my/tool-bar-pick-glyph)))

(defvar-local my/tool-bar--project nil
  "This buffer's project name, or nil.")

(defun my/tool-bar-project-label ()
  "Label for the project button: the project's name."
  (let ((name (or my/tool-bar--project "")))
    (my/tool-bar--pad
     (if (my/tool-bar--images-p)
         name
       (concat (my/tool-bar--icon 'project) " " name)))))

;;;; State the buttons show
;; Both of these are called from inside redisplay, so both are read-only and cheap.

(defun my/tool-bar--speedbar-on-p ()
  "Non-nil when the sidebar is up, docked or detached."
  (and (bound-and-true-p speedbar-buffer)
       (buffer-live-p speedbar-buffer)
       (get-buffer-window speedbar-buffer t)
       t))

(defun my/tool-bar--terminal-on-p ()
  "Non-nil when the terminal panel is on screen."
  (and (fboundp 'my/term-visible-p) (my/term-visible-p)))

;;;; The buttons

(defun my/tool-bar--item (map key name command &rest props)
  "Append a button to MAP under KEY."
  (define-key-after map (vector key)
    `(menu-item ,name ,command ,@props)))

(defun my/tool-bar--button (key text command icon-only &rest props)
  "A button for `my/tool-bar--groups', as (KEY NAME COMMAND ."
  (append (list key
                (if (stringp text) (my/tool-bar--label key text icon-only) text)
                command)
          (when-let* ((image (my/tool-bar--image key)))
            (if icon-only (list :vert-only t :image image) (list :image image)))
          props))

;;;; Commands a button needs of its own

(defun my/tool-bar-show-diagnostics ()
  "List this buffer's diagnostics, highlighting the one at point if there is one."
  (interactive)
  (flymake-show-buffer-diagnostics (car (flymake-diagnostics (point)))))

;;;; The button groups

(defun my/tool-bar--groups ()
  "The groups of buttons this buffer gets, in order, empty ones dropped."
  (delq
   nil
   (list
    ;; Panels
    (list (my/tool-bar--button 'sidebar "Sidebar" #'my/speedbar-toggle t
                               :help "Toggle the file sidebar"
                               :button '(:toggle . (my/tool-bar--speedbar-on-p)))
          (my/tool-bar--button 'terminal "Terminal" #'my/term-toggle t
                               :help "Toggle the terminal panel"
                               :button '(:toggle . (my/tool-bar--terminal-on-p)))
          (my/tool-bar--button 'terminal+ "New terminal" #'my/term-new t
                               :help "Open a new terminal"))

    ;; Run and build
    (let ((run (my/task-available-p 'run))
          (build (my/task-available-p 'build)))
      (when (or run build)
        (append
         (when run
           (list (my/tool-bar--button 'run "Run" #'my/task-run nil
                                      :help "Run this file")
                  (my/tool-bar--button 'run-menu '(my/tool-bar-pick-label 'run)
                                       #'my/task-select-run nil)))
          (when build
            (list (my/tool-bar--button 'build "Build" #'my/task-build t
                                       :help "Build this file")
                  (my/tool-bar--button 'build-menu '(my/tool-bar-pick-label 'build)
                                       #'my/task-select-build nil)))
         (list (my/tool-bar--button 'stop "Stop" #'my/task-stop t
                                    :help "Interrupt it; press again to kill it"
                                    :enable '(my/task-running-p))))))

    ;; What those commands resolve in.
    (when (my/task-env-available-p)
      (list (my/tool-bar--button 'env '(my/tool-bar-env-label)
                                 #'my/task-env-select nil)))

    ;; TeX
    (when (derived-mode-p 'TeX-mode)
      (list (my/tool-bar--button 'live "Live" #'my/tex-live-mode nil
                                 :help "Rebuild and re-sync the viewer on every save"
                                 :button '(:toggle . (bound-and-true-p my/tex-live-mode)))
            (my/tool-bar--button 'pdf "PDF" #'TeX-view nil
                                 :help "Open the built PDF at this line")
            (my/tool-bar--button 'clean "Clean" #'TeX-clean nil
                                 :help "Delete the intermediate files")))

    ;; The buffer
    (list (my/tool-bar--button 'save "Save" #'save-buffer t
                               :help "Save this buffer"
                               :enable '(buffer-modified-p))
          (my/tool-bar--button 'format "Format" #'my/format-dwim t
                               :help "Format this buffer")
          ;; The one `:visible' left in this group: `flymake-mode' is a minor mode.
          (my/tool-bar--button 'diagnostics "Diagnostics"
                               #'my/tool-bar-show-diagnostics t
                               :help "List this buffer's diagnostics"
                               :visible '(bound-and-true-p flymake-mode)))

    ;; The project
    (when my/tool-bar--project
      (list (my/tool-bar--button 'find "Find file" #'project-find-file t
                                 :help "Find a file in this project")
            (my/tool-bar--button 'search "Search" #'project-find-regexp t
                                 :help "Search this project")
            (my/tool-bar--button 'project '(my/tool-bar-project-label)
                                 #'project-switch-project nil
                                 :help "Switch project"))))))

(defun my/tool-bar--make-map ()
  "Build the tool bar keymap for the current buffer."
  (let ((map (make-sparse-keymap))
        (index 0))
    (dolist (group (my/tool-bar--groups))
      (when (> index 0)
        (define-key-after map (vector (intern (format "sep-%d" index)))
          '(menu-item "--")))
      (dolist (button group)
        (apply #'my/tool-bar--item map button))
      (setq index (1+ index)))
    map))

;;;; Isearch

(defun my/tool-bar--isearch-abort-filter (binding)
  "Isearch's `:filter' for Abort: `isearch-abort' once the search has a hit."
  (if isearch-success 'isearch-abort binding))

(defun my/tool-bar--make-isearch-map ()
  "Build isearch's tool bar keymap with this file's icons."
  (let ((map (make-sparse-keymap)))
    (dolist (button
             (list
              (my/tool-bar--button 'isearch-repeat-backward "Repeat backward"
                                   #'isearch-repeat-backward nil
                                   :help "Repeat search backward")
              (my/tool-bar--button 'isearch-repeat-forward "Repeat forward"
                                   #'isearch-repeat-forward nil
                                   :help "Repeat search forward")
              (my/tool-bar--button 'isearch-cancel "Abort"
                                   #'isearch-cancel nil
                                   :help "Abort search"
                                   :filter #'my/tool-bar--isearch-abort-filter)
              (my/tool-bar--button 'isearch-exit "Finish"
                                   #'isearch-exit nil
                                   :help "Finish search leaving point where it is"
                                   :visible '(not (string-equal isearch-string "")))
              (my/tool-bar--button 'isearch-delete-char "Undo"
                                   #'isearch-delete-char nil
                                   :help "Undo last input item")
              (my/tool-bar--button 'isearch-query-replace "Replace"
                                   #'isearch-query-replace nil
                                   :help "Replace search string")
              (my/tool-bar--button 'isearch-occur "Show hits"
                                   #'isearch-occur nil
                                   :help "Show each search hit")
              (my/tool-bar--button 'isearch-describe-mode "Help"
                                   #'isearch-describe-mode nil
                                   :help "Get help for Isearch")
              ;; m/sar.el.
              (my/tool-bar--button 'sar-case "Aa" #'my/sar-toggle-case nil
                                   :help "Match case"
                                   :button '(:toggle . (bound-and-true-p
                                                        my/sar-case-sensitive)))
              (my/tool-bar--button 'sar-word "Word" #'my/sar-toggle-whole-word nil
                                   :help "Whole words only"
                                   :button '(:toggle . (bound-and-true-p
                                                        my/sar-whole-word)))
              ;; A picker, like Run and Build: the label says which style is in force and the `▾' says there.
              (my/tool-bar--button 'sar-style '(my/sar-style-label)
                                   #'my/sar-set-style nil)
              (my/tool-bar--button 'sar-panel "In sidebar"
                                   #'my/sar-open-in-panel nil
                                   :help "Carry this search into the side panel")))
      (apply #'my/tool-bar--item map button))
    map))

(defun my/tool-bar-isearch-setup ()
  "Point `isearch-tool-bar-map' at our version of it."
  (setq isearch-tool-bar-map (my/tool-bar--make-isearch-map)))

;;;; Faces

(defconst my/tool-bar--shades '(8 14 20)
  "How far the button, its hover, and its pressed state sit off the page.")

(defface my/tool-bar-button
  '((t :box (:line-width -1 :style released-button)))
  "A tool-bar-styled button."
  :group 'my/task)

(defface my/tool-bar-button-hover
  '((t :box (:line-width -1 :style released-button)))
  "A tool-bar-styled button under the mouse."
  :group 'my/task)

(defface my/tool-bar-button-disabled
  '((t :inherit shadow :box (:line-width -1 :style released-button)))
  "A tool-bar-styled button that cannot be pressed."
  :group 'my/task)

(defface my/tool-bar-button-checked
  '((t :box (:line-width -1 :style pressed-button)))
  "A tool-bar-styled button that is on."
  :group 'my/task)

(defface my/tool-bar-button-checked-hover
  '((t :box (:line-width -1 :style pressed-button)))
  "A tool-bar-styled button that is on, under the mouse."
  :group 'my/task)

(defun my/tool-bar--label-family ()
  "The family the labels are set in."
  (or (if (my/tool-bar--images-p)
          (face-attribute 'variable-pitch :family nil t)
        (bound-and-true-p my/nerd-mono-font))
      'unspecified))

(defun my/tool-bar--refresh-faces (&rest _)
  "Derive the tool bar's colours from the theme now in force."
  (let ((foreground (face-attribute 'default :foreground nil t))
        (family (my/tool-bar--label-family))
        (raised '(:line-width -1 :style released-button))
        (pressed '(:line-width -1 :style pressed-button)))
    (set-face-attribute 'my/tool-bar-button nil
                        :inherit 'default :inverse-video nil :family family
                        :background (my/theme-shade (nth 0 my/tool-bar--shades))
                        :foreground foreground :box raised)
    (set-face-attribute 'my/tool-bar-button-hover nil
                        :inherit 'default :inverse-video nil :family family
                        :background (my/theme-shade (nth 1 my/tool-bar--shades))
                        :foreground foreground :box raised)
    (set-face-attribute 'my/tool-bar-button-checked nil
                        :inherit 'default :inverse-video nil :family family
                        :background (my/theme-shade (nth 2 my/tool-bar--shades))
                        :foreground foreground :box pressed)
    (set-face-attribute 'my/tool-bar-button-checked-hover nil
                        :inherit 'default :inverse-video nil :family family
                        :background (my/theme-shade (nth 2 my/tool-bar--shades))
                        :foreground foreground :box pressed)
    (set-face-attribute 'my/tool-bar-button-disabled nil
                        :inherit 'default :inverse-video nil :family family
                        :background (my/theme-shade (nth 0 my/tool-bar--shades))
                        :foreground (face-attribute 'shadow :foreground nil t)
                        :box raised)))

;;;; Turning it on

(defcustom my/tool-bar-hooks '(prog-mode-hook text-mode-hook conf-mode-hook)
  "Mode hooks that get a tool bar."
  :type '(repeat variable)
  :group 'my/task)

(defcustom my/tool-bar-exclude-modes '(log-edit-mode diff-mode)
  "Modes that reach `my/tool-bar-hooks' but should not have a tool bar. A commit message derives from `text-mode' and has nothing to run, build or format."
  :type '(repeat symbol)
  :group 'my/task)

(defun my/tool-bar-setup (&optional force)
  "Give this buffer the tool bar."
  (interactive)
  (unless (and (not force)
               (or (minibufferp)
                   (string-prefix-p " " (buffer-name))
                   (derived-mode-p my/tool-bar-exclude-modes)))
    ;; Before the map is built: the project group is one of the things it decides.
    (setq-local my/tool-bar--project
                (and (not (file-remote-p default-directory))
                     (require 'project nil t)
                     (when-let* ((project (project-current nil default-directory)))
                       (project-name project))))
    (setq-local tool-bar-map (my/tool-bar--make-map))))

(defun my/tool-bar-toggle ()
  "Show or hide the tool bar."
  (interactive)
  (tool-bar-mode 'toggle))

;;;; Keeping it current

(defun my/tool-bar--active-p ()
  "Non-nil when this buffer has a local tool bar map."
  (local-variable-p 'tool-bar-map))

(defun my/tool-bar-refresh ()
  "Rebuild the tool bar wherever one is showing."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (my/tool-bar--active-p)
        (setq-local tool-bar-map (my/tool-bar--make-map)))))
  (force-mode-line-update t))

(defvar my/tool-bar--retheme-timer nil)

(defun my/tool-bar--rethemed-1 ()
  "Rebuild faces, icon specs and maps for the theme now in force."
  (setq my/tool-bar--retheme-timer nil)
  (my/tool-bar--refresh-faces)
  (my/tool-bar--resolve-icons)
  (my/tool-bar-isearch-setup)
  (my/tool-bar-refresh))

(defun my/tool-bar--rethemed (&rest _)
  "Redraw the bar after a theme change, once AppKit has settled."
  (when (timerp my/tool-bar--retheme-timer)
    (cancel-timer my/tool-bar--retheme-timer))
  (setq my/tool-bar--retheme-timer
        (run-at-time 0 nil #'my/tool-bar--rethemed-1)))

;;;; Setup

(use-package tool-bar :ensure nil
  :demand t
  :config
  (setopt tool-bar-style (if (my/tool-bar--images-p) 'both 'text))
  (setq tool-bar-map (make-sparse-keymap))
  (when (fboundp 'global-window-tool-bar-mode)
    (global-window-tool-bar-mode -1))
  (require 'image)
  (my/tool-bar--install-mac-icons)
  (my/tool-bar--refresh-faces)
  (my/tool-bar--resolve-icons)
  ;; After the icons are resolved: the map holds the images, not the names.
  (my/tool-bar-isearch-setup)
  (add-hook 'enable-theme-functions #'my/tool-bar--rethemed)
  (add-hook 'my/task-state-change-hook #'my/tool-bar-refresh)
  (dolist (hook my/tool-bar-hooks)
    (add-hook hook #'my/tool-bar-setup))
  (tool-bar-mode 1))

;;; bar.el ends here
