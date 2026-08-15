;;; seek.el --- Right-click to search the selection in the system browser -*- lexical-binding: t; -*-

;;; Commentary:
;; right-click -> search selection (or word under mouse) in the system browser.

;;; Code:

(require 'browse-url)                   ; `browse-url-default-browser' is not autoloaded
(require 'mule-util)                    ; `truncate-string-to-width'
(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'url-util)                     ; `url-hexify-string'

;;;; Customization

(defgroup seek nil
  "Right-click to search the selection in the system browser."
  :group 'browse-url
  :prefix "seek-")

(defcustom seek-engines
  '(("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
    ("Google"     . "https://www.google.com/search?q=%s")
    ("Yandex"     . "https://yandex.ru/search/?text=%s")
    ("Kagi"       . "https://kagi.com/search?q=%s")
    ("4get"       . "https://4get.ca/web?s=%s"))
  "Search engines, as an alist of (NAME ."
  :type '(alist :key-type (string :tag "Name")
                :value-type (choice (string :tag "URL template")
                                    (function :tag "Function of the query")))
  :group 'seek)

(defcustom seek-default-engine "DuckDuckGo"
  "Name of the engine the top-level menu item and `seek' use. Should be a car of `seek-engines'."
  :type 'string
  :group 'seek)

(defcustom seek-browse-function #'browse-url-default-browser
  "Function called with the search URL."
  :type '(choice (const :tag "System default browser" browse-url-default-browser)
                 (const :tag "Whatever `browse-url' would do" browse-url)
                 (function :tag "Other function"))
  :group 'seek)

(defcustom seek-things '(url word)
  "Things to look for at the pointer when no region is active."
  :type '(repeat (symbol :tag "Thing"))
  :group 'seek)

(defcustom seek-max-query-length 200
  "Longest query handed to an engine, in characters, or nil for no limit."
  :type '(choice (const :tag "No limit" nil)
                 (natnum :tag "Characters"))
  :group 'seek)

(defcustom seek-label-width 32
  "Width of the query as it appears in the menu label, in columns."
  :type 'natnum
  :group 'seek)

;;;; Engines and URLs

(defun seek--engines ()
  "Return the entries of `seek-engines' that could actually be used."
  (seq-filter (lambda (entry)
                (and (consp entry)
                     (stringp (car entry))
                     (or (stringp (cdr entry)) (functionp (cdr entry)))))
              seek-engines))

(defun seek--engine (&optional name)
  "Return the usable entry called NAME, or the default one when NAME is nil."
  (let ((engines (seek--engines)))
    (or (assoc (or name seek-default-engine) engines)
        (car engines))))

(defun seek-url (query &optional name)
  "Return the URL searching for QUERY with the engine called NAME."
  (when-let* ((template (cdr (seek--engine name))))
    (if (functionp template)
        (funcall template query)
      (let ((encoded (url-hexify-string query)))
        (if (string-search "%s" template)
            (replace-regexp-in-string "%s" encoded template t t)
          (concat template encoded))))))

(defun seek--browse (query &optional name)
  "Open the search for QUERY with the engine called NAME."
  (if-let* ((url (seek-url query name)))
      (funcall seek-browse-function url)
    (user-error "No usable engine in `seek-engines'")))

;;;; What to search for

(defun seek--clean (string)
  "Return STRING as a one-line query, or nil when nothing is left of it."
  (when (stringp string)
    (let ((query (string-clean-whitespace
                  (replace-regexp-in-string
                   "[[:cntrl:]]+" " " (substring-no-properties string)))))
      (cond ((string-empty-p query) nil)
            ((and (natnump seek-max-query-length)
                  (> (length query) seek-max-query-length))
             (substring query 0 seek-max-query-length))
            (t query)))))

(defun seek--region-string ()
  "Return the active region of the current buffer, unpolished, or nil."
  (when (and mark-active (mark) (/= (region-beginning) (region-end)))
    (let* ((beg (region-beginning))
           (end (region-end))
           (slack (if (natnump seek-max-query-length)
                      (* 4 seek-max-query-length)
                    (- end beg))))
      (buffer-substring-no-properties beg (min end (+ beg slack))))))

(defun seek--thing-string ()
  "Return the first of `seek-things' found at point, cleaned, or nil."
  (seq-some (lambda (thing)
              (seek--clean (ignore-errors (thing-at-point thing t))))
            seek-things))

(defun seek-query ()
  "Return what to search for at point, or nil."
  (or (seek--clean (seek--region-string))
      (seek--thing-string)))

(defun seek--query-at (posn)
  "Return what to search for at POSN, or nil."
  (let ((window (posn-window posn))
        (position (posn-point posn)))
    (when (and (windowp window) (numberp position))
      (with-current-buffer (window-buffer window)
        (or (seek--clean (seek--region-string))
            (save-excursion
              (goto-char position)
              (seek--thing-string)))))))

;;;; Labels

(defun seek--quote (string)
  "Return STRING in quotation marks, curly ones where they will render."
  (if (char-displayable-p ?\N{LEFT DOUBLE QUOTATION MARK})
      (concat "\N{LEFT DOUBLE QUOTATION MARK}" string
              "\N{RIGHT DOUBLE QUOTATION MARK}")
    (concat "\"" string "\"")))

(defun seek--label (name query)
  "Return the menu label for searching QUERY with the engine called NAME."
  (format "Search %s for %s" name
          (seek--quote
           (truncate-string-to-width query seek-label-width nil nil t))))

;;;; The menu

(defun seek--command (query name)
  "Return a command searching for QUERY with the engine called NAME."
  (lambda ()
    (interactive)
    (seek--browse query name)))

(defun seek--submenu (query engines)
  "Return a keymap offering ENGINES, each searching for QUERY."
  (let ((map (make-sparse-keymap "Search With"))
        (index 0))
    (dolist (entry engines map)
      (let ((name (car entry)))
        ;; Numbered keys rather than names: engine names come from user configuration and nothing stops.
        (define-key-after map
          (vector (intern (format "seek-engine-%d" (setq index (1+ index)))))
          `(menu-item ,name ,(seek--command query name)
                      :help ,(format "Search %s in the browser" name)))))))

;;;###autoload
(defun seek-context-menu (menu click)
  "Populate MENU with searches for the text at CLICK."
  (when-let* ((engines (seek--engines))
              (default (car (seek--engine)))
              (query (condition-case nil
                         (seek--query-at (event-start click))
                       (error nil))))
    ;; `context-menu-map' drops this separator when it lands next to the one.
    (define-key-after menu [seek-separator] menu-bar-separator
      'middle-separator)
    (define-key-after menu [seek]
      `(menu-item ,(seek--label default query)
                  ,(seek--command query default)
                  :help ,(format "Search %s in the browser" default))
      'seek-separator)
    (when (cdr engines)                 ; a submenu of one is just a menu item
      (define-key-after menu [seek-with]
        `(menu-item "Search With" ,(seek--submenu query engines)
                    :help "Search for the same text with another engine")
        'seek)))
  menu)

;;;; Commands

(defvar seek-query-history nil
  "Minibuffer history for the query read by `seek' and `seek-with'.")

(defvar seek-engine-history nil
  "Minibuffer history for the engine read by `seek-with'.")

(defun seek--read-query ()
  "Return what to search for, asking when there is nothing at point."
  (or (seek-query)
      (seek--clean (read-string "Search for: " nil 'seek-query-history))
      (user-error "Nothing to search for")))

(defun seek--read-engine ()
  "Read the name of an engine, defaulting to `seek-default-engine'."
  (let ((names (mapcar #'car (seek--engines)))
        (default (car (seek--engine))))
    (unless names
      (user-error "No usable engine in `seek-engines'"))
    (completing-read (format-prompt "Search with" default)
                     names nil t nil 'seek-engine-history default)))

;;;###autoload
(defun seek (query)
  "Search for QUERY with `seek-default-engine' in the system browser."
  (interactive (list (seek--read-query)))
  (seek--browse query))

;;;###autoload
(defun seek-with (engine query)
  "Search for QUERY with the engine called ENGINE in the system browser."
  ;; The query first: it is read from the buffer.
  (interactive (let ((query (seek--read-query)))
                 (list (seek--read-engine) query)))
  (seek--browse query engine))

(provide 'seek)
;;; seek.el ends here
