;;; fontdue.el --- Fetch and install the fonts a config asks for -*- lexical-binding: t; -*-

;;; Commentary:
;; download/install fonts the config asks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)                    ; for `url-file-nondirectory'
(require 'url-util)                     ; for `url-unhex-string'

;;;; Customization

(defgroup fontdue nil
  "Fetch and install the fonts a config asks for."
  :group 'faces
  :prefix "fontdue-")

(defcustom fontdue-sources nil
  "Where each font family can be fetched from."
  :type '(alist :key-type (symbol :tag "Source")
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'fontdue)

(defcustom fontdue-wanted nil
  "The font families to account for, or a function returning them."
  :type '(choice (repeat :tag "Family names" string)
                 (function :tag "Function returning family names"))
  :group 'fontdue)

(defcustom fontdue-system-families nil
  "Families the operating system ships, which are never missing."
  :type '(repeat string)
  :group 'fontdue)

(defcustom fontdue-unobtainable-families nil
  "Families with no source, and why, as an alist of (FAMILY ."
  :type '(alist :key-type (string :tag "Family") :value-type (string :tag "Reason"))
  :group 'fontdue)

(defcustom fontdue-scope 'user
  "Whether fonts are installed for this account or for the machine."
  :type '(choice (const :tag "This account only" user)
                 (const :tag "The whole machine" system))
  :group 'fontdue)

(defcustom fontdue-directory nil
  "Directory fonts are installed into, or nil to derive one."
  :type '(choice (const :tag "Derive from `fontdue-scope'" nil)
                 (directory :tag "This directory"))
  :group 'fontdue)

(defcustom fontdue-search-directories nil
  "Directories read when working out which families are installed, or nil."
  :type '(choice (const :tag "Derive from `system-type'" nil)
                 (repeat directory))
  :group 'fontdue)

(defcustom fontdue-elevate 'sudo
  "How to write to the font directory when it is not ours to write."
  :type '(choice (const :tag "TRAMP /sudo:: (minibuffer prompt)" sudo)
                 (const :tag "macOS authorisation dialog" osascript)
                 (const :tag "Never elevate" nil))
  :group 'fontdue)

(defcustom fontdue-auto-install nil
  "Whether `fontdue-maybe-install' fetches missing fonts when it runs. Put that function on `emacs-startup-hook' and a machine missing fonts fixes itself at the one moment you are likely to care."
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Report and ask once" ask)
                 (const :tag "Just fetch them" t))
  :group 'fontdue)

(defcustom fontdue-display-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . 0.4))
  "Display action for the log and report buffer."
  :type 'sexp
  :group 'fontdue)

(defcustom fontdue-curl-program "curl"
  "Program used to download.  Must understand curl's options."
  :type 'string
  :group 'fontdue)

;;;; Where fonts live on this system

(defun fontdue--env (name)
  "Return environment variable NAME, or nil when it is unset or empty."
  (let ((value (getenv name)))
    (and value (not (string-empty-p value)) value)))

(defun fontdue--under (parent &rest components)
  "Join COMPONENTS under PARENT as a directory name."
  (file-name-as-directory
   (concat (file-name-as-directory parent) (string-join components "/"))))

(defun fontdue-default-directory (&optional scope)
  "Return the font directory for SCOPE, defaulting to `fontdue-scope'."
  (pcase (cons (or scope fontdue-scope) system-type)
    ('(user . darwin) (expand-file-name "~/Library/Fonts/"))
    ('(system . darwin) "/Library/Fonts/")
    ('(user . windows-nt)
     (fontdue--under (or (fontdue--env "LOCALAPPDATA")
                         (expand-file-name "~/AppData/Local"))
                     "Microsoft" "Windows" "Fonts"))
    ('(system . windows-nt)
     (fontdue--under (or (fontdue--env "SystemRoot") "C:/Windows") "Fonts"))
    (`(user . ,_)
     (fontdue--under (or (fontdue--env "XDG_DATA_HOME")
                         (expand-file-name "~/.local/share"))
                     "fonts"))
    (`(system . ,_) "/usr/local/share/fonts/")))

(defun fontdue--target ()
  "Return the directory fonts are installed into."
  (file-name-as-directory
   (if fontdue-directory
       (expand-file-name fontdue-directory)
     (fontdue-default-directory))))

(defun fontdue--search-directories ()
  "Return the directories read to find out what is installed."
  (or fontdue-search-directories
      (delete-dups
       (append (list (fontdue-default-directory 'user)
                     (fontdue-default-directory 'system))
               (unless (memq system-type '(darwin windows-nt))
                 (list (expand-file-name "~/.fonts/")))))))

;;;; What a font calls itself


(defsubst fontdue--u8 (off)
  "Byte at OFF in the current buffer."
  (char-after (+ (point-min) off)))

(defsubst fontdue--u16 (off)
  "Big-endian 16-bit integer at OFF in the current buffer."
  (logior (ash (fontdue--u8 off) 8) (fontdue--u8 (1+ off))))

(defsubst fontdue--u32 (off)
  "Big-endian 32-bit integer at OFF in the current buffer."
  (logior (ash (fontdue--u16 off) 16) (fontdue--u16 (+ off 2))))

(defconst fontdue--tag-ttcf #x74746366 "The `ttcf' tag: a font collection.")
(defconst fontdue--tag-name #x6e616d65 "The `name' tag: the name table.")

(defun fontdue--directories ()
  "Return the offsets of every table directory in the current buffer."
  (if (/= (fontdue--u32 0) fontdue--tag-ttcf)
      (list 0)
    (let ((count (fontdue--u32 8)) (offsets nil))
      (dotimes (i count)
        (push (fontdue--u32 (+ 12 (* 4 i))) offsets))
      (nreverse offsets))))

(defun fontdue--table (directory tag)
  "Return the offset of the table called TAG in DIRECTORY, or nil."
  (let ((count (fontdue--u16 (+ directory 4)))
        (found nil))
    (dotimes (i count)
      (let ((record (+ directory 12 (* 16 i))))
        (when (and (null found) (= (fontdue--u32 record) tag))
          (setq found (fontdue--u32 (+ record 8))))))
    found))

(defun fontdue--names (table ids)
  "Return the strings in the name TABLE whose name IDs are in IDS."
  (let* ((count (fontdue--u16 (+ table 2)))
         (storage (+ table (fontdue--u16 (+ table 4))))
         (names nil))
    (dotimes (i count)
      (let* ((record (+ table 6 (* 12 i)))
             (platform (fontdue--u16 record))
             (id (fontdue--u16 (+ record 6)))
             (length (fontdue--u16 (+ record 8)))
             (offset (fontdue--u16 (+ record 10))))
        (when (memq id ids)
          (let* ((beg (+ (point-min) storage offset))
                 (end (+ beg length)))
            (when (<= end (point-max))
              (let ((string (ignore-errors
                              (decode-coding-string
                               (buffer-substring-no-properties beg end)
                               (if (= platform 3) 'utf-16be 'mac-roman) t))))
                (when (and string (not (string-empty-p string)))
                  (push (string-trim string) names))))))))
    (nreverse names)))

(defun fontdue--font-families (file)
  "Return the family names FILE declares, or nil when it cannot be read."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (let ((families nil))
          (dolist (directory (fontdue--directories))
            (when-let* ((table (fontdue--table directory fontdue--tag-name)))
              (dolist (name (fontdue--names table '(1 16)))
                (cl-pushnew name families :test #'equal))))
          (nreverse families)))
    (error nil)))

;;;; What is installed

(defconst fontdue--font-file-regexp "\\.\\(?:ttf\\|otf\\|ttc\\|otc\\)\\'"
  "Font files worth reading or installing.")

(defun fontdue--files (directory)
  "Return the font files directly in DIRECTORY."
  (when (file-directory-p directory)
    (directory-files directory t fontdue--font-file-regexp t)))

(defun fontdue-installed-families ()
  "Return every family this machine can currently use."
  (let ((families (ignore-errors (font-family-list))))
    (dolist (directory (fontdue--search-directories))
      (dolist (file (fontdue--files directory))
        (dolist (family (fontdue--font-families file))
          (cl-pushnew family families :test #'equal))))
    families))

;;;; What is wanted

(defun fontdue-wanted-families ()
  "Return the families to account for, from `fontdue-wanted'."
  (delete-dups
   (copy-sequence
    (if (functionp fontdue-wanted) (funcall fontdue-wanted) fontdue-wanted))))

(defun fontdue--source-for (family)
  "Return the id of the source providing FAMILY, or nil."
  (car (seq-find (lambda (source)
                   (member family (plist-get (cdr source) :provides)))
                 fontdue-sources)))

(defun fontdue--status (family installed)
  "Return what FAMILY is, given the list of INSTALLED families."
  (cond ((member family installed) 'installed)
        ((member family fontdue-system-families) 'system)
        ((fontdue--source-for family))
        ((assoc family fontdue-unobtainable-families)
         (cons 'unobtainable (cdr (assoc family fontdue-unobtainable-families))))
        (t (cons 'unobtainable "no source in `fontdue-sources'"))))

(defun fontdue-missing-sources ()
  "Return the sources needed to cover every missing wanted family."
  (let ((installed (fontdue-installed-families))
        (needed nil))
    (dolist (family (fontdue-wanted-families))
      (let ((status (fontdue--status family installed)))
        (when (and (symbolp status) (not (memq status '(installed system))))
          (cl-pushnew status needed))))
    (seq-filter (lambda (source) (memq (car source) needed)) fontdue-sources)))

;;;; Reporting

(defconst fontdue-buffer-name "*fontdue*"
  "Buffer the report and the download log are written to.")

(defun fontdue--buffer ()
  "Return the log buffer, creating it."
  (get-buffer-create fontdue-buffer-name))

(defun fontdue--show ()
  "Display the log buffer per `fontdue-display-action'."
  (display-buffer (fontdue--buffer) fontdue-display-action))

(defun fontdue--log (format &rest args)
  "Append FORMAT with ARGS to the log buffer, and echo it."
  (let ((line (apply #'format format args)))
    (with-current-buffer (fontdue--buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert line "\n")))
    (message "fontdue: %s" line)))

;;;###autoload
(defun fontdue-report ()
  "Show what every family in `fontdue-wanted' is doing."
  (interactive)
  (let ((installed (fontdue-installed-families))
        (families (fontdue-wanted-families))
        (missing 0))
    (with-current-buffer (fontdue--buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-34s %s\n" "FAMILY" "STATUS") (make-string 72 ?-) "\n")
        (if (null families)
            (insert "`fontdue-wanted' is empty -- nothing to account for.\n")
          (dolist (family families)
            (let ((status (fontdue--status family installed)))
              (insert
               (format "%-34s %s\n" family
                       (cond ((eq status 'installed) "installed")
                             ((eq status 'system) "ships with the system")
                             ((consp status) (format "unobtainable -- %s" (cdr status)))
                             (t (setq missing (1+ missing))
                                (format "fetch from `%s' (%s)" status
                                        (or (plist-get (alist-get status fontdue-sources)
                                                       :size)
                                            "?"))))))))
          (insert (make-string 72 ?-) "\n")
          (insert (format "%d of %d wanted families are missing and fetchable"
                          missing (length families)))
          (let ((sources (length (fontdue-missing-sources))))
            (when (> missing 0)
              (insert (format ", from %d source%s.\nM-x fontdue-install-missing"
                              sources (if (= 1 sources) "" "s")))))
          (insert (format "\nInstalling into %s\n"
                          (abbreviate-file-name (fontdue--target)))))
        (goto-char (point-min))
        (special-mode)))
    (fontdue--show)))

;;;; Installing

(defvar fontdue--queue nil
  "Sources still to be fetched, as (ID . PLIST) entries.")

(defvar fontdue--workdir nil
  "Scratch directory for the source in progress, deleted when it ends.")

(defvar fontdue--staging nil
  "Directory the fonts of every source accumulate in until the run ends.")

(defvar fontdue--pending nil
  "URLs of the current source still to be fetched.")

(defvar fontdue--source nil
  "The (ID . PLIST) entry being fetched, and the flag saying a run is live.")

(defconst fontdue--archive-regexp
  "\\.\\(?:zip\\|tar\\|tar\\.gz\\|tgz\\|tar\\.bz2\\|tar\\.xz\\)\\'"
  "URLs ending like this are unpacked; anything else is a font file.")

(defun fontdue--archive-p (url)
  "Whether URL names an archive rather than a font file."
  (string-match-p fontdue--archive-regexp (car (split-string url "?" t))))

(defun fontdue--url-file-name (url)
  "Return the local file name URL is asking for."
  (url-unhex-string (url-file-nondirectory url)))

(defun fontdue--run (name command sentinel)
  "Run COMMAND named NAME, calling SENTINEL with t or nil when it ends."
  (make-process
   :name name
   :buffer (fontdue--buffer)
   :noquery t
   :connection-type 'pipe
   :command command
   :sentinel (lambda (process _event)
               (unless (process-live-p process)
                 (funcall sentinel (eq 0 (process-exit-status process)))))))

;;;; Writing where we may not be allowed to

(defun fontdue--applescript-string (string)
  "Return STRING as an AppleScript string literal."
  (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" string) "\""))

(defun fontdue--copy-command (files directory)
  "Return the shell command putting FILES into DIRECTORY."
  (let ((target (shell-quote-argument (directory-file-name directory))))
    (format "/bin/mkdir -p %s && /bin/cp -f %s %s/"
            target (mapconcat #'shell-quote-argument files " ") target)))

(defun fontdue--copy-into (files directory)
  "Copy FILES into DIRECTORY, elevating if it is not ours to write."
  (cond
   ((and (file-directory-p directory) (file-writable-p directory))
    (condition-case err
        (progn (dolist (file files)
                 (copy-file file (expand-file-name (file-name-nondirectory file)
                                                   directory)
                            t))
               nil)
      (error (error-message-string err))))
   ((null fontdue-elevate)
    (format "%s is not writable and `fontdue-elevate' is nil" directory))
   ((eq system-type 'windows-nt)
    (format "%s is not writable, and elevation is not supported on Windows -- \
set `fontdue-scope' to `user'" directory))
   ((eq fontdue-elevate 'sudo)
    (fontdue--log "asking for a password: %s is not writable"
                  (abbreviate-file-name directory))
    (require 'tramp)
    ;; /sudo:: is this same machine, so the local absolute paths in the command are the paths root.
    (let* ((default-directory "/sudo::/")
           (status (process-file "/bin/sh" nil (fontdue--buffer) nil "-c"
                                 (fontdue--copy-command files directory))))
      (unless (eq status 0) (format "sudo copy exited %s" status))))
   ((eq fontdue-elevate 'osascript)
    (fontdue--log "asking macOS to authorise writing to %s"
                  (abbreviate-file-name directory))
    (let ((status (call-process
                   "osascript" nil (fontdue--buffer) nil "-e"
                   (format "do shell script %s with administrator privileges"
                           (fontdue--applescript-string
                            (fontdue--copy-command files directory))))))
      (unless (eq status 0)
        (format "osascript exited %s -- cancelled, or no window server" status))))
   (t (format "`fontdue-elevate' is %S, which means nothing here" fontdue-elevate))))

;;;; Telling the system a font arrived


(defun fontdue--register (files directory)
  "Make DIRECTORY's newly arrived FILES visible to the system."
  (pcase system-type
    ('darwin nil)
    ('windows-nt (fontdue--register-windows files directory))
    (_ (if (not (executable-find "fc-cache"))
           "no `fc-cache' in exec-path -- fonts may not be seen until you run one"
         (let ((status (call-process "fc-cache" nil (fontdue--buffer) nil
                                     "-f" (directory-file-name directory))))
           (unless (eq status 0) (format "fc-cache exited %s" status)))))))

(defun fontdue--registry-name (file)
  "Return the name Windows files FILE under in the fonts registry."
  (format "%s (%s)"
          (or (car (fontdue--font-families file))
              (file-name-base file))
          (if (member (downcase (or (file-name-extension file) ""))
                      '("otf" "otc"))
              "OpenType" "TrueType")))

(defun fontdue--register-windows (files directory)
  "Add FILES in DIRECTORY to the Windows font registry."
  (let* ((system (equal (file-name-as-directory directory)
                        (fontdue-default-directory 'system)))
         (hive (if system "HKLM" "HKCU"))
         (key (format "%s\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Fonts"
                      hive))
         (failed 0))
    (dolist (file files)
      (let ((installed (expand-file-name (file-name-nondirectory file) directory)))
        (unless (eq 0 (call-process
                       "reg" nil (fontdue--buffer) nil "add" key
                       "/v" (fontdue--registry-name file)
                       "/t" "REG_SZ"
                       "/d" (if system (file-name-nondirectory file)
                              (subst-char-in-string ?/ ?\\ installed))
                       "/f"))
          (setq failed (1+ failed)))))
    (when (> failed 0)
      (format "%d of %d registry entries failed -- fonts are on disk but \
unregistered" failed (length files)))))

;;;; The run

(defun fontdue--tidy ()
  "Delete the scratch directory of the source that just finished."
  (when (and fontdue--workdir (file-directory-p fontdue--workdir))
    (delete-directory fontdue--workdir t))
  (setq fontdue--workdir nil))

(defun fontdue--fail (format &rest args)
  "Log FORMAT with ARGS as a failure and move on to the next source."
  (apply #'fontdue--log (concat "FAILED -- " format) args)
  (fontdue--tidy)
  (fontdue--next))

(defun fontdue--begin (sources)
  "Start a run over SOURCES, which must not be empty."
  (with-current-buffer (fontdue--buffer)
    (let ((inhibit-read-only t)) (erase-buffer) (special-mode)))
  (fontdue--show)
  (setq fontdue--staging (expand-file-name "install" (make-temp-file "fontdue-" t))
        fontdue--queue sources)
  (make-directory fontdue--staging t)
  (fontdue--next))

(defun fontdue--next ()
  "Start the next queued source, or install what has piled up."
  (if-let* ((source (pop fontdue--queue)))
      (fontdue--start source)
    (fontdue--finish)))

(defun fontdue--start (source)
  "Begin fetching SOURCE, an entry of `fontdue-sources'."
  (setq fontdue--source source
        fontdue--workdir (make-temp-file "fontdue-" t)
        fontdue--pending (copy-sequence (plist-get (cdr source) :urls)))
  (fontdue--log "%s: fetching %s" (car source)
                (or (plist-get (cdr source) :size) "?"))
  (fontdue--fetch))

(defun fontdue--fetch ()
  "Download the next pending URL of the current source."
  (if-let* ((url (pop fontdue--pending)))
      (let ((file (expand-file-name (fontdue--url-file-name url) fontdue--workdir)))
        (fontdue--run
         "fontdue-curl"
         ;; -location for a release URL's redirect to a CDN.
         (list fontdue-curl-program "--fail" "--location" "--silent"
               "--show-error" "--globoff" "--retry" "2"
               "--connect-timeout" "20" "--output" file url)
         (lambda (ok)
           (if (not ok)
               (fontdue--fail "%s: could not download %s" (car fontdue--source) url)
             (fontdue--fetch)))))
    (fontdue--unpack)))

(defun fontdue--unpack ()
  "Unpack the current source's archive, if it has one, then stage it."
  (let* ((urls (plist-get (cdr fontdue--source) :urls))
         (archive (and (= 1 (length urls))
                       (fontdue--archive-p (car urls))
                       (expand-file-name (fontdue--url-file-name (car urls))
                                         fontdue--workdir))))
    (if (not archive)
        (fontdue--stage)
      (let ((into (expand-file-name "unpacked" fontdue--workdir)))
        (make-directory into t)
        (fontdue--run
         "fontdue-unpack"
         (if (string-suffix-p ".zip" archive)
             (list "unzip" "-o" "-q" archive "-d" into)
           ;; tar reads gz, bz2 and xz without being told.
           (list "tar" "-xf" archive "-C" into))
         (lambda (ok)
           (if (not ok)
               (fontdue--fail "%s: could not unpack %s" (car fontdue--source)
                              (file-name-nondirectory archive))
             (fontdue--stage))))))))

(defun fontdue--stage ()
  "Move the current source's fonts into staging and check their families."
  (let* ((id (car fontdue--source))
         (expected (plist-get (cdr fontdue--source) :provides))
         (files (directory-files-recursively
                 fontdue--workdir fontdue--font-file-regexp))
         (found nil))
    (if (null files)
        (fontdue--fail "%s: the download held no font files" id)
      (dolist (file files)
        (let ((staged (expand-file-name (file-name-nondirectory file)
                                        fontdue--staging)))
          (rename-file file staged t)
          (dolist (family (fontdue--font-families staged))
            (cl-pushnew family found :test #'equal))))
      (fontdue--log "%s: %d file%s staged" id (length files)
                    (if (= 1 (length files)) "" "s"))
      ;; The catalogue said what this would provide; the fonts themselves are the authority.
      (let ((surprise (seq-remove (lambda (f) (member f expected)) found))
            (absent (seq-remove (lambda (f) (member f found)) expected)))
        (when surprise
          (fontdue--log "%s: also provides %s" id
                        (string-join (sort surprise #'string<) ", ")))
        (when absent
          (fontdue--log "%s: expected but did not find %s -- `:provides' is stale"
                        id (string-join (sort absent #'string<) ", "))))
      (fontdue--tidy)
      (fontdue--next))))

(defun fontdue--finish ()
  "Install everything staged, then clear the run."
  (let* ((target (fontdue--target))
         (files (and fontdue--staging (fontdue--files fontdue--staging)))
         (failure (and files (fontdue--copy-into files target))))
    (cond ((null files)
           (fontdue--log "nothing was staged -- nothing installed"))
          (failure
           (fontdue--log "FAILED -- could not install into %s: %s"
                         (abbreviate-file-name target) failure))
          (t
           (fontdue--log "installed %d file%s into %s" (length files)
                         (if (= 1 (length files)) "" "s")
                         (abbreviate-file-name target))
           (when-let* ((trouble (fontdue--register files target)))
             (fontdue--log "note: %s" trouble))
           (fontdue--log
            "done.  Restart Emacs for new families to reach `font-family-list'.")))
    (when (and fontdue--staging (file-directory-p fontdue--staging))
      (delete-directory (directory-file-name
                         (file-name-directory (directory-file-name fontdue--staging)))
                        t))
    (setq fontdue--staging nil fontdue--source nil fontdue--queue nil)))

(defun fontdue--check-ready ()
  "Signal unless a run could start now."
  (unless (executable-find fontdue-curl-program)
    (user-error "No `%s' in exec-path" fontdue-curl-program))
  (when fontdue--source
    (user-error "fontdue is already downloading `%s'" (car fontdue--source))))

(defun fontdue--summary (sources)
  "Return the confirmation text for downloading SOURCES."
  (format "Download %d source%s into %s?\n%s\n"
          (length sources) (if (= 1 (length sources)) "" "s")
          (abbreviate-file-name (fontdue--target))
          (mapconcat (lambda (source)
                       (format "  %-18s %-7s %s" (car source)
                               (or (plist-get (cdr source) :size) "?")
                               (string-join (plist-get (cdr source) :provides) ", ")))
                     sources "\n")))

;;;###autoload
(defun fontdue-install-missing ()
  "Download and install every fetchable family that is missing."
  (interactive)
  (fontdue--check-ready)
  (let ((sources (fontdue-missing-sources)))
    (unless sources
      (user-error "Nothing missing that can be fetched -- see M-x fontdue-report"))
    (when (yes-or-no-p (fontdue--summary sources))
      (fontdue--begin sources))))

;;;###autoload
(defun fontdue-install-source (id)
  "Download and install the source called ID, whatever is installed already."
  (interactive
   (list (intern (completing-read
                  "Install source: "
                  (mapcar (lambda (source) (symbol-name (car source))) fontdue-sources)
                  nil t))))
  (fontdue--check-ready)
  (let ((source (assq id fontdue-sources)))
    (unless source
      (user-error "No such source: %s" id))
    (fontdue--begin (list source))))

;;;###autoload
(defun fontdue-verify ()
  "List the families every font in the install directory declares."
  (interactive)
  (let* ((target (fontdue--target))
         (files (fontdue--files target))
         (wanted (fontdue-wanted-families))
         (families nil))
    (dolist (file files)
      (dolist (family (fontdue--font-families file))
        (cl-pushnew family families :test #'equal)))
    (with-current-buffer (fontdue--buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d font files in %s declare %d famil%s:\n\n"
                        (length files) (abbreviate-file-name target)
                        (length families) (if (= 1 (length families)) "y" "ies")))
        (dolist (family (sort families #'string<))
          (insert "  " family (if (member family wanted) "  <- wanted" "") "\n"))
        (goto-char (point-min))
        (special-mode)))
    (fontdue--show)))

;;;###autoload
(defun fontdue-maybe-install ()
  "Fetch missing fonts if `fontdue-auto-install' says to."
  (interactive)
  (when (and fontdue-auto-install
             fontdue-sources
             (not fontdue--source)
             (executable-find fontdue-curl-program))
    (condition-case err
        (when-let* ((sources (fontdue-missing-sources)))
          (if (eq fontdue-auto-install 'ask)
              (when (yes-or-no-p (concat "fontdue: " (fontdue--summary sources)))
                (fontdue--begin sources))
            (fontdue--begin sources)))
      (error (message "fontdue: %s" (error-message-string err))))))

(provide 'fontdue)
;;; fontdue.el ends here
