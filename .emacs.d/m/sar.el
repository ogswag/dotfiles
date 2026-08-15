;;; sar.el --- Search and replace, in a buffer or across a project -*- lexical-binding: t; -*-

;;; Commentary:
;; isearch + project search panel. plain/wildcard/pcre-ish.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)

;; m/p is not on `load-path' by default.
(eval-and-compile
  (add-to-list 'load-path
               (or (bound-and-true-p my-vendor-directory)
                   (expand-file-name "m/p" user-emacs-directory))))
(require 'sidepanel)

;;;; Forward declarations

(declare-function rxt-pcre-to-elisp "pcre2el" (pcre &optional flags))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function my/tool-bar-refresh "bar" ())
(declare-function undo-fu-disable-checkpoint "undo-fu" ())

(defvar my/tab-exempt-regexps)

;;;; Options

(defgroup my/sar nil
  "Search and replace across a buffer, a folder or a project."
  :group 'matching
  :prefix "my/sar-")

(defcustom my/sar-style 'plain
  "How a query is read.

`plain' takes it literally, `wildcard' gives `*' and `?' their shell meanings,
and `regex' reads it as PCRE -- see this file's commentary for which parts."
  :type '(choice (const :tag "Plain text" plain)
                 (const :tag "Wildcard (* and ?)" wildcard)
                 (const :tag "Regular expression" regex)))

(defcustom my/sar-case-sensitive nil
  "Whether a query only matches text of the same case."
  :type 'boolean)

(defcustom my/sar-whole-word nil
  "Whether a query only matches at word boundaries."
  :type 'boolean)

(defcustom my/sar-preserve-case nil
  "Whether a replacement takes the case of the text it replaces.

Off, so what is typed is what is inserted.  On, it follows `replace-match's
case adaptation: a match in capitals is replaced in capitals."
  :type 'boolean)

;;;; Styles

(defun my/sar--wildcard-to-regexp (query)
  "QUERY as an Emacs regexp, with `*' and `?' given their shell meanings.

Built a character at a time rather than by quoting the lot and unquoting the
two wildcards afterwards: `regexp-quote' is free to escape however it likes,
and a query with a backslash of its own would make that guesswork."
  (mapconcat (lambda (character)
               (pcase character
                 (?* ".*")
                 (?? ".")
                 (_ (regexp-quote (char-to-string character)))))
             query ""))

(defconst my/sar--pcre-hints
  '(("(\\?[=!]" . "Emacs's regexp engine has no lookahead")
    ("(\\?<[=!]" . "Emacs's regexp engine has no lookbehind")
    ("(\\?P\\|(\\?<" . "Emacs has no named groups -- use a numbered one")
    ("(\\?>" . "Emacs has no atomic groups")
    ("{[A-Za-z]" . "Emacs has no \\p{...} classes -- try [[:alpha:]] and friends"))
  "Alist of (REGEXP . HINT) matched against pcre2el's complaint.")

(defun my/sar--pcre-hint (message)
  "A word on why MESSAGE happened, where this file knows one."
  (or (seq-some (lambda (entry)
                  (and (string-match-p (car entry) message) (cdr entry)))
                my/sar--pcre-hints)
      "not supported here"))

(defun my/sar--pcre-to-regexp (query)
  "QUERY, read as PCRE, as an Emacs regexp.

Refuses rather than approximates.  Every construct pcre2el turns down is one
Emacs cannot express, so the alternative to this message is a pattern that
means one thing in the buffer and another in the panel."
  (unless (require 'pcre2el nil t)
    (user-error "The regex style needs the pcre2el package"))
  (condition-case err
      (rxt-pcre-to-elisp query)
    (rxt-invalid-regexp
     (user-error "%s (%s)" (error-message-string err)
                 (my/sar--pcre-hint (error-message-string err))))
    (error
     (user-error "Bad pattern: %s" (error-message-string err)))))

(defun my/sar--build (query style whole-word)
  "QUERY as an Emacs regexp under STYLE, anchored to words when WHOLE-WORD."
  (let ((regexp (pcase style
                  ('plain (regexp-quote query))
                  ('wildcard (my/sar--wildcard-to-regexp query))
                  ('regex (my/sar--pcre-to-regexp query))
                  (_ (user-error "Unknown search style `%s'" style)))))
    (if whole-word
        ;; The shy group is not decoration: `\|' binds looser than anything else.
        (concat "\\b\\(?:" regexp "\\)\\b")
      regexp)))

(defvar my/sar--matcher-cache (make-hash-table :test #'equal)
  "Cache of (QUERY STYLE WHOLE-WORD) -> Emacs regexp.")

(defun my/sar--matcher (query)
  "The Emacs regexp QUERY means under the style and toggles now in force.

Memoised: a live search rebuilds this on every keystroke, and the regex style
runs a parser.  Case is deliberately not part of it -- it belongs to
`case-fold-search' at the point of searching, not to the pattern."
  (if (string-empty-p query)
      ""
    (let ((key (list query my/sar-style my/sar-whole-word)))
      (or (gethash key my/sar--matcher-cache)
          (puthash key (my/sar--build query my/sar-style my/sar-whole-word)
                   my/sar--matcher-cache)))))

(defun my/sar--valid-p (query)
  "Non-nil when QUERY builds a usable regexp, quietly."
  (condition-case nil
      (progn (my/sar--matcher query) t)
    (error nil)))

;;;; Replacement templates

(defun my/sar--replacement (template)
  "TEMPLATE as a `replace-match' replacement string.

`$1' and `${1}' are group one, `$&' and `$0' the whole match, `$$' a dollar;
`\\n' and `\\t' are the characters they name.  Emacs's own `\\1' is taken as
written, so either dialect works.  In the plain style there is nothing to
translate -- the replacement goes in literally, dollars, backslashes and all."
  (if (eq my/sar-style 'plain)
      template
    (let ((parts nil)
          (index 0)
          (length (length template)))
      (while (< index length)
        (let ((character (aref template index))
              (next (and (< (1+ index) length) (aref template (1+ index)))))
          (cond
           ((and (eq character ?$) next)
            (setq index (1+ index))
            (cond
             ((memq next '(?& ?0)) (push "\\&" parts))
             ((eq next ?$) (push "$" parts))
             ((and (>= next ?1) (<= next ?9)) (push (format "\\%c" next) parts))
             ((eq next ?{)
              (let ((close (string-search "}" template index)))
                (if-let* ((close)
                          (digits (substring template (1+ index) close))
                          ((string-match-p "\\`[0-9]+\\'" digits)))
                    (progn (push (concat "\\" digits) parts)
                           (setq index close))
                  (push "$" parts)
                  (setq index (1- index)))))
             (t (push "$" parts) (setq index (1- index)))))
           ((and (eq character ?\\) next)
            (setq index (1+ index))
            (push (pcase next
                    (?n "\n") (?t "\t") (?r "\r")
                    (?\\ "\\\\") (?& "\\&")
                    ((guard (and (>= next ?0) (<= next ?9))) (format "\\%c" next))
                    (_ (concat "\\\\" (char-to-string next))))
                  parts))
           ;; A trailing lone backslash, same reasoning.
           ((eq character ?\\) (push "\\\\" parts))
           (t (push (char-to-string character) parts))))
        (setq index (1+ index)))
      (apply #'concat (nreverse parts)))))

;;;; The region

(defun my/sar--region-bounds ()
  "The active region as a (BEGIN . END) pair of markers, or nil.

Markers, and taken now: m/edit.el clears `mark-ring' and the mark marker on
every `deactivate-mark', so nothing may go back and ask for the region later.
END advances with insertions, so a replacement that grows the text stays inside
the region it was told to work in."
  (when (use-region-p)
    (cons (copy-marker (region-beginning))
          (copy-marker (region-end) t))))

;;;; Replacing in one buffer

(defun my/sar--replace-in-buffer (query template &optional bounds)
  "Replace every match of QUERY with TEMPLATE in the current buffer.

BOUNDS is a (BEGIN . END) pair as `my/sar--region-bounds' returns, or nil for
the whole buffer.  Returns the number of replacements made."
  (let* ((regexp (my/sar--matcher query))
         (replacement (my/sar--replacement template))
         (literal (eq my/sar-style 'plain))
         (case-fold-search (not my/sar-case-sensitive))
         (start (if bounds (marker-position (car bounds)) (point-min)))
         (end (if bounds (cdr bounds) (point-max-marker)))
         (count 0)
         (searching t))
    (save-excursion
      (goto-char start)
      (while (and searching (re-search-forward regexp end t))
        (if (> (match-end 0) (match-beginning 0))
            (progn
              (replace-match replacement (not my/sar-preserve-case) literal)
              (setq count (1+ count)))
          ;; A pattern that can match nothing would otherwise sit on one spot for ever.
          (if (< (point) end)
              (forward-char 1)
            (setq searching nil)))))
    count))

;;;; In-buffer search
;; isearch, with the styles and toggles layered on.

(defvar my/sar--isearch-bounds nil
  "Region the running search is confined to, or nil for the whole buffer.")

(defconst my/sar--isearch-locals
  '(isearch-search-fun-function
    isearch-filter-predicate
    lazy-highlight-buffer
    isearch-lazy-count
    isearch-wrap-pause)
  "What `my/sar-find' rebinds buffer-locally, and gives back afterwards.")

(defun my/sar--isearch-search-fun ()
  "A searcher reading `isearch-string' under the current style and toggles."
  (lambda (string bound noerror count)
    (when-let* ((regexp (condition-case nil (my/sar--matcher string) (error nil)))
                ((not (string-empty-p regexp))))
      (let ((case-fold-search (not my/sar-case-sensitive)))
        (if isearch-forward
            (re-search-forward regexp bound noerror count)
          (re-search-backward regexp bound noerror count))))))

(defun my/sar--isearch-filter (beg end)
  "Accept a match from BEG to END when it is inside the searched region."
  (and (isearch-filter-visible beg end)
       (or (null my/sar--isearch-bounds)
           (and (>= beg (car my/sar--isearch-bounds))
                (<= end (cdr my/sar--isearch-bounds))))))

(defun my/sar--isearch-cleanup ()
  "Give the buffer its search settings back.  On `isearch-mode-end-hook'."
  (remove-hook 'isearch-mode-end-hook #'my/sar--isearch-cleanup t)
  (dolist (variable my/sar--isearch-locals)
    (kill-local-variable variable))
  (setq my/sar--isearch-bounds nil))

(defun my/sar--isearch-refresh ()
  "Re-run the search in place after a toggle changed what it means."
  (when (bound-and-true-p isearch-mode)
    ;; Forget the last lazy-highlight pass.
    (setq isearch-lazy-highlight-last-string nil
          isearch-adjusted t
          isearch-success t)
    (isearch-update))
  (when (fboundp 'my/tool-bar-refresh)
    (my/tool-bar-refresh)))

;;;###autoload
(defun my/sar-find (&optional backward)
  "Search this buffer.  With BACKWARD, start off searching upwards.

Honours the region: with one active the search is confined to it, the way the
replace commands are."
  (interactive "P")
  (setq my/sar--isearch-bounds (my/sar--region-bounds))
  (when my/sar--isearch-bounds (deactivate-mark))
  (dolist (variable my/sar--isearch-locals)
    (set (make-local-variable variable) (symbol-value variable)))
  (setq-local isearch-search-fun-function #'my/sar--isearch-search-fun
              isearch-filter-predicate #'my/sar--isearch-filter
              ;; Every match in the buffer lit, not just the screenful, and a "3/17" counter.
              lazy-highlight-buffer t
              isearch-lazy-count t
              ;; Wrap without the "Overwrapped" stop: nothing here searches in one direction only.
              isearch-wrap-pause 'no)
  (add-hook 'isearch-mode-end-hook #'my/sar--isearch-cleanup nil t)
  (isearch-mode (not backward)))

;;;###autoload
(defun my/sar-toggle-case ()
  "Toggle whether the search cares about case."
  (interactive)
  (setq my/sar-case-sensitive (not my/sar-case-sensitive))
  (my/sar--isearch-refresh)
  (my/sar--panel-restyle)
  (message "Case %s" (if my/sar-case-sensitive "sensitive" "insensitive")))

;;;###autoload
(defun my/sar-toggle-whole-word ()
  "Toggle whether the search only matches whole words."
  (interactive)
  (setq my/sar-whole-word (not my/sar-whole-word))
  (my/sar--isearch-refresh)
  (my/sar--panel-restyle)
  (message "Whole words %s" (if my/sar-whole-word "only" "and parts")))

(defconst my/sar--styles '(plain wildcard regex)
  "The styles, in the order the picker offers them.")

;;;###autoload
(defun my/sar-set-style (style)
  "Read the query as STYLE from now on."
  (interactive
   (list (intern (completing-read
                  (format-prompt "Search style" my/sar-style)
                  (mapcar #'symbol-name my/sar--styles) nil t nil nil
                  (symbol-name my/sar-style)))))
  (setq my/sar-style style)
  (my/sar--isearch-refresh)
  (my/sar--panel-restyle)
  (message "Search style: %s" style))

;;;###autoload
(defun my/sar-cycle-style ()
  "Step to the next search style."
  (interactive)
  (my/sar-set-style
   (or (nth (1+ (or (seq-position my/sar--styles my/sar-style) -1)) my/sar--styles)
       (car my/sar--styles))))

(defun my/sar-style-label ()
  "The style picker's label, for the tool bar."
  (concat " " (symbol-name my/sar-style) " \N{BLACK DOWN-POINTING SMALL TRIANGLE} "))

;;;; Scope, roots and file filters

(defcustom my/sar-panel-width 52
  "Columns for the search panel."
  :type 'natnum)

(defcustom my/sar-panel-side 'right
  "Which edge of the frame the search panel sits on."
  :type '(choice (const :tag "Right (VS Code)" right) (const :tag "Left" left)))

(defcustom my/sar-max-depth nil
  "How far below the search root to descend, or nil for no limit."
  :type '(choice (const :tag "No limit" nil) (natnum :tag "Levels")))

(defcustom my/sar-search-file-regexp ""
  "Only files whose path matches this are searched.  Empty means all of them.

Matched against the path relative to the search root, and read in the same
dialect as the query itself."
  :type 'string)

(defcustom my/sar-replace-file-regexp ""
  "Only files whose path matches this are replaced in.  Empty means all searched.

A separate filter from `my/sar-search-file-regexp' on purpose: a file can be
worth reading through and not worth rewriting.  Files it excludes still show
their matches, greyed, and Replace All steps over them."
  :type 'string)

(defcustom my/sar-max-hits 5000
  "Stop collecting after this many matches, so a stray query cannot hang Emacs."
  :type 'natnum)

(defcustom my/sar-rg-extra-arguments '("--hidden" "--glob" "!.git/")
  "Arguments handed to every ripgrep run, before the pattern.

`--hidden' is not optional in practice: ripgrep skips dot-directories by
default, so without it a project living in one -- `~/dotfiles/.emacs.d', say --
returns nothing at all and looks broken.  Excluding `.git/' is the other half
of that bargain, since `--hidden' would otherwise send it through the object
store.  Files ignored by version control stay ignored either way."
  :type '(repeat string))

(defun my/sar--project-root (&optional directory)
  "The project root DIRECTORY belongs to, or DIRECTORY itself.

Asks about DIRECTORY rather than letting `project-current' fall back on
`default-directory' -- the same reasoning as `my/python--root' in m/py.el, and
the same shape."
  (let ((directory (expand-file-name (or directory default-directory))))
    (file-name-as-directory
     (or (when-let* (((require 'project nil t))
                     (project (project-current nil directory)))
           (expand-file-name (project-root project)))
         directory))))

(defun my/sar--short-name (directory)
  "DIRECTORY's own name, without the path leading to it."
  (let ((name (file-name-nondirectory
               (directory-file-name (expand-file-name directory)))))
    (if (string-empty-p name) "/" name)))

(defun my/sar--path-filter (pattern)
  "PATTERN as an Emacs regexp for matching paths, or nil when it is empty."
  (unless (string-empty-p (string-trim (or pattern "")))
    (condition-case nil (my/sar--matcher pattern) (error nil))))

(defun my/sar--path-matches-p (filter relative)
  "Non-nil when FILTER is absent or matches RELATIVE."
  (or (null filter)
      (let ((case-fold-search nil))
        (and (string-match-p filter relative) t))))

;;;; Matches

(cl-defstruct (my/sar-hit (:constructor my/sar--hit-make) (:copier nil))
  "One matching line."
  file      ; absolute
  line      ; 1-based
  text      ; the line, without its newline
  spans)    ; ((BEG . END) ...) into TEXT

(cl-defstruct (my/sar-group (:constructor my/sar--group-make) (:copier nil))
  "Every match in one file."
  file relative hits replaceable)

(defun my/sar--spans (regexp text)
  "Every match of REGEXP in TEXT, as a list of (BEG . END)."
  (let ((case-fold-search (not my/sar-case-sensitive))
        (start 0)
        (spans nil))
    (while (and (<= start (length text))
                (string-match regexp text start))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (if (= beg end)
            (setq start (1+ beg))
          (push (cons beg end) spans)
          (setq start end))))
    (nreverse spans)))

(defun my/sar--group-hits (hits root regexp)
  "HITS, grouped by file, in the order the files first appeared."
  (let ((replace-filter (my/sar--path-filter my/sar-replace-file-regexp))
        (groups nil))
    (dolist (hit hits)
      (let* ((file (my/sar-hit-file hit))
             (group (seq-find (lambda (g) (equal (my/sar-group-file g) file)) groups)))
        (unless group
          (let ((relative (file-relative-name file root)))
            (setq group (my/sar--group-make
                         :file file :relative relative :hits nil
                         :replaceable (my/sar--path-matches-p replace-filter relative)))
            (push group groups)))
        (setf (my/sar-hit-spans hit) (my/sar--spans regexp (my/sar-hit-text hit)))
        (push hit (my/sar-group-hits group))))
    (dolist (group groups)
      (setf (my/sar-group-hits group) (nreverse (my/sar-group-hits group))))
    (nreverse groups)))

;;;; Searching one buffer

(defun my/sar--scan-buffer (regexp buffer &optional bounds)
  "Every match of REGEXP in BUFFER, as a list of `my/sar-hit'.

BOUNDS confines the scan the way `my/sar--region-bounds' describes."
  (with-current-buffer buffer
    (save-excursion
      (let ((case-fold-search (not my/sar-case-sensitive))
            (file (or (buffer-file-name) (buffer-name)))
            (end (if bounds (cdr bounds) (point-max)))
            (hits nil)
            (searching t))
        (goto-char (if bounds (car bounds) (point-min)))
        (while (and searching
                    (< (length hits) my/sar-max-hits)
                    (re-search-forward regexp end t))
          (if (= (match-beginning 0) (match-end 0))
              (if (< (point) end) (forward-char 1) (setq searching nil))
            (let ((line (line-number-at-pos (match-beginning 0))))
              ;; One entry per line, however many matches it holds: the spans are worked out from the line's.
              (unless (and hits (= (my/sar-hit-line (car hits)) line))
                (push (my/sar--hit-make
                       :file file :line line
                       :text (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                      hits))
              (goto-char (max (match-end 0) (1+ (match-beginning 0)))))))
        (nreverse hits)))))

;;;; Searching files, through ripgrep

(defun my/sar--rg-program ()
  "Path to ripgrep, or a message saying where to get it."
  (or (executable-find "rg")
      (user-error "Searching files needs ripgrep (rg) on PATH")))

(defun my/sar--rg-pattern (query)
  "QUERY in ripgrep's dialect.

Only the wildcard style needs building: plain goes over as-is under
`--fixed-strings', and the regex style's supported subset is a subset of Rust's
regex syntax too.  Ripgrep is a prefilter here -- every match position is
worked out again with Emacs's own engine -- so a dialect corner costs at worst
one line re-checked and thrown away."
  (pcase my/sar-style
    ('plain query)
    ('wildcard (mapconcat (lambda (character)
                            (pcase character
                              (?* ".*")
                              (?? ".")
                              (_ (regexp-quote (char-to-string character)))))
                          query ""))
    (_ query)))

(defun my/sar--rg-arguments (query root)
  "The ripgrep command line for QUERY under ROOT."
  (append
   (list "--json" "--line-number" "--no-heading" "--color" "never")
   (copy-sequence my/sar-rg-extra-arguments)
   (when (eq my/sar-style 'plain) (list "--fixed-strings"))
   (list (if my/sar-case-sensitive "--case-sensitive" "--ignore-case"))
   (when my/sar-whole-word (list "--word-regexp"))
   (when (natnump my/sar-max-depth)
     (list "--max-depth" (number-to-string my/sar-max-depth)))
   (list "--regexp" (my/sar--rg-pattern query) "--" (expand-file-name root))))

(defun my/sar--rg-record (line)
  "One JSON line from ripgrep as (FILE LINE TEXT), or nil for anything else."
  (when (string-prefix-p "{" line)
    (when-let* ((object (condition-case nil
                            (json-parse-string line :object-type 'plist
                                               :array-type 'list)
                          (error nil)))
                ((equal (plist-get object :type) "match"))
                (data (plist-get object :data))
                (file (plist-get (plist-get data :path) :text))
                (number (plist-get data :line_number))
                (text (plist-get (plist-get data :lines) :text)))
      (list file number (string-trim-right text "[\r\n]+")))))

(defun my/sar--scan-files (query root callback)
  "Search ROOT for QUERY with ripgrep, then call CALLBACK with the hits.

Only the file, the line number and the line's text are taken from ripgrep.
Where the match sits on that line is worked out with Emacs's own regexp engine
later -- ripgrep reports byte offsets, which do not survive a non-ASCII line,
and two engines disagreeing about a pattern is a bug nobody could see."
  (let* ((program (my/sar--rg-program))
         (filter (my/sar--path-filter my/sar-search-file-regexp))
         (root (file-name-as-directory (expand-file-name root)))
         (pending "")
         (hits nil)
         (buffer (generate-new-buffer " *sar-rg*")))
    (make-process
     :name "sar-rg"
     :buffer buffer
     :noquery t
     :connection-type 'pipe
     :command (cons program (my/sar--rg-arguments query root))
     :filter
     (lambda (_process output)
       (setq pending (concat pending output))
       (let ((lines (split-string pending "\n")))
         ;; The last piece is whatever arrived without its newline yet.
         (setq pending (car (last lines)))
         (dolist (line (butlast lines))
           (when (< (length hits) my/sar-max-hits)
             (when-let* ((record (my/sar--rg-record line))
                         (file (nth 0 record))
                         (relative (file-relative-name file root))
                         ((my/sar--path-matches-p filter relative)))
               (push (my/sar--hit-make :file file :line (nth 1 record)
                                       :text (nth 2 record))
                     hits))))))
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (funcall callback (nreverse hits)))))))

;;;; Faces

(defface my/sar-heading '((t :inherit mode-line-emphasis))
  "The panel's section headings.")

(defface my/sar-file '((t :inherit font-lock-function-name-face))
  "A file's name in the results.")

(defface my/sar-file-locked '((t :inherit shadow :slant italic))
  "A file the replace filter excludes.")

(defface my/sar-count '((t :inherit shadow))
  "The number of matches beside a file.")

(defface my/sar-line-number '((t :inherit line-number))
  "A match's line number.")

(defface my/sar-match '((t :inherit match))
  "The matched text inside a result line.")

(defface my/sar-hover '((t :inherit highlight :extend t))
  "The result row under the pointer.")

(defface my/sar-status '((t :inherit shadow))
  "The line reporting what the last search or replace did.")

;;;; The panel

(defconst my/sar--buffer-name " *search*"
  "Name of the search panel's buffer.  Leading space: it is furniture, and
m/tabs.el's exempt list skips such names, so it never takes a tab of its own.")

(defvar-local my/sar--origin nil
  "The buffer the panel searches under the `file' scope.")

(defvar-local my/sar--root nil
  "The directory the panel searches under the other scopes.")

(defvar-local my/sar--scope 'project
  "What the panel searches: `file', `folder' or `project'.")

(defvar-local my/sar--query "" "The query as last typed.")
(defvar-local my/sar--replacement "" "The replacement as last typed.")
(defvar-local my/sar--groups nil "Results, as `my/sar-group's.")
(defvar-local my/sar--collapsed nil "Hash of file -> collapsed.")
(defvar-local my/sar--more nil "Non-nil while the extra filters are showing.")
(defvar-local my/sar--status "" "What the last search or replace did.")
(defvar-local my/sar--results-start nil "Marker where the results begin.")
(defvar-local my/sar--process nil "The running ripgrep, or nil.")
(defvar-local my/sar--touched nil "Buffers the last replace changed.")
(defvar-local my/sar--last-query nil "Query as of the last live pass.")
(defvar-local my/sar--report nil
  "What the last replace did, held across the re-search that follows it.
Without this the count would be drawn and then immediately overwritten by the
status line of the search that refreshes the results.")
(defvar-local my/sar--widgets nil "Alist of NAME -> widget.")

(defvar my/sar--rendering nil
  "Bound non-nil while the panel is redrawing, to keep callbacks out.")

(defun my/sar--origin-directory ()
  "The directory the searched buffer sits in."
  (or (and (buffer-live-p my/sar--origin)
           (buffer-local-value 'default-directory my/sar--origin))
      default-directory))

(defun my/sar--panel-buffer ()
  "The panel's buffer, created if need be."
  (or (get-buffer my/sar--buffer-name)
      (with-current-buffer (get-buffer-create my/sar--buffer-name)
        (my/sar-mode)
        (current-buffer))))

(defvar my/sar--saved-width nil
  "Panel width as last dragged to, so a hand-chosen size survives.

Not persisted: it lasts the session, the way the terminal panel's height does.
`my/sar-panel-width' is what a fresh Emacs starts from.")

(defun my/sar--width ()
  "Width to ask for: whatever it was last, else `my/sar-panel-width'."
  (or my/sar--saved-width my/sar-panel-width))

(defun my/sar--remember-width (&rest _)
  "Record the panel's width for the next time it is shown."
  (when-let* ((window (sidepanel-window 'search)))
    (setq my/sar--saved-width (window-total-width window))))

(sidepanel-define 'search
  :buffer-function #'my/sar--panel-buffer
  :owner-p (lambda (buffer) (eq buffer (get-buffer my/sar--buffer-name)))
  :side (lambda () my/sar-panel-side)
  :slot 0
  :size #'my/sar--width
  :fixed 'width
  :resizable t
  :reachable t
  :on-hide #'my/sar--remember-width)

(defun my/sar--panel-body-width ()
  "Columns available inside the panel."
  (if-let* ((window (sidepanel-window 'search)))
      (window-body-width window)
    (my/sar--width)))

(defvar-local my/sar--drawn-width nil
  "Width the header was last laid out for.")

(defun my/sar--track-size (frame)
  "Remember a dragged width and refit the fields, for FRAME.
On `window-size-change-functions'."
  (when-let* ((window (sidepanel-window 'search))
              ((eq (window-frame window) frame))
              (width (window-total-width window)))
    (setq my/sar--saved-width width)
    (when-let* ((buffer (window-buffer window))
                ((buffer-live-p buffer))
                ((not (equal (buffer-local-value 'my/sar--drawn-width buffer)
                             (window-body-width window)))))
      ;; Off a timer: this hook runs out of redisplay.
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (unless my/sar--rendering (my/sar--render)))))))))

;;;;; Header

(defun my/sar--widget (name)
  "The header widget registered as NAME."
  (cdr (assq name my/sar--widgets)))

(defun my/sar--widget-value (name)
  "NAME's value as a string, or the empty string when it is not showing."
  (if-let* ((widget (my/sar--widget name)))
      (or (widget-value widget) "")
    ""))

(defun my/sar--read-header ()
  "Copy every header field back into the variables it stands for."
  (setq my/sar--query (my/sar--widget-value 'query)
        my/sar--replacement (my/sar--widget-value 'replacement))
  (when my/sar--more
    (let ((depth (string-trim (my/sar--widget-value 'depth))))
      (setq my/sar-max-depth (and (string-match-p "\\`[0-9]+\\'" depth)
                                  (string-to-number depth))))
    (setq my/sar-search-file-regexp (string-trim (my/sar--widget-value 'include))
          my/sar-replace-file-regexp (string-trim (my/sar--widget-value 'exclude)))))

(defun my/sar--field (name value size)
  "Create an editable field called NAME holding VALUE, SIZE columns wide."
  (push (cons name
              (widget-create 'editable-field
                             :size size
                             :value (or value "")
                             :action (lambda (&rest _) (my/sar-search))))
        my/sar--widgets))

(defun my/sar--insert-header ()
  "Draw the query, the replacement, the toggles and the filters."
  (let* ((width (my/sar--panel-body-width))
         (field-size (max 8 (- width 14))))
    (setq my/sar--drawn-width width)
    (widget-insert (propertize " SEARCH" 'face 'my/sar-heading) "  ")
    (push (cons 'case
                (widget-create 'toggle
                               :value my/sar-case-sensitive
                               :on "[Aa]" :off " Aa "
                               :format "%[%v%]"
                               :help-echo "Match case"
                               :notify (lambda (widget &rest _)
                                         (setq my/sar-case-sensitive (widget-value widget))
                                         (my/sar-search))))
          my/sar--widgets)
    (widget-insert " ")
    (push (cons 'word
                (widget-create 'toggle
                               :value my/sar-whole-word
                               :on "[ab|]" :off " ab| "
                               :format "%[%v%]"
                               :help-echo "Whole words only"
                               :notify (lambda (widget &rest _)
                                         (setq my/sar-whole-word (widget-value widget))
                                         (my/sar-search))))
          my/sar--widgets)
    (widget-insert " ")
    (push (cons 'style
                (widget-create 'menu-choice
                               :value my/sar-style
                               :tag "style"
                               :format "%[%v%]"
                               :help-echo "How the query is read"
                               :notify (lambda (widget &rest _)
                                         (setq my/sar-style (widget-value widget))
                                         (my/sar-search))
                               '(item :tag "plain" :value plain :format "%t")
                               '(item :tag "wildcard" :value wildcard :format "%t")
                               '(item :tag "regex" :value regex :format "%t")))
          my/sar--widgets)
    (widget-insert (propertize " \N{BLACK DOWN-POINTING SMALL TRIANGLE}\n"
                               'face 'my/sar-count))

    (my/sar--field 'query my/sar--query field-size)
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (my/sar-search))
                   "Find")
    (widget-insert "\n")

    (my/sar--field 'replacement my/sar--replacement field-size)
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (my/sar-replace-all))
                   "Repl")
    (widget-insert "\n")

    (widget-create 'push-button
                   ;; The one control that really does change the header's shape.
                   :notify (lambda (&rest _)
                             (my/sar--read-header)
                             (setq my/sar--more (not my/sar--more))
                             (let ((buffer (current-buffer)))
                               (run-at-time 0 nil
                                            (lambda ()
                                              (when (buffer-live-p buffer)
                                                (with-current-buffer buffer
                                                  (my/sar--render)))))))
                   (if my/sar--more "\N{DOWNWARDS ARROW} less" "\N{RIGHTWARDS ARROW} more"))
    (widget-insert "\n")
    ;; Each scope says what it would actually search.
    (let* ((directory (my/sar--origin-directory))
           (folder (my/sar--short-name directory))
           (project (my/sar--short-name (my/sar--project-root directory)))
           (room (max 6 (/ (- width 18) 2))))
      (push (cons 'scope
                  (widget-create
                   'radio-button-choice
                   :value my/sar--scope
                   :entry-format "%b %v  "
                   :help-echo (format "buffer / %s / %s"
                                      (abbreviate-file-name directory)
                                      (abbreviate-file-name
                                       (my/sar--project-root directory)))
                   :notify (lambda (widget &rest _)
                             (setq my/sar--scope (widget-value widget))
                             (my/sar-search))
                   '(item :tag "file" :value file :format "%t")
                   `(item :tag ,(concat "folder "
                                        (truncate-string-to-width folder room 0 nil t))
                          :value folder :format "%t")
                   `(item :tag ,(concat "project "
                                        (truncate-string-to-width project room 0 nil t))
                          :value project :format "%t")))
            my/sar--widgets))
    (widget-insert "\n")

    (when my/sar--more
      (widget-insert " depth   ")
      (my/sar--field 'depth
                     (if (natnump my/sar-max-depth)
                         (number-to-string my/sar-max-depth) "")
                     (max 4 (- width 12)))
      (widget-insert "\n search  ")
      (my/sar--field 'include my/sar-search-file-regexp (max 4 (- width 12)))
      (widget-insert "\n replace ")
      (my/sar--field 'exclude my/sar-replace-file-regexp (max 4 (- width 12)))
      (widget-insert "\n"))))

;;;;; Results

;; These live on the results text rather than in the major mode's map.
(defvar-keymap my/sar-results-map
  :doc "Keymap over the results, where single-letter keys are safe."
  "<down-mouse-1>" #'ignore
  "n" #'next-line
  "p" #'previous-line
  "M-n" #'my/sar-next-file
  "M-p" #'my/sar-previous-file
  "g" #'my/sar-search
  "q" #'my/sar-panel-hide
  "TAB" #'my/sar-toggle-group)

(defvar-keymap my/sar-group-map
  :doc "Keymap on a file heading in the results."
  :parent my/sar-results-map
  "<mouse-1>" #'my/sar-mouse-toggle-group
  "RET" #'my/sar-toggle-group)

(defvar-keymap my/sar-hit-map
  :doc "Keymap on a matching line in the results."
  :parent my/sar-results-map
  "<mouse-1>" #'my/sar-mouse-visit
  "RET" #'my/sar-visit)

(defun my/sar--hit-at (&optional position)
  "The hit on the line at POSITION, or nil."
  (get-text-property (or position (point)) 'my/sar-hit))

(defun my/sar--group-at (&optional position)
  "The group on the line at POSITION, or nil."
  (get-text-property (or position (point)) 'my/sar-group))

(defun my/sar--collapsed-p (group)
  "Non-nil when GROUP is collapsed."
  (and my/sar--collapsed
       (gethash (my/sar-group-file group) my/sar--collapsed)))

(defun my/sar--insert-group (group)
  "Draw GROUP's heading."
  (let* ((collapsed (my/sar--collapsed-p group))
         (replaceable (my/sar-group-replaceable group))
         (count (length (my/sar-group-hits group)))
         (label (concat (if collapsed "\N{BLACK RIGHT-POINTING SMALL TRIANGLE} "
                          "\N{BLACK DOWN-POINTING SMALL TRIANGLE} ")
                        (my/sar-group-relative group)
                        (unless replaceable " \N{LOCK}")))
         (start (point)))
    (insert (propertize label 'face (if replaceable 'my/sar-file 'my/sar-file-locked)))
    (insert (propertize " " 'display
                        `(space :align-to (- right ,(+ 1 (length (number-to-string count)))))))
    (insert (propertize (number-to-string count) 'face 'my/sar-count))
    (insert (propertize " " 'display '(space :align-to right)))
    (insert "\n")
    (add-text-properties
     start (1- (point))
     `( my/sar-group ,group
        mouse-face my/sar-hover
        keymap ,my/sar-group-map
        help-echo ,(concat (my/sar-group-file group)
                           (unless replaceable
                             "\nexcluded by the replace filter")
                           "\nmouse-1: collapse")))))

(defun my/sar--insert-hit (hit)
  "Draw one matching line for HIT."
  (let* ((number (format "%5d  " (my/sar-hit-line hit)))
         (text (string-trim-left (my/sar-hit-text hit)))
         (shift (- (length (my/sar-hit-text hit)) (length text)))
         (start (point)))
    (insert (propertize number 'face 'my/sar-line-number))
    (let ((text-start (point)))
      (insert text)
      ;; The spans were measured against the untrimmed line.
      (dolist (span (my/sar-hit-spans hit))
        (let ((beg (+ text-start (- (car span) shift)))
              (end (+ text-start (- (cdr span) shift))))
          (when (and (>= beg text-start) (<= end (point)))
            (put-text-property beg end 'face 'my/sar-match)))))
    ;; Padded out to the window edge, and then the newline is deliberately left outside the.
    (insert (propertize " " 'display '(space :align-to right)))
    (insert "\n")
    (add-text-properties
     start (1- (point))
     `( my/sar-hit ,hit
        mouse-face my/sar-hover
        keymap ,my/sar-hit-map
        help-echo ,(format "%s:%d\nmouse-1: open"
                           (my/sar-hit-file hit) (my/sar-hit-line hit))))))

(defun my/sar--render-results ()
  "Redraw everything below the header."
  (when (markerp my/sar--results-start)
    (let* ((inhibit-read-only t)
           ;; widget.el puts `widget-before-change' on this buffer, and it signals `text-read-only' for any.
           (inhibit-modification-hooks t)
           (start (marker-position my/sar--results-start))
           ;; Point only needs putting back when it was down here.
           (in-results (>= (point) start))
           (line (line-number-at-pos))
           (column (current-column))
           (width (my/sar--panel-body-width)))
      (save-excursion
        (delete-region start (point-max))
        (goto-char (point-max))
        ;; The status line and its rule live down here.
        (insert (propertize (concat " " my/sar--status "\n") 'face 'my/sar-status))
        (insert (propertize (make-string width ?\N{BOX DRAWINGS LIGHT HORIZONTAL})
                            'face 'my/sar-status)
                "\n")
        (if (null my/sar--groups)
            (insert (propertize (if (string-empty-p my/sar--query)
                                    "  Type something to search for.\n"
                                  "  No matches.\n")
                                'face 'my/sar-status))
          (dolist (group my/sar--groups)
            (my/sar--insert-group group)
            (unless (my/sar--collapsed-p group)
              (dolist (hit (my/sar-group-hits group))
                (my/sar--insert-hit hit)))))
        (add-text-properties start (point-max) '(read-only t))
        ;; The results map goes only where a row has not already put its own down.
        (let ((position start))
          (while (< position (point-max))
            (let ((next (next-single-property-change position 'keymap nil (point-max))))
              (unless (get-text-property position 'keymap)
                (put-text-property position next 'keymap my/sar-results-map))
              (setq position next)))))
      (when in-results
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column column)))))

(defun my/sar--render ()
  "Draw the panel from scratch: header widgets, then results."
  (let ((my/sar--rendering t)
        (inhibit-read-only t))
    (let ((inhibit-modification-hooks t))
      (setq widget-field-new nil
            widget-field-list nil
            my/sar--widgets nil)
      (remove-overlays)
      (erase-buffer))
    (my/sar--insert-header)
    (widget-setup)
    (goto-char (point-max))
    (setq my/sar--results-start (point-marker))
    (set-marker-insertion-type my/sar--results-start nil)
    (my/sar--render-results)))

(defun my/sar--focus-field (name)
  "Put the point in the header field called NAME."
  (when-let* ((widget (my/sar--widget name)))
    (goto-char (widget-field-start widget))))

;;;;; Searching from the panel

(defun my/sar--search-root ()
  "The directory the current scope searches."
  (let ((directory (my/sar--origin-directory)))
    (pcase my/sar--scope
      ('folder (file-name-as-directory (expand-file-name directory)))
      (_ (my/sar--project-root directory)))))

(defun my/sar--finish (hits root regexp)
  "Turn HITS under ROOT into groups and redraw, reporting what was found."
  (setq my/sar--groups (my/sar--group-hits hits root regexp)
        my/sar--status
        (or (prog1 my/sar--report (setq my/sar--report nil))
            (if (null hits)
                "no matches"
              (format "%d match%s in %d file%s"
                      (length hits) (if (= (length hits) 1) "" "es")
                      (length my/sar--groups)
                      (if (= (length my/sar--groups) 1) "" "s")))))
  ;; Results only.
  (my/sar--render-results))

;;;###autoload
(defun my/sar-search ()
  "Run the panel's query."
  (interactive)
  (with-current-buffer (my/sar--panel-buffer)
    (my/sar--read-header)
    (when (process-live-p my/sar--process)
      (delete-process my/sar--process))
    (setq my/sar--last-query my/sar--query)
    (if (string-empty-p (string-trim my/sar--query))
        (progn (setq my/sar--groups nil my/sar--status "") (my/sar--render-results))
      (condition-case err
          (let ((regexp (my/sar--matcher my/sar--query))
                (root (my/sar--search-root)))
            (setq my/sar--root root)
            (if (eq my/sar--scope 'file)
                (let ((buffer (if (buffer-live-p my/sar--origin)
                                  my/sar--origin
                                (current-buffer))))
                  (my/sar--finish (my/sar--scan-buffer regexp buffer)
                                  (file-name-directory
                                   (or (buffer-file-name buffer) default-directory))
                                  regexp))
              (setq my/sar--status
                    (or my/sar--report "searching\N{HORIZONTAL ELLIPSIS}"))
              (my/sar--render)
              (let ((panel (current-buffer)))
                (setq my/sar--process
                      (my/sar--scan-files
                       my/sar--query root
                       (lambda (hits)
                         (when (buffer-live-p panel)
                           (with-current-buffer panel
                             (setq my/sar--process nil)
                             (my/sar--finish hits root regexp)))))))))
        (user-error
         (setq my/sar--groups nil
               my/sar--status (error-message-string err))
         (my/sar--render-results))))))

(defun my/sar--sync-widgets ()
  "Push the option values into the header widgets already on screen."
  (let ((my/sar--rendering t))
    (pcase-dolist (`(,name . ,value) `((case . ,my/sar-case-sensitive)
                                       (word . ,my/sar-whole-word)
                                       (style . ,my/sar-style)))
      (when-let* ((widget (my/sar--widget name))
                  ((not (equal (widget-value widget) value))))
        (widget-value-set widget value)))))

(defun my/sar--panel-restyle ()
  "Follow a toggle that was flipped from outside the panel."
  (when-let* (((not my/sar--rendering))
              (buffer (get-buffer my/sar--buffer-name)))
    (with-current-buffer buffer
      ;; The widgets are updated in place rather than rebuilt: a rebuild is what breaks a click that is.
      (my/sar--sync-widgets)
      ;; The search itself only when there is something to look at.
      (when (get-buffer-window buffer t)
        (my/sar-search)))))

;;;;; Moving about

(defun my/sar--event-target (event)
  "Return (BUFFER . POSITION) for where EVENT landed, or nil.

A click in a window that is not the selected one still runs the command bound
at the click, but leaves the current buffer alone -- so reading a text property
without going to the clicked window's buffer first would read the wrong buffer,
which is why a result was only clickable while the panel had focus."
  (let* ((start (event-start event))
         (window (posn-window start))
         (position (posn-point start)))
    (when (and (windowp window) (numberp position) (window-live-p window))
      (cons (window-buffer window) position))))

(defun my/sar--target-window ()
  "A window to show a match in -- never the panel or another side window."
  (or (and (not (window-parameter (selected-window) 'window-side))
           (not (window-minibuffer-p))
           (selected-window))
      (seq-find (lambda (window)
                  (not (window-parameter window 'window-side)))
                (window-list nil 'no-minibuf))
      (selected-window)))

(defun my/sar-visit (&optional hit)
  "Open HIT, or the one at point, in the other window."
  (interactive)
  (when-let* ((hit (or hit (my/sar--hit-at))))
    (let ((window (my/sar--target-window)))
      (select-window window)
      (if (file-exists-p (my/sar-hit-file hit))
          (find-file (my/sar-hit-file hit))
        (when-let* ((buffer (get-buffer (my/sar-hit-file hit))))
          (switch-to-buffer buffer)))
      (goto-char (point-min))
      (forward-line (1- (my/sar-hit-line hit)))
      (when-let* ((span (car (my/sar-hit-spans hit))))
        (forward-char (car span))))))

(defun my/sar-mouse-visit (event)
  "Open the match EVENT landed on, whether or not the panel has focus."
  (interactive "e")
  (when-let* ((target (my/sar--event-target event))
              (hit (with-current-buffer (car target)
                     (my/sar--hit-at (cdr target)))))
    (my/sar-visit hit)))

(defun my/sar-toggle-group (&optional group)
  "Collapse or expand GROUP, or the one at point."
  (interactive)
  (when-let* ((group (or group (my/sar--group-at))))
    (unless my/sar--collapsed
      (setq my/sar--collapsed (make-hash-table :test #'equal)))
    (let ((file (my/sar-group-file group)))
      (puthash file (not (gethash file my/sar--collapsed)) my/sar--collapsed))
    (my/sar--render-results)))

(defun my/sar-mouse-toggle-group (event)
  "Collapse or expand the file EVENT landed on, focused or not."
  (interactive "e")
  (when-let* ((target (my/sar--event-target event)))
    ;; In the panel's own buffer: the collapse state and the redraw are both buffer-local.
    (with-current-buffer (car target)
      (my/sar-toggle-group (my/sar--group-at (cdr target))))))

(defun my/sar-next-file ()
  "Move to the next file heading."
  (interactive)
  (let ((position (next-single-property-change (point) 'my/sar-group)))
    (while (and position (not (my/sar--group-at position)))
      (setq position (next-single-property-change position 'my/sar-group)))
    (when position (goto-char position))))

(defun my/sar-previous-file ()
  "Move to the previous file heading."
  (interactive)
  (let ((position (previous-single-property-change (point) 'my/sar-group)))
    (while (and position (not (my/sar--group-at position)))
      (setq position (previous-single-property-change position 'my/sar-group)))
    (when position (goto-char position))))

;;;; Replacing across files

(defun my/sar--replace-groups (groups)
  "Replace in every replaceable file in GROUPS.  Returns (COUNT . BUFFERS).

Every file is opened, edited between a pair of `undo-boundary's -- so one undo
in that buffer takes back all of its replacements -- and left unsaved.  The
whole walk sits inside one change group made by `nconc'ing the per-buffer
handles, so an error or a C-g part way through rolls back every file rather
than leaving a project half rewritten."
  (let ((files (seq-filter #'identity
                           (mapcar (lambda (group)
                                     (and (my/sar-group-replaceable group)
                                          (my/sar-group-file group)))
                                   groups)))
        (query my/sar--query)
        (template my/sar--replacement)
        (buffers nil)
        (handle nil)
        (count 0)
        (done nil))
    (unwind-protect
        (progn
          (dolist (file files)
            (when (file-readable-p file)
              (let ((buffer (find-file-noselect file)))
                (push buffer buffers)
                (setq handle (nconc handle (prepare-change-group buffer))))))
          (setq buffers (nreverse buffers))
          (activate-change-group handle)
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (undo-boundary)
              (setq count (+ count (my/sar--replace-in-buffer query template)))
              (undo-boundary)))
          (setq done t))
      (if done
          (accept-change-group handle)
        (cancel-change-group handle)))
    (cons count buffers)))

;;;###autoload
(defun my/sar-replace-all ()
  "Replace every match the panel is showing, leaving the buffers unsaved."
  (interactive)
  (with-current-buffer (my/sar--panel-buffer)
    (my/sar--read-header)
    (cond
     ((null my/sar--groups) (user-error "Nothing to replace -- search first"))
     ((string-empty-p my/sar--query) (user-error "No query"))
     (t
      (let* ((skipped (seq-count (lambda (g) (not (my/sar-group-replaceable g)))
                                 my/sar--groups))
             (result (if (eq my/sar--scope 'file)
                         (let ((buffer (if (buffer-live-p my/sar--origin)
                                           my/sar--origin
                                         (current-buffer))))
                           (cons (with-current-buffer buffer
                                   (my/sar--replace-in-buffer
                                    my/sar--query my/sar--replacement))
                                 (list buffer)))
                       (my/sar--replace-groups my/sar--groups)))
             (count (car result))
             (buffers (cdr result)))
        (setq my/sar--touched (seq-filter #'buffer-live-p buffers))
        (when (fboundp 'undo-fu-disable-checkpoint)
          (dolist (buffer my/sar--touched)
            (with-current-buffer buffer (undo-fu-disable-checkpoint))))
        (setq my/sar--report
              (format "%d replaced in %d file%s, unsaved%s"
                      count (length my/sar--touched)
                      (if (= (length my/sar--touched) 1) "" "s")
                      (if (> skipped 0) (format " (%d skipped)" skipped) "")))
        (setq my/sar--status my/sar--report)
        (my/sar-search))))))

;;;###autoload
(defun my/sar-save-all ()
  "Save every buffer the last replace changed."
  (interactive)
  (with-current-buffer (my/sar--panel-buffer)
    (let ((saved 0))
      (dolist (buffer my/sar--touched)
        (when (and (buffer-live-p buffer) (buffer-modified-p buffer))
          (with-current-buffer buffer (save-buffer))
          (setq saved (1+ saved))))
      (setq my/sar--status (format "%d file%s saved" saved (if (= saved 1) "" "s")))
      (my/sar--render))))

;;;###autoload
(defun my/sar-revert-all ()
  "Throw away the last replace, in every buffer it changed."
  (interactive)
  (with-current-buffer (my/sar--panel-buffer)
    (let ((reverted 0))
      (dolist (buffer my/sar--touched)
        (when (and (buffer-live-p buffer) (buffer-modified-p buffer)
                   (buffer-file-name buffer))
          (with-current-buffer buffer (revert-buffer 'ignore-auto 'noconfirm))
          (setq reverted (1+ reverted))))
      (setq my/sar--touched nil
            my/sar--status (format "%d file%s reverted"
                                   reverted (if (= reverted 1) "" "s")))
      (my/sar-search))))

;;;; The panel's mode

(defvar-keymap my/sar-mode-map
  :doc "Keymap for `my/sar-mode'."
  :parent widget-keymap
  "C-c C-c" #'my/sar-search
  "C-c C-r" #'my/sar-replace-all
  "C-c C-s" #'my/sar-save-all
  "C-c C-k" #'my/sar-revert-all
  "C-c C-q" #'my/sar-panel-hide
  "M-n" #'my/sar-next-file
  "M-p" #'my/sar-previous-file)

(define-derived-mode my/sar-mode fundamental-mode "Search"
  "The search and replace panel.

Not `special-mode': the query and replacement are real editable fields, which
needs a writable buffer.  The results below them carry a `read-only' property
of their own instead."
  :interactive nil
  ;; `:keep-cursor', because unlike the other panels the point really does enter this one.
  (sidepanel-setup-buffer :keep-cursor t :undo t)
  ;; Clamping runs first, so a live search never reads a query the point was about to be moved out.
  (add-hook 'post-command-hook #'my/sar--clamp-point nil t)
  (add-hook 'post-command-hook #'my/sar--live-search t t))

(defun my/sar--clamp-point ()
  "Keep the point inside a field's text instead of out in its padding.

A widget field is padded with spaces to its full width, so clicking anywhere
past the end of what is typed leaves the point in that padding, and the next
character typed lands after a run of spaces -- giving a query of
\"          word\", which matches nothing.  Every other search box puts the
caret at the end of the text instead, and at the start when there is none."
  (when-let* ((widget (widget-field-at (point)))
              (end (widget-field-text-end widget))
              ((> (point) end)))
    (goto-char end)))

(defun my/sar--live-search ()
  "Re-run a `file' search as the query is typed.  On `post-command-hook'.

Only the buffer scope: the others spawn a process, and doing that on every
keystroke would thrash.  They wait for RET or the Find button."
  (when (and (eq my/sar--scope 'file)
             (not my/sar--rendering)
             (my/sar--widget 'query))
    (let ((typed (my/sar--widget-value 'query)))
      (unless (equal typed my/sar--last-query)
        (my/sar-search)))))

;;;; Opening the panel

(defun my/sar--panel-open (scope &optional query focus-replacement)
  "Show the panel over SCOPE, seeded with QUERY.

FOCUS-REPLACEMENT puts the point in the replacement field rather than the query."
  (let ((origin (current-buffer))
        (region (my/sar--region-bounds)))
    (with-current-buffer (my/sar--panel-buffer)
      (setq my/sar--origin origin
            my/sar--scope scope)
      (when query (setq my/sar--query query))
      ;; A region means "search in here", which is a buffer search whatever scope was asked.
      (when region (setq my/sar--scope 'file))
      (my/sar--render))
    (sidepanel-show 'search 'select)
    (with-current-buffer (my/sar--panel-buffer)
      (unless (string-empty-p (string-trim my/sar--query))
        (my/sar-search))
      (my/sar--focus-field (if focus-replacement 'replacement 'query)))))

;;;###autoload
(defun my/sar-panel-project ()
  "Search the project in the panel."
  (interactive)
  (my/sar--panel-open 'project))

;;;###autoload
(defun my/sar-panel-folder ()
  "Search this file's folder in the panel."
  (interactive)
  (my/sar--panel-open 'folder))

;;;###autoload
(defun my/sar-panel-file ()
  "Search this buffer in the panel."
  (interactive)
  (my/sar--panel-open 'file))

;;;###autoload
(defun my/sar-panel-replace ()
  "Open the panel with the point in the replacement field."
  (interactive)
  (my/sar--panel-open 'project nil 'replacement))

;;;###autoload
(defun my/sar-panel-toggle ()
  "Show the search panel, or hide it if it is up."
  (interactive)
  (if (sidepanel-visible-p 'search)
      (sidepanel-hide 'search)
    (my/sar--panel-open 'project)))

(defun my/sar-panel-hide ()
  "Hide the search panel."
  (interactive)
  (sidepanel-hide 'search))

;;;###autoload
(defun my/sar-open-in-panel ()
  "Leave the running search and carry its query into the panel."
  (interactive)
  (let ((query (if (bound-and-true-p isearch-mode) isearch-string my/sar--query)))
    (when (bound-and-true-p isearch-mode) (isearch-exit))
    (my/sar--panel-open 'file query)))

;;;###autoload
(defun my/sar-replace ()
  "Replace in this buffer, honouring the region."
  (interactive)
  (my/sar--panel-open 'file nil 'replacement))

;;;; Setup

(add-hook 'window-size-change-functions #'my/sar--track-size)

(use-package pcre2el :ensure t
  :defer t
  :commands (rxt-pcre-to-elisp))

;;;; Self-test

(defun my/sar--self-test ()
  "Exercise the matcher and the replacement templates.  Signals on failure."
  (let ((my/sar-style 'regex)
        (my/sar-whole-word nil)
        (my/sar-case-sensitive t)
        (my/sar-preserve-case nil)
        (failures 0))
    (cl-flet
        ((check (label got want)
           (unless (equal got want)
             (setq failures (1+ failures))
             (princ (format "FAIL %-40s got %S want %S\n" label got want))))
         (matches (regexp string)
           (let ((case-fold-search nil))
             (and (string-match regexp string) (match-string 0 string)))))

      ;; The regex style, through pcre2el.
      (check "grouping and alternation"
             (matches (my/sar--matcher "foo(bar|baz)qux") "xfoobazquxy") "foobazqux")
      (check "braces and classes"
             (matches (my/sar--matcher "\\d{2,4}-\\w+") "id 1234-abc!") "1234-abc")
      (check "non-greedy passes through"
             (matches (my/sar--matcher "a+?b*c?") "aaabbc") "a")
      (check "backslash-d inside a character class"
             (matches (my/sar--matcher "[a-z\\d]+") "!ab3z!") "ab3z")
      (check "backreference"
             (matches (my/sar--matcher "^(\\w+)\\s+\\1$") "abc  abc") "abc  abc")

      ;; Whole word wraps the whole pattern, not just its first branch.
      (let ((my/sar-whole-word t))
        (check "whole word wraps alternation"
               (matches (my/sar--matcher "foo|bar") "a bar b") "bar")
        (check "whole word rejects an infix"
               (matches (my/sar--matcher "foo|bar") "embargo") nil))

      ;; The other two styles.
      (let ((my/sar-style 'plain))
        (check "plain takes a dot literally"
               (matches (my/sar--matcher "a.b") "axb") nil)
        (check "plain matches itself"
               (matches (my/sar--matcher "a.b") "xa.by") "a.b"))
      (let ((my/sar-style 'wildcard))
        (check "wildcard star"
               (matches (my/sar--matcher "*.el") "init.el") "init.el")
        (check "wildcard question mark"
               (matches (my/sar--matcher "a?c") "abc") "abc")
        (check "wildcard quotes the rest"
               (matches (my/sar--matcher "a+b") "a+b") "a+b"))

      (dolist (bad '("(?=x)" "(?<!y)" "\\p{L}" "(?P<n>x)"))
        (check (format "refuses %s" bad)
               (condition-case err (progn (my/sar--matcher bad) 'no-error)
                 (user-error (and (stringp (error-message-string err)) 'user-error))
                 (error 'wrong-error))
               'user-error))

      ;; Memoisation hands back the very same string.
      (check "memoised" (eq (my/sar--matcher "a(b)c") (my/sar--matcher "a(b)c")) t)

      ;; Replacement templates.
      (check "dollar group" (my/sar--replacement "n=$1") "n=\\1")
      (check "braced group" (my/sar--replacement "${12}!") "\\12!")
      (check "whole match" (my/sar--replacement "[$&]") "[\\&]")
      (check "dollar zero is the whole match" (my/sar--replacement "$0") "\\&")
      (check "escaped dollar" (my/sar--replacement "$$5") "$5")
      (check "lone dollar" (my/sar--replacement "100$") "100$")
      (check "emacs style passes through" (my/sar--replacement "\\1") "\\1")
      (check "newline and tab" (my/sar--replacement "a\\nb\\tc") "a\nb\tc")
      (check "literal backslash" (my/sar--replacement "a\\\\b") "a\\\\b")
      (check "backslash on something else" (my/sar--replacement "a\\qb") "a\\\\qb")
      (let ((my/sar-style 'plain))
        (check "plain replacement is untouched" (my/sar--replacement "$1\\n") "$1\\n"))

      ;; End to end, in a buffer.
      (with-temp-buffer
        (insert "count=42 and count=7\n")
        (check "replace count" (my/sar--replace-in-buffer "count=(\\d+)" "n=$1") 2)
        (check "replace result" (buffer-string) "n=42 and n=7\n"))
      (with-temp-buffer
        (insert "aaa\n")
        (check "zero-width pattern terminates"
               (my/sar--replace-in-buffer "x*" "-") 0))
      (with-temp-buffer
        (insert "one two one\n")
        (let ((bounds (cons (copy-marker 1) (copy-marker 8 t))))
          (check "region scope" (my/sar--replace-in-buffer "one" "1" bounds) 1)
          (check "region result" (buffer-string) "1 two one\n")))
      (with-temp-buffer
        (let ((my/sar-case-sensitive nil))
          (insert "Foo foo\n")
          (check "case insensitive" (my/sar--replace-in-buffer "foo" "x") 2)))

      (if (zerop failures)
          (princ "sar: all checks passed\n")
        (error "sar: %d check(s) failed" failures)))))

;;; sar.el ends here
