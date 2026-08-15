;;; optex.el --- Russian OpTeX keywords: highlighting, completion, snippets -*- lexical-binding: t; -*-

;;; Commentary:
;; cyrillic optex macros from rutex.tex - highlight, completion, snippets.

;;; Code:

(require 'abbrev)
(require 'tempel)

(declare-function texmathp "texmathp" ())
(declare-function aas-mode "aas" (&optional arg))
(declare-function aas-set-snippets "aas" (name &rest args))
(declare-function aas-activate-keymap "aas" (keymap-symbol))
(declare-function aas-deactivate-keymap "aas" (keymap-symbol))
(defvar TeX-master)
(defvar tex-main-file)
(defvar optex-mode)

(defvar-local optex--russian nil
  "Non-nil if this buffer belongs to a document using the Russian layer.")

(defgroup optex nil
  "Russian OpTeX keyword support."
  :group 'tex
  :prefix "optex-")


;;;; Macro inventory

(defconst optex-macro-alist
  '(("часть" . "part")                    ("ч" . "часть")
    ("глава" . "chap")                    ("гл" . "глава")
    ("заголовок" . "sec")                 ("заг" . "заголовок")
    ("подзаголовок" . "secc")             ("подзаг" . "подзаголовок")
    ("пункт" . "seccc")                   ("пкт" . "пункт")
    ("безномера" . "nonum")               ("бн" . "безномера")
    ("безсодержания" . "notoc")           ("бс" . "безсодержания")
    ("лекция" . "lec")                    ("лек" . "лекция")
    ("титул" . "maketitle")               ("содержание" . "maketoc")
    ("конецдокумента" . "bye")
    ("новаястраница" . "vfil+break")      ("нс" . "новаястраница")
    ("метка" . "label")                   ("мет" . "метка")
    ("ссылка" . "ref")                    ("сс" . "ссылка")
    ("умссылка" . "cref")                 ("усс" . "умссылка")
    ("номерформулы" . "eqmark")           ("номформ" . "номерформулы")
    ("список" . "begitems")               ("сп" . "список")
    ("консписка" . "enditems")            ("ксп" . "консписка")
    ("стиль" . "style")                   ("номерпункта" . "itemnum")
    ("подпись" . "caption")               ("отбивка" . "cskip")
    ("картинка" . "inspic")               ("ширинакартинки" . "picw")
    ("папкакартинок" . "picdir")
    ("выделение" . "em")                  ("выд" . "выделение")
    ("жирный" . "bf")                     ("ж" . "жирный")
    ("курсив" . "it")                     ("к" . "курсив")
    ("жирныйкурсив" . "bi")               ("жк" . "жирныйкурсив")
    ("машинописный" . "tt")               ("маш" . "машинописный")
    ("прямой" . "rm")
    ("текст" . "mathbox")                 ("рамка" . "frame")
    ("код" . "code")                      ("сноска" . "fnote")
    ("вебссылка" . "ulink")               ("веб" . "вебссылка")
    ("поцентру" . "centerline")           ("справа" . "rightline")
    ("слева" . "leftline")                ("масштаб" . "typoscale")
    ("Акцент" . "Accent")                 ("Серый" . "Grey")
    ("названиекурса" . "coursetitle")     ("лектор" . "lecturer")
    ("окурсе" . "courseinfo")             ("скомпилировано" . "compiled")
    ("нумерациястраниц" . "pagenumbers")  ("нумстр" . "нумерациястраниц")
    ("безнумерациистраниц" . "nopagenumbers")
    ("безнумстр" . "безнумерациистраниц")
    ("номерстраницы" . "pageno")
    ("руимя" . "alias declarator")        ("новыйблок" . "newthm")
    ("стилькурсив" . "thmplain")          ("стильпрямой" . "thmdefinition")
    ("теор" . "Теорема")                  ("лем" . "Лемма")
    ("опр" . "Определение")               ("прим" . "Пример")
    ("предл" . "Предложение")             ("след" . "Следствие")
    ("зам" . "Замечание")
    ("док" . "proof")                     ("конец" . "endthm"))
  "Russian OpTeX macro names, mapped to what each one aliases.")

(defconst optex-latin-macro-alist
  '(("begitems" . "OpTeX list")      ("enditems" . "OpTeX list")
    ("style" . "item style")         ("itemnum" . "item counter")
    ("chap" . "level 1")             ("sec" . "level 2")
    ("secc" . "level 3")             ("seccc" . "level 4")
    ("nonum" . "no number")          ("notoc" . "no TOC entry")
    ("caption" . "/f or /t")         ("cskip" . "caption gap")
    ("inspic" . "insert picture")    ("picw" . "picture width")
    ("picdir" . "picture directory") ("eqmark" . "equation number")
    ("mathbox" . "text in math")     ("bbchar" . "blackboard bold")
    ("code" . "inline verbatim")     ("ulink" . "external link")
    ("fnote" . "footnote")           ("newthm" . "declare a block")
    ("lec" . "lecture separator")    ("cref" . "clever reference")
    ("thmplain" . "italic body")     ("thmdefinition" . "upright body")
    ("endthm" . "close a block")     ("proof" . "proof block"))
  "OpTeX macros with Latin names that AUCTeX does not know about.")


;;;; The \input line that names rutex.tex

(defcustom optex-rutex-input-regexp
  "^[ \t]*\\\\input[ \t]*{?[ \t]*\\([^ \t\n{}%]*rutex\\)\\(?:\\.tex\\)?\\(?:[ \t}%]\\|$\\)"
  "Regexp whose presence marks a buffer as using the Russian layer."
  :type 'regexp
  :group 'optex)

(defcustom optex-rutex-search-lines 100
  "How many lines from the top of a file to search for the \\input."
  :type 'natnum
  :group 'optex)

(defconst optex--head-bytes 6000
  "How much of another file to read when looking for its \\input line.")

(defmacro optex--with-file-head (file bytes &rest body)
  "Evaluate BODY in a temp buffer holding the first BYTES of FILE."
  (declare (indent 2) (debug (form form body)))
  `(when (file-readable-p ,file)
     (with-temp-buffer
       ;; Byte range, so a multibyte character may be cut in half at the end.
       (when (ignore-errors (insert-file-contents ,file nil 0 ,bytes) t)
         (goto-char (point-min))
         ,@body))))

(defun optex--input-rutex-path (&optional bound)
  "Return the path this buffer's rutex \\input names, as written, or nil."
  (save-excursion
    (and (re-search-forward optex-rutex-input-regexp bound t)
         (match-string-no-properties 1))))

(defun optex--input-found-p ()
  "Non-nil if `optex-rutex-input-regexp' matches near the top of this buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward optex-rutex-input-regexp
                       (line-end-position optex-rutex-search-lines) t)))

(defun optex--file-input-rutex-path (file)
  "Return the path FILE's rutex \\input names, or nil."
  (optex--with-file-head file optex--head-bytes
    (optex--input-rutex-path (line-end-position optex-rutex-search-lines))))

(defun optex--file-inputs-rutex-p (file)
  "Non-nil if FILE has the rutex \\input near its top."
  (and (optex--file-input-rutex-path file) t))

(defun optex--resolve-rutex (path dir)
  "Return PATH resolved against DIR as a readable file, or nil."
  (when (and path dir)
    (let ((f (expand-file-name path dir)))
      (seq-find #'file-readable-p
                (list f (concat f ".tex")
                      (concat (file-name-sans-extension f) ".tex"))))))

(defun optex--master-file ()
  "Return the file `TeX-master' names, with its extension, or nil."
  (when-let* ((m (and (stringp TeX-master) (expand-file-name TeX-master))))
    (seq-find #'file-readable-p (list m (concat m ".tex")))))


;;;; Scanning rutex.tex

(defconst optex--declaration-re
  "\\\\\\(?:руимя\\|новыйблок\\|_?def\\|_?let\\)[ \t]*\\\\\\([А-Яа-яЁё]+\\)"
  "Match a Cyrillic macro being declared.  Group 1 is the bare name.")

(defcustom optex-rutex-file-name "rutex.tex"
  "Name of the file holding the Russian translations."
  :type 'string
  :group 'optex)

(defcustom optex-search-depth 5
  "How many directory levels to walk up when looking for the document."
  :type 'natnum
  :group 'optex)

(defun optex--ancestor-dirs (&optional dir)
  "Return DIR and up to `optex-search-depth' directories above it. Nearest first. DIR defaults to `default-directory'."
  (let ((d (expand-file-name (or dir default-directory)))
        (n optex-search-depth)
        out)
    (while (and d (> n 0))
      (push d out)
      (setq n (1- n))
      (let ((up (file-name-directory (directory-file-name d))))
        (setq d (unless (equal up d) up))))
    (nreverse out)))

(defun optex--strip-comment (line)
  "Return LINE up to its first unescaped TeX comment character."
  (if (string-match "\\(\\`\\|[^\\\\]\\)%" line)
      (substring line 0 (match-end 1))
    line))

(defun optex--scan-declarations ()
  "Return the Cyrillic macro names declared in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (names)
      (while (not (eobp))
        (let ((code (optex--strip-comment
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (start 0))
          (while (string-match optex--declaration-re code start)
            (push (match-string 1 code) names)
            (setq start (match-end 1))))
        (forward-line 1))
      (nreverse names))))

(defun optex--rutex-under (dir)
  "Return `optex-rutex-file-name' in DIR or an immediate subdirectory, or nil."
  (seq-find
   #'file-readable-p
   (cons (expand-file-name optex-rutex-file-name dir)
         (mapcar (lambda (sub) (expand-file-name optex-rutex-file-name sub))
                 (seq-filter #'file-directory-p
                             (ignore-errors
                               (directory-files dir t "\\`[^.]" t)))))))

(defun optex--rutex-file-in-tree ()
  "Return `optex-rutex-file-name' found by walking up from this buffer."
  (seq-some #'optex--rutex-under (optex--ancestor-dirs)))

(defun optex--dirs-to-root (&optional dir)
  "Ancestor directories of DIR, cut off at the document root."
  (let (out)
    (catch 'done
      (dolist (d (optex--ancestor-dirs dir))
        (push d out)
        (when-let* ((r (optex--rutex-under d)))
          (when (equal (file-name-directory r) (file-name-as-directory d))
            (when-let* ((up (file-name-directory (directory-file-name d)))
                        ((not (equal up d))))
              (push up out)))
          (throw 'done nil))))
    (nreverse out)))

(defvar-local optex--rutex-file-cache 'unset
  "Cached result of `optex--rutex-file'; `unset' before the first look.")

(defun optex--rutex-file ()
  "Return the path of `optex-rutex-file-name', or nil if there is none."
  (if (not (eq optex--rutex-file-cache 'unset))
      optex--rutex-file-cache
    (setq optex--rutex-file-cache
          (or (optex--resolve-rutex (save-excursion
                                      (goto-char (point-min))
                                      (optex--input-rutex-path
                                       (line-end-position
                                        optex-rutex-search-lines)))
                                    default-directory)
              (when-let* ((m (optex--master-file)))
                (optex--resolve-rutex (optex--file-input-rutex-path m)
                                      (file-name-directory m)))
              (optex--rutex-file-in-tree)))))


(defconst optex-item-styles
  '(("o" . "•")            ("-" . "-")
    ("n" . "1. 2. 3.")     ("N" . "1) 2) 3)")
    ("i" . "(i) (ii)")     ("I" . "I  II  III")
    ("a" . "a) b) c)")     ("A" . "A) B) C)")
    ("x" . "▪")            ("X" . "■")
    ("d" . "слово - ...")  ("m" . "1.1. 1.2."))
  "OpTeX's own `\\style' letters, and what each one prints. From lists.opm; `m' is the nested numbering from optex-tricks.opm.")

(defconst optex--item-alias-re
  "\\\\_?slet[ \t]*{_item:\\(.\\)}[ \t]*{_item:\\(.\\)}"
  "Match an item style being aliased in rutex.tex.")

(defun optex--scan-item-styles ()
  "Return the item-style aliases declared in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (out)
      (while (not (eobp))
        (let ((code (optex--strip-comment
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
              (start 0))
          (while (string-match optex--item-alias-re code start)
            (push (cons (match-string 1 code) (match-string 2 code)) out)
            (setq start (match-end 0))))
        (forward-line 1))
      (nreverse out))))

(defvar-local optex--macros nil
  "Cached alist of completion candidates for this buffer.")

(defvar-local optex--item-styles nil
  "Cached list of (LETTER . SAMPLE) for this buffer.")

(defun optex--macros ()
  "Return the alist of macro names to offer, computed once per buffer."
  (or optex--macros
      (setq optex--macros
            (let ((found (append
                          (optex--scan-declarations)
                          (when-let* ((f (optex--rutex-file)))
                            (with-temp-buffer
                              (insert-file-contents f)
                              (optex--scan-declarations))))))
              (append optex-macro-alist
                      optex-latin-macro-alist
                      ;; whatever the sources declare but the tables above miss
                      (delq nil
                            (mapcar
                             (lambda (n)
                               (unless (assoc n optex-macro-alist)
                                 (cons n "in rutex.tex")))
                             (delete-dups found))))))))

(defun optex-item-styles ()
  "Return the `\\style' letters to offer, Russian first."
  (or optex--item-styles
      (setq optex--item-styles
            (let* ((aliases (append
                             (optex--scan-item-styles)
                             (when-let* ((f (optex--rutex-file)))
                               ;; nil bytes: whole file.
                               (optex--with-file-head f nil
                                 (optex--scan-item-styles)))))
                   (extra (delq nil
                                (mapcar
                                 (lambda (a)
                                   (unless (assoc (car a) optex-item-styles)
                                     (cons (car a)
                                           (or (cdr (assoc (cdr a)
                                                           optex-item-styles))
                                               (cdr a)))))
                                 aliases))))
              (append (seq-uniq extra (lambda (a b) (equal (car a) (car b))))
                      optex-item-styles)))))

(defun optex--invalidate ()
  "Drop the cached macro list, so the next completion re-reads the sources."
  (setq optex--macros nil
        optex--item-styles nil
        optex--rutex-file-cache 'unset))

(defun optex-refresh ()
  "Re-read the macro list and set this buffer up again from scratch."
  (interactive)
  (optex--invalidate)
  (when optex-mode
    (optex-mode -1)
    (optex-mode 1))
  (message "optex: %d macros" (length (optex--macros))))


;;;; Is this an OpTeX/Russian buffer?

(defcustom optex-rutex-file-implies-russian t
  "If non-nil, a rutex.tex anywhere above a plain-TeX file marks it Russian."
  :type 'boolean
  :group 'optex)

(defun optex--plain-tex-p ()
  "Non-nil in a plain-TeX buffer -- which is what an OpTeX file is."
  (derived-mode-p 'plain-TeX-mode 'plain-tex-mode))

(defun optex--rutex-buffer-p ()
  "Non-nil if this buffer belongs to a document using the Russian layer."
  (or (optex--input-found-p)
      (when-let* ((f (optex--master-file)))
        (and (not (equal f buffer-file-name))
             (optex--file-inputs-rutex-p f)))
      (and optex-rutex-file-implies-russian
           (optex--plain-tex-p)
           (optex--rutex-file)
           t)))


;;;; Which file is the master

(defcustom optex-set-tex-master t
  "If non-nil, point `TeX-master' at the file that \\input's this one. A file- or directory-local `TeX-master' always wins: only the stock value t, meaning \"this file is its own master\", is ever replaced."
  :type 'boolean
  :group 'optex)

(defconst optex--part-head-bytes 100000
  "How much of a candidate master to read when looking for its \\input of us.")

(defun optex--inputs-file-p (candidate file)
  "Non-nil if CANDIDATE \\input's FILE."
  (and (not (equal candidate file))
       (let* ((rel (file-relative-name (file-name-sans-extension file)
                                       (file-name-directory candidate)))
              (re (concat "^[ \t]*\\\\input[ \t]*{?[ \t]*"
                          (regexp-quote rel) "\\(?:\\.tex\\)?\\(?:[ \t}%]\\|$\\)")))
         ;; A part outside the master's own tree would give a `rel' full of `../', which no document.
         (and (not (string-prefix-p "/" rel))
              (optex--with-file-head candidate optex--part-head-bytes
                (re-search-forward re nil t))))))

(defun optex--importer-of (file)
  "Return the .tex file that \\input's FILE, or nil."
  (seq-some
   (lambda (dir)
     (seq-find (lambda (c) (optex--inputs-file-p c file))
               (ignore-errors (directory-files dir t "\\.tex\\'" t))))
   (optex--dirs-to-root (file-name-directory file))))

(defconst optex--master-chain-max 8
  "How far to follow \\input's upwards before deciding it is a cycle.")

(defun optex--find-master ()
  "Return the top-level file this buffer belongs to, or nil."
  (when-let* ((file buffer-file-name))
    (or (let ((seen (list file))
              (cur file)
              (n optex--master-chain-max)
              found)
          (while (and cur (> n 0))
            (setq n (1- n)
                  cur (optex--importer-of cur))
            ;; A file that \input's something that \input's it back would otherwise be walked forever.
            (when (member cur seen) (setq cur nil))
            (when cur (push cur seen) (setq found cur)))
          found)
        (let ((found (seq-filter
                      (lambda (c)
                        (and (not (equal c file))
                             (optex--file-inputs-rutex-p c)))
                      (mapcan (lambda (dir)
                                (ignore-errors
                                  (directory-files dir t "\\.tex\\'" t)))
                              (optex--dirs-to-root)))))
          (and found (null (cdr found)) (car found))))))

(defconst optex--own-document-re
  "^[ \t]*\\\\\\(?:bye\\|конецдокумента\\|кд\\)\\>"
  "What a plain-TeX file that is a whole document ends with.")

(defun optex--own-document-p ()
  "Non-nil if this buffer is a complete document rather than an \\input part."
  (save-excursion
    (goto-char (point-max))
    (re-search-backward optex--own-document-re nil t)))

(defun optex--set-master ()
  "Set `TeX-master' from the document layout, if it is still at its default."
  (when (and optex-set-tex-master
             (eq TeX-master t)
             buffer-file-name
             (optex--plain-tex-p)
             (not (optex--input-found-p))    ; a master needs no telling
             (not (optex--own-document-p)))
    (when-let* ((m (optex--find-master)))
      (setq-local TeX-master
                  (file-relative-name (file-name-sans-extension m))))))

(defun optex--latexenc-without-string-master (orig arg-list)
  "Around advice for `latexenc-find-file-coding-system'.  See comment above."
  (let ((TeX-master (if (and (boundp 'TeX-master) (stringp TeX-master))
                        nil
                      (and (boundp 'TeX-master) TeX-master)))
        (tex-main-file (if (and (boundp 'tex-main-file) (stringp tex-main-file))
                           nil
                         (and (boundp 'tex-main-file) tex-main-file))))
    (funcall orig arg-list)))

(advice-add 'latexenc-find-file-coding-system
            :around #'optex--latexenc-without-string-master)


;;;; Highlighting

(defcustom optex-disable-ispell-completion t
  "If non-nil, drop `ispell-completion-at-point' from TeX buffers. `text-mode' installs it and `TeX-mode' inherits it."
  :type 'boolean
  :group 'optex)

(defcustom optex-highlight-unknown-macros nil
  "If non-nil, face Cyrillic macros that are declared nowhere as warnings."
  :type 'boolean
  :group 'optex)

(defvar-local optex--font-lock-added nil
  "The keyword list handed to `font-lock-add-keywords', so it can be removed.")

(defun optex--font-lock-add ()
  "Install Cyrillic macro highlighting in this buffer."
  (let ((keywords
         (append
          `((,(concat "\\\\\\(?:" (regexp-opt (mapcar #'car (optex--macros)))
                      "\\)\\>")
             0 font-lock-keyword-face))
          (when optex-highlight-unknown-macros
            '(("\\\\[А-Яа-яЁё]+\\>" 0 font-lock-warning-face))))))
    (setq optex--font-lock-added keywords)
    ;; Prepended, not appended: font-latex's generic macro matcher has a single-character branch.
    (font-lock-add-keywords nil keywords)))

(defun optex--font-lock-remove ()
  "Remove the highlighting installed by `optex--font-lock-add'."
  (when optex--font-lock-added
    (font-lock-remove-keywords nil optex--font-lock-added)
    (setq optex--font-lock-added nil)))


;;;; Completion

(defvar optex-macro-history nil
  "Recently completed OpTeX macro names (bare, no backslash).")

(defcustom optex-macro-history-max 50
  "How many names to keep in `optex-macro-history'."
  :type 'natnum
  :group 'optex)

(defun optex--macro-bounds ()
  "Return (START . END) of the macro name before point, backslash excluded."
  (save-excursion
    (let ((end (point)))
      (skip-chars-backward "[:alpha:]")
      (and (eq (char-before) ?\\) (cons (point) end)))))

(defun optex--annotation (candidate)
  "Return the annotation string shown next to CANDIDATE."
  (when-let* ((a (cdr (assoc candidate (optex--macros)))))
    (concat "  " a)))

(defun optex--remember-macro (name)
  "Push bare macro NAME onto `optex-macro-history'."
  (when (and name (not (string-empty-p name)))
    (setq optex-macro-history
          (cons name (delete name optex-macro-history)))
    (when (> (length optex-macro-history) optex-macro-history-max)
      (setq optex-macro-history
            (seq-take optex-macro-history optex-macro-history-max)))))

(defun optex--cyrillic-p (name)
  "Non-nil if NAME is one of the Russian macro names."
  (and (> (length name) 0)
       (memq (get-char-code-property (aref name 0) 'general-category)
             '(Lu Ll))
       (<= #x400 (aref name 0) #x4FF)))

(defun optex--sort-candidates (cands)
  "Sort CANDS: recently used first, then Russian, then Latin."
  (let ((rank (make-hash-table :test #'equal))
        (i 0))
    (dolist (h optex-macro-history)
      (unless (gethash h rank)
        (puthash h i rank)
        (setq i (1+ i))))
    (sort (copy-sequence cands)
          (lambda (a b)
            (let ((ra (gethash a rank most-positive-fixnum))
                  (rb (gethash b rank most-positive-fixnum)))
              (cond
               ((/= ra rb) (< ra rb))
               ((not (eq (optex--cyrillic-p a) (optex--cyrillic-p b)))
                (and (optex--cyrillic-p a) t))
               (t (string-lessp a b))))))))

(defun optex--exit-function (string status)
  "Record STRING in macro history when completion finishes with STATUS."
  (when (memq status '(finished sole))
    (optex--remember-macro (substring-no-properties string))))

(defun optex-completion-at-point ()
  "Complete OpTeX macro names, Russian and Latin, after a backslash."
  (when-let* ((bounds (optex--macro-bounds)))
    (list (car bounds) (cdr bounds)
          (mapcar #'car (optex--macros))
          :annotation-function #'optex--annotation
          :display-sort-function #'optex--sort-candidates
          :cycle-sort-function #'optex--sort-candidates
          :exit-function #'optex--exit-function
          :company-prefix-length t
          :exclusive t)))


;;;; Outlines

(defconst optex-outline-levels
  '(("часть" . 1) ("ч" . 1) ("part" . 1)
    ("глава" . 2) ("гл" . 2) ("chap" . 2)
    ("лекция" . 2) ("лек" . 2) ("lec" . 2)
    ("заголовок" . 3) ("заг" . 3) ("sec" . 3)
    ("подзаголовок" . 4) ("подзаг" . 4) ("secc" . 4)
    ("пункт" . 5) ("пкт" . 5) ("seccc" . 5))
  "OpTeX heading commands, Russian and Latin, and their depth.")

(defconst optex--outline-prefixes
  '("безномера" "бн" "безсодержания" "бс" "nonum" "notoc")
  "Modifiers that may stand in front of a heading command.")

(defun optex--outline-regexp ()
  "Return the `outline-regexp' for an OpTeX buffer."
  (concat "^\\(?:\\\\\\(?:" (regexp-opt optex--outline-prefixes) "\\)\\>\\)*"
          "\\\\\\(" (regexp-opt (mapcar #'car optex-outline-levels)) "\\)\\>"))

(defun optex--outline-level ()
  "Return the depth of the heading `outline-regexp' just matched."
  (or (cdr (assoc (match-string 1) optex-outline-levels)) 1))

(defun optex--outline-setup ()
  "Teach `outline-minor-mode' about OpTeX headings, and switch it on."
  (when (derived-mode-p 'plain-TeX-mode 'plain-tex-mode)
    (setq-local outline-regexp (optex--outline-regexp))
    (setq-local outline-level #'optex--outline-level)
    (outline-minor-mode 1)))


;;;; List styles

(defvar optex-item-style-history nil
  "Recently chosen `\\style' letters.")

(defun optex--item-style-affix (cands)
  "Affixation function for `optex-read-item-style'.  CANDS are letters."
  (let ((styles (optex-item-styles)))
    (mapcar (lambda (c)
              (list c "   "
                    (propertize (concat "  " (or (cdr (assoc c styles)) ""))
                                'face 'completions-annotations)))
            cands)))

;;;###autoload
(defun optex-read-item-style ()
  "Read an OpTeX `\\style' letter, showing what each one prints."
  (let* ((styles (optex-item-styles))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (category . optex-item-style)
                        (affixation-function . optex--item-style-affix)
                        (cycle-sort-function . identity)
                        (display-sort-function . identity))
                    (complete-with-action action (mapcar #'car styles)
                                          string pred))))
         (choice (completing-read "Стиль списка: " table nil t nil
                                  'optex-item-style-history)))
    (and (not (string-empty-p choice)) choice)))


;;;; Snippets

(defconst optex-snippets
  '(("t" теор     "\\теор["  p "]" n r> :doc "Теорема.")
    ("l" лем      "\\лем["   p "]" n r> :doc "Лемма")
    ("d" опр      "\\опр["   p "]" n r> :doc "Определение")
    ("e" прим     "\\прим["  p "]" n r> :doc "Пример")
    ("p" предл    "\\предл[" p "]" n r> :doc "Предложение")
    ("c" след     "\\след["  p "]" n r> :doc "Следствие")
    ("r" зам      "\\зам["   p "]" n r> :doc "Замечание")
    ("P" док      "\\док" n r> n "\\конец" n :doc "Доказательство")
    ("i" список   "\\список \\стиль " (optex-read-item-style) n
     "* " r> n "\\консписка" n :doc "Список")
    ("f" рисунок  "\\подпись/р [" p "] " p n
     "\\отбивка" n r> :doc "Подпись рисунка")
    ("T" таблица  "\\подпись/т [" p "] " p n
     "\\отбивка" n r> :doc "Подпись таблицы")
    ("m" формула  "\\мет[" p "]" n "$$" n "   " r>
     n "   \\номформ" n "$$" n :doc "Нумерованное выражение")
    ("s" заг      "\\заг[" p "] " r> :doc "Заголовок")
    ("S" подзаг   "\\подзаг " r> :doc "Подзаголовок")
    ("u" пкт      "\\пкт " r> :doc "Под-подзаголовок")
    ("C" часть    "\\часть " r> :doc "Часть")
    ("h" глава    "\\глава " r> :doc "Глава")
    ("L" лек      "\\лек " r> :doc "Лекция")
    ("w" веб      "\\вебссылка[" p "]{" r> "}" :doc "Веб-ссылка")
    ("g" картинка "\\ширинакартинки=.8\\hsize "
     "\\картинка{" r> "}" :doc "Вставка картинки")
    ("a" eqalign  "$$\\eqalign{" n "   " r> " &= "
     p " \\cr" n "}$$" n :doc "Выравненные выражения")
    ("x" pmatrix  "\\pmatrix{" r> " & " p " \\cr "
     p " & " p " \\cr}" :doc "Круглая матрица")
    ("k" cases    "\\cases{" r> " & " p " \\cr "
     p " & " p " \\cr}" :doc "Фигурная скобка вокруг выражений"))
  "Russian snippets, as (KEY . TEMPLATE).  See the commentary above.")

(defconst optex-latin-snippets
  '((nil proof    "\\proof" n r> n "\\endthm" n :doc "Proof block")
    (nil begitems "\\begitems \\style " (optex-read-item-style) n
         "* " r> n "\\enditems" n :doc "Item list")
    (nil figure   "\\caption/f [" p "] " p n
         "\\cskip" n r> :doc "Figure caption")
    (nil table    "\\caption/t [" p "] " p n
         "\\cskip" n r> :doc "Table caption")
    (nil newthm   "\\newthm \\" p " {" p "} {" p "} \\thmplain" n
         :doc "Declare a block"))
  "Latin-named counterparts of `optex-snippets', same shape.")

(defconst optex-inline-snippets
  '(("вв" вебссылка     "\\вебссылка[" p "]{" p "}" :doc "Веб-ссылка")
    ("кк" код           "\\код{" p "}"              :doc "Код")
    ("тт" текст         "\\текст{" p "}"            :doc "Текст")
    ("рр" рамка         "\\рамка{" p "}"            :doc "Рамка")
    ("нн" новаястраница "\\новаястраница"           :doc "Новая страница")
    ("оо" отбивка       "\\отбивка"                 :doc "Отбивка"))
  "Inline snippets, as (ABBREV . TEMPLATE).")

(defconst optex--all-templates
  (mapcar #'cdr (append optex-snippets optex-latin-snippets
                        optex-inline-snippets))
  "Every template, keyed by name, in the shape `tempel' wants.")

(defun optex--template-doc (template)
  "Return the :doc string of TEMPLATE, or a generic one."
  (let ((tail template))
    (while (and tail (not (keywordp (car tail))))
      (setq tail (cdr tail)))
    (or (plist-get tail :doc) "OpTeX snippet.")))

(defun optex-templates ()
  "Return the templates offered in this buffer, for `tempel-template-sources'."
  (when (bound-and-true-p optex-mode)
    (mapcar #'cdr
            (if optex--russian
                (append optex-snippets optex-latin-snippets
                        optex-inline-snippets)
              optex-latin-snippets))))

(defun optex--define-snippet-commands ()
  "Define an inserting command per template, and bind the keyed ones."
  (dolist (entry (append optex-snippets optex-latin-snippets))
    (let* ((key (car entry))
           (template (cdr entry))
           (cmd (intern (format "optex-insert-%s" (car template)))))
      (defalias cmd
        (lambda ()
          (interactive)
          (tempel-insert template))
        (optex--template-doc template))
      (when key
        (keymap-set optex-prefix-map key cmd)))))


;;;; Snippets: inline abbrevs

(defun optex--snippets-enabled-p ()
  "Non-nil when an OpTeX snippet should expand at point."
  (and optex-mode
       (not (nth 4 (syntax-ppss)))
       (not (and (fboundp 'texmathp) (ignore-errors (texmathp))))))

(define-abbrev-table 'optex-mode-abbrev-table nil
  "Inline OpTeX abbreviations.
The expansions are `tempel' templates, so point lands on a real field --
which is why the old `###@###' placeholder is gone."
  :enable-function #'optex--snippets-enabled-p)

(defun optex--define-inline-abbrevs ()
  "Give every inline snippet an abbrev that expands it through `tempel'."
  (dolist (entry optex-inline-snippets)
    (let* ((template (cdr entry))
           (cmd (intern (format "optex-insert-%s" (car template)))))
      (defalias cmd
        (lambda ()
          (interactive)
          (tempel-insert template)
          t)
        (optex--template-doc template))
      (put cmd 'no-self-insert t)
      (define-abbrev optex-mode-abbrev-table (car entry) "" cmd))))


;;;; Snippets: auto-firing

(defcustom optex-auto-snippets
  '(;; TRIGGER     RUSSIAN     LATIN
    ("thm"         теор        nil)
    ("lem"         лем         nil)
    ("defn"        опр         nil)
    ("ex"          прим        nil)
    ("prop"        предл       nil)
    ("cor"         след        nil)
    ("rem"         зам         nil)
    ("pf"          док         proof)
    ("proof"       док         proof)
    ("items"       список      begitems)
    ("begitems"    список      begitems)
    ("fig"         рисунок     figure)
    ("caption"     рисунок     figure)
    ("tbl"         таблица     table)
    ("newthm"      nil         newthm))
  "Auto-firing triggers, as (TRIGGER RUSSIAN-NAME LATIN-NAME)."
  :type '(repeat (list (string :tag "Trigger (after \\)")
                       (choice (symbol :tag "Russian name") (const nil))
                       (choice (symbol :tag "Latin name") (const nil))))
  ;; `optex-auto-set-snippets' is defined below, and a customized value makes `defcustom' run this.
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (fboundp 'aas-set-snippets)
                    (fboundp 'optex--auto-set-snippets))
           (optex--auto-set-snippets)))
  :group 'optex)

(defun optex--auto-template (russian latin)
  "Return the template named RUSSIAN or LATIN, whichever fits, or nil."
  (when-let* ((name (if (and russian optex--russian) russian latin)))
    (assq name optex--all-templates)))

(defun optex--auto-set-snippets ()
  "Register `optex-auto-snippets' with `aas' under the `optex-mode' keymap."
  (apply
   #'aas-set-snippets 'optex-mode
   (mapcan
    (pcase-lambda (`(,trigger ,russian ,latin))
      (list
       :cond (lambda ()
               (and (optex--snippets-enabled-p)
                    (optex--auto-template russian latin)
                    t))
       (concat "\\" trigger)
       (lambda ()
         (interactive)
         (when-let* ((template (optex--auto-template russian latin)))
           (tempel-insert template)))))
    optex-auto-snippets)))

(with-eval-after-load 'aas (optex--auto-set-snippets))


;;;; Keys

(defvar-keymap optex-prefix-map
  :doc "Insert OpTeX/Russian snippets."
  "n"   #'tempel-next
  "b"   #'tempel-previous
  "SPC" #'tempel-insert
  "C-r" #'optex-refresh)

(optex--define-snippet-commands)
(optex--define-inline-abbrevs)

(defun optex-insert-backslash (arg)
  "Insert `\\' and open macro completion."
  (interactive "*p")
  (if (and (bound-and-true-p TeX-electric-escape)
           (fboundp 'TeX-electric-macro))
      (TeX-electric-macro)
    (self-insert-command arg)
    (when (and optex--russian (not (nth 4 (syntax-ppss))))
      (completion-at-point))))

(defvar-keymap optex-mode-map
  :doc "Keymap for `optex-mode'."
  "C-c r" optex-prefix-map
  "C-M-i" #'completion-at-point
  "\\" #'optex-insert-backslash)


;;;; The mode

;;;###autoload
(define-minor-mode optex-mode
  "Minor mode for OpTeX documents using the Russian keyword layer."
  :lighter " ru"
  :keymap optex-mode-map
  :group 'optex
  (cond
   (optex-mode
    (add-hook 'tempel-template-sources #'optex-templates nil t)
    (setq local-abbrev-table optex-mode-abbrev-table)
    (abbrev-mode 1)
    ;; Before `optex--rutex-buffer-p', which consults `TeX-master'.
    (optex--set-master)
    (optex--outline-setup)
    ;; Asked once and remembered: the auto-firing snippets consult it on every trigger to decide.
    (setq optex--russian (and (optex--rutex-buffer-p) t))
    (when (and (fboundp 'aas-activate-keymap)
               (or optex--russian (optex--plain-tex-p)))
      (aas-mode 1)
      (aas-activate-keymap 'optex-mode))
    ;; `text-mode' installs this and `TeX-mode' inherits it.
    (when optex-disable-ispell-completion
      (remove-hook 'completion-at-point-functions
                   #'ispell-completion-at-point t))
    (when optex--russian
      (optex--invalidate)
      ;; Warm the cache now so the first `\' is not stalled inside `while-no-input' scanning rutex.tex.
      (optex--macros)
      (optex--font-lock-add)
      (add-hook 'completion-at-point-functions
                #'optex-completion-at-point nil t)
      ;; A bare `\' has to offer the whole list.
      (add-hook 'after-save-hook #'optex--invalidate nil t)))
   (t
    (optex--font-lock-remove)
    (remove-hook 'tempel-template-sources #'optex-templates t)
    (remove-hook 'completion-at-point-functions #'optex-completion-at-point t)
    (remove-hook 'after-save-hook #'optex--invalidate t)
    (when (fboundp 'aas-deactivate-keymap)
      (aas-deactivate-keymap 'optex-mode))
    (abbrev-mode -1)
    (setq optex--russian nil)))
  (when font-lock-mode (font-lock-flush)))


;;;; Installation

(defun optex--tex-mode-setup ()
  "Turn on `optex-mode' once file-local variables have been applied."
  (if buffer-file-name
      (add-hook 'hack-local-variables-hook #'optex-mode nil t)
    (optex-mode 1)))

;;;###autoload
(progn
  (add-hook 'TeX-mode-hook #'optex--tex-mode-setup)
  (add-hook 'tex-mode-hook #'optex--tex-mode-setup))

(provide 'optex)
;;; optex.el ends here
