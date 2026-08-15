;;; homebrew.el --- Transient UI for Homebrew -*- lexical-binding: t; -*-

;;; Commentary:
;; transient ui for brew search/install/upgrade/uninstall.

;;; Code:

(require 'cl-lib)
(require 'goto-addr)
(require 'json)
(require 'subr-x)
(require 'transient)

;;;; Customization

(defgroup homebrew nil
  "Transient UI for Homebrew."
  :group 'external
  :prefix "homebrew-")

(defcustom homebrew-executable "brew"
  "Homebrew executable name or absolute path."
  :type 'string
  :group 'homebrew)

(defcustom homebrew-cache-ttl 300
  "Seconds before cached package name lists are refreshed."
  :type 'number
  :group 'homebrew)

(defcustom homebrew-process-buffer-name "*homebrew*"
  "Buffer name for async Homebrew process output."
  :type 'string
  :group 'homebrew)

(defcustom homebrew-info-buffer-name "*homebrew-info*"
  "Buffer name for formatted package info."
  :type 'string
  :group 'homebrew)

(defcustom homebrew-status-buffer-name "*homebrew-status*"
  "Buffer name for the Homebrew status buffer."
  :type 'string
  :group 'homebrew)

(defcustom homebrew-progress t
  "Whether to show a progress bar while a Homebrew process runs."
  :type 'boolean
  :group 'homebrew)

(defcustom homebrew-progress-width 24
  "Width of the Homebrew progress bar, in characters."
  :type 'natnum
  :group 'homebrew)

(defcustom homebrew-progress-characters '(?█ . ?░)
  "Characters (FILLED . EMPTY) used to draw the Homebrew progress bar."
  :type '(cons character character)
  :group 'homebrew)

(defcustom homebrew-progress-download-share 0.4
  "Share of a package's progress attributed to downloading it."
  :type 'float
  :group 'homebrew)

(defcustom homebrew-progress-spinner-frames '("◐" "◓" "◑" "◒")
  "Frames cycled through while a Homebrew process is running."
  :type '(repeat string)
  :group 'homebrew)

;;;; Internals

(defvar homebrew--cache nil
  "Alist of cached package data and a `time' stamp.")

(defvar homebrew--process nil
  "Currently running mutating Homebrew process, or nil.")

(defvar homebrew--type-scope "both"
  "Type filter: \"formula\", \"cask\", or \"both\".")

(defvar homebrew-package-history nil
  "Minibuffer history for Homebrew package names.")

(defvar-local homebrew--info-package nil
  "Package plist shown in an info buffer, or nil.")

(defun homebrew--exe ()
  "Return absolute path to the brew executable, or signal."
  (or (executable-find homebrew-executable)
      (user-error "Homebrew executable not found: %s" homebrew-executable)))

(defun homebrew--call (&rest args)
  "Run brew ARGS synchronously; return stdout string."
  (with-temp-buffer
    (let ((err-file (make-temp-file "homebrew-err")))
      (unwind-protect
          (let* ((status (apply #'call-process (homebrew--exe) nil
                                (list (current-buffer) err-file) nil args))
                 (out (buffer-string))
                 (err-str (with-temp-buffer
                            (insert-file-contents err-file)
                            (buffer-string))))
            (unless (eq status 0)
              (user-error "brew %s failed (%s): %s"
                          (string-join args " ")
                          status
                          (string-trim (if (string-empty-p err-str) out err-str))))
            out)
        (ignore-errors (delete-file err-file))))))

(defun homebrew--call-json (&rest args)
  "Run brew ARGS and parse JSON stdout."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read-from-string (apply #'homebrew--call args))))

(defun homebrew--lines (&rest args)
  "Run brew ARGS and return non-empty stdout lines."
  (let ((out (string-trim-right (apply #'homebrew--call args))))
    (if (string-empty-p out)
        nil
      (split-string out "\n" t))))

(defun homebrew--hash (names)
  "Return a hash table of NAMES for O(1) membership tests."
  (let ((h (make-hash-table :test #'equal :size (max 1 (length names)))))
    (dolist (n names)
      (puthash n t h))
    h))

(defun homebrew--make-cand (name type &optional annotation)
  "Propertize NAME with TYPE and optional ANNOTATION."
  (let ((s (propertize name 'homebrew-type type)))
    (when annotation
      (setq s (propertize s 'homebrew-annotation annotation)))
    s))

(defun homebrew--pkg (name type &rest props)
  "Return a package plist for status/actions."
  (append (list :name name :type type) props))

(defun homebrew--pkg-name (pkg) (plist-get pkg :name))
(defun homebrew--pkg-type (pkg) (plist-get pkg :type))

(defun homebrew--cache-fresh-p ()
  "Return non-nil if `homebrew--cache' is still valid."
  (let ((time (alist-get 'time homebrew--cache)))
    (and time (< (- (float-time) time) homebrew-cache-ttl))))

(defun homebrew--invalidate-cache ()
  "Drop cached package lists."
  (setq homebrew--cache nil))

(defun homebrew--ensure-cache (&optional force)
  "Populate `homebrew--cache' unless fresh.  FORCE rebuilds it."
  (when (or force (not (homebrew--cache-fresh-p)))
    (message "Homebrew: refreshing package cache...")
    (redisplay)
    (let* ((formulae (or (homebrew--lines "formulae")
                         (user-error "brew formulae returned no packages")))
           (casks (or (homebrew--lines "casks")
                      (user-error "brew casks returned no packages")))
           (installed (homebrew--call-json "info" "--json=v2" "--installed"))
           (outdated (homebrew--call-json "outdated" "--json=v2"))
           (leaves (or (homebrew--lines "leaves") nil))
           (f-hash (homebrew--hash formulae))
           (c-hash (homebrew--hash casks))
           (out-f-info
            (mapcar (lambda (f)
                      (homebrew--pkg (alist-get 'name f) 'formula
                                     :old (car (alist-get 'installed_versions f))
                                     :new (alist-get 'current_version f)
                                     :pinned (alist-get 'pinned f)))
                    (alist-get 'formulae outdated)))
           (out-c-info
            (mapcar (lambda (c)
                      (homebrew--pkg (alist-get 'name c) 'cask
                                     :old (car (alist-get 'installed_versions c))
                                     :new (alist-get 'current_version c)
                                     :pinned (alist-get 'pinned c)))
                    (alist-get 'casks outdated)))
           (inst-f-info
            (mapcar
             (lambda (f)
               (let* ((inst (car (alist-get 'installed f)))
                      (name (alist-get 'name f)))
                 (homebrew--pkg name 'formula
                                :version (alist-get 'version inst)
                                :on-request (alist-get 'installed_on_request inst)
                                :desc (alist-get 'desc f)
                                :outdated (alist-get 'outdated f))))
             (alist-get 'formulae installed)))
           (inst-c-info
            (mapcar
             (lambda (c)
               (let* ((token (alist-get 'token c))
                      (installed (alist-get 'installed c))
                      (ver (or (alist-get 'version c)
                               (if (listp installed) (car installed) installed))))
                 (homebrew--pkg token 'cask
                                :version ver
                                :desc (alist-get 'desc c)
                                :outdated (alist-get 'outdated c))))
             (alist-get 'casks installed)))
           (inst-f (mapcar #'homebrew--pkg-name inst-f-info))
           (inst-c (mapcar #'homebrew--pkg-name inst-c-info))
           (out-f (mapcar #'homebrew--pkg-name out-f-info))
           (out-c (mapcar #'homebrew--pkg-name out-c-info))
           (inst-f-hash (homebrew--hash inst-f))
           (out-f-hash (homebrew--hash out-f))
           (out-ann
            (let ((h (make-hash-table :test #'equal)))
              (dolist (p (append out-f-info out-c-info))
                (puthash (homebrew--pkg-name p)
                         (format "%s -> %s"
                                 (or (plist-get p :old) "?")
                                 (or (plist-get p :new) "?"))
                         h))
              h))
           (inst-ann
            (let ((h (make-hash-table :test #'equal)))
              (dolist (p (append inst-f-info inst-c-info))
                (puthash (homebrew--pkg-name p)
                         (or (plist-get p :version) "")
                         h))
              h))
           (f-cands (mapcar (lambda (n) (homebrew--make-cand n 'formula)) formulae))
           (c-cands (mapcar (lambda (n) (homebrew--make-cand n 'cask)) casks))
           (both-cands
            (let ((acc (copy-sequence f-cands)))
              (dolist (n casks)
                (if (gethash n f-hash)
                    (push (homebrew--make-cand (concat n " (cask)") 'cask) acc)
                  (push (homebrew--make-cand n 'cask) acc)))
              (nreverse acc)))
           (mk-inst
            (lambda (p)
              (homebrew--make-cand
               (homebrew--pkg-name p)
               (homebrew--pkg-type p)
               (let ((ver (plist-get p :version))
                     (out (gethash (homebrew--pkg-name p) out-ann)))
                 (string-trim
                  (concat (or ver "")
                          (if out (concat "  " out) "")))))))
           (inst-cands
            (append (mapcar mk-inst inst-f-info)
                    (mapcar (lambda (p)
                              (let ((n (homebrew--pkg-name p)))
                                (if (gethash n inst-f-hash)
                                    (homebrew--make-cand
                                     (concat n " (cask)") 'cask
                                     (gethash n inst-ann))
                                  (funcall mk-inst p))))
                            inst-c-info)))
           (mk-out
            (lambda (p)
              (homebrew--make-cand
               (homebrew--pkg-name p)
               (homebrew--pkg-type p)
               (format "%s -> %s"
                       (or (plist-get p :old) "?")
                       (or (plist-get p :new) "?")))))
           (out-cands
            (append (mapcar mk-out out-f-info)
                    (mapcar (lambda (p)
                              (let ((n (homebrew--pkg-name p)))
                                (if (gethash n out-f-hash)
                                    (homebrew--make-cand
                                     (concat n " (cask)") 'cask
                                     (gethash n out-ann))
                                  (funcall mk-out p))))
                            out-c-info))))
      (setq homebrew--cache
            `((formulae . ,formulae)
              (casks . ,casks)
              (formula-hash . ,f-hash)
              (cask-hash . ,c-hash)
              (leaves . ,leaves)
              (installed-formulae . ,inst-f)
              (installed-casks . ,inst-c)
              (outdated-formulae . ,out-f)
              (outdated-casks . ,out-c)
              (outdated-formula-info . ,out-f-info)
              (outdated-cask-info . ,out-c-info)
              (installed-formula-info . ,inst-f-info)
              (installed-cask-info . ,inst-c-info)
              (cands-formula . ,f-cands)
              (cands-cask . ,c-cands)
              (cands-both . ,both-cands)
              (cands-installed . ,inst-cands)
              (cands-installed-formula . ,(mapcar mk-inst inst-f-info))
              (cands-installed-cask . ,(mapcar mk-inst inst-c-info))
              (cands-outdated . ,out-cands)
              (cands-outdated-formula . ,(mapcar mk-out out-f-info))
              (cands-outdated-cask . ,(mapcar mk-out out-c-info))
              (time . ,(float-time))))
      (message "Homebrew: cache ready (%d formulae, %d casks)"
               (length formulae) (length casks)))))

(defun homebrew--cached (key)
  "Return cached value for KEY, ensuring cache is warm."
  (homebrew--ensure-cache)
  (alist-get key homebrew--cache))

(defun homebrew--type-arg ()
  "Return current type scope: \"formula\", \"cask\", or \"both\"."
  (let ((scope (or homebrew--type-scope "both")))
    (if (string= scope "auto") "both" scope)))

(defun homebrew--include-formulae-p ()
  "Return non-nil if formulae are in the current type scope."
  (not (string= (homebrew--type-arg) "cask")))

(defun homebrew--include-casks-p ()
  "Return non-nil if casks are in the current type scope."
  (not (string= (homebrew--type-arg) "formula")))

(defun homebrew--candidates (kind)
  "Return completion candidates for KIND (`all', `installed', `outdated')."
  (let ((type (homebrew--type-arg)))
    (pcase kind
      ('all
       (pcase type
         ("cask" (homebrew--cached 'cands-cask))
         ("both" (homebrew--cached 'cands-both))
         (_ (homebrew--cached 'cands-formula))))
      ('installed
       (pcase type
         ("cask" (homebrew--cached 'cands-installed-cask))
         ("both" (homebrew--cached 'cands-installed))
         (_ (homebrew--cached 'cands-installed-formula))))
      ('outdated
       (pcase type
         ("cask" (homebrew--cached 'cands-outdated-cask))
         ("both" (homebrew--cached 'cands-outdated))
         (_ (homebrew--cached 'cands-outdated-formula)))))))

(defun homebrew--annotate (cand)
  "Annotation string for completion candidate CAND."
  (let ((type (get-text-property 0 'homebrew-type cand))
        (ann (get-text-property 0 'homebrew-annotation cand)))
    (string-trim
     (concat (if type (format "  %s" type) "")
             (if (and ann (not (string-empty-p ann)))
                 (format "  %s" ann)
               "")))))

(defun homebrew--parse-cand (pick cands)
  "Return package plist from PICK using CANDS."
  (let* ((matched (cl-find pick cands :test #'string=))
         (type (or (and matched (get-text-property 0 'homebrew-type matched))
                   (pcase (homebrew--type-arg)
                     ("formula" 'formula)
                     ("cask" 'cask)
                     (_ nil))))
         (name pick))
    (when (string-suffix-p " (cask)" name)
      (setq name (substring name 0 -7)
            type (or type 'cask)))
    (unless type
      (setq type
            (cond
             ((gethash name (homebrew--cached 'formula-hash)) 'formula)
             ((gethash name (homebrew--cached 'cask-hash)) 'cask)
             (t 'formula))))
    (homebrew--pkg name type)))

(defun homebrew--completion-table (cands)
  "Return a completion table for CANDS with Homebrew metadata."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . homebrew-package)
          (annotation-function . homebrew--annotate))
      (complete-with-action action cands string pred))))

(defun homebrew--read (prompt kind)
  "Read a package with PROMPT from KIND; return package plist."
  (let* ((cands (homebrew--candidates kind))
         (n (length cands))
         (prompt (format "%s (%d): " prompt n))
         (completion-category-defaults nil)
         (completion-category-overrides
          '((homebrew-package
             (styles basic partial-completion flex substring)))))
    (when (null cands)
      (user-error "No matching Homebrew packages"))
    (let* ((pick (completing-read prompt
                                  (homebrew--completion-table cands)
                                  nil t nil
                                  'homebrew-package-history))
           (bare (substring-no-properties pick)))
      (homebrew--parse-cand bare cands))))

(defun homebrew--pkg-cons (pkg)
  "Return (NAME . TYPE) from package plist PKG."
  (cons (homebrew--pkg-name pkg) (homebrew--pkg-type pkg)))

(defun homebrew--busy-p ()
  "Return non-nil if a mutating brew process is running."
  (and homebrew--process
       (process-live-p homebrew--process)))

(defun homebrew--current-flags ()
  "Return active transient flags relevant to brew mutations."
  (let* ((args (append (and (bound-and-true-p transient-current-command)
                            (ignore-errors
                              (transient-args transient-current-command)))
                       (ignore-errors (transient-args 'homebrew-dispatch))
                       nil))
         (allowed '("--force" "--dry-run" "--verbose")))
    (cl-remove-duplicates
     (cl-remove-if-not (lambda (a) (member a allowed)) args)
     :test #'equal)))

(defun homebrew--type-flag (type)
  "Return brew CLI args for TYPE (`formula' or `cask')."
  (pcase type
    ('cask '("--cask"))
    ('formula '("--formula"))
    (_ nil)))

;;;; Progress


(defface homebrew-progress-bar
  '((t :inherit success))
  "Face for the filled portion of the Homebrew progress bar."
  :group 'homebrew)

(defface homebrew-progress-bar-empty
  '((t :inherit shadow))
  "Face for the unfilled portion of the Homebrew progress bar."
  :group 'homebrew)

(defface homebrew-progress-phase
  '((t :inherit font-lock-comment-face))
  "Face for the phase description beside the Homebrew progress bar."
  :group 'homebrew)

(defface homebrew-progress-spinner
  '((t :inherit font-lock-keyword-face))
  "Face for the spinner shown while a Homebrew process runs."
  :group 'homebrew)

(defconst homebrew--progress-phase-weights
  '((install . 0.15) (pour . 0.5) (link . 0.85))
  "Fraction of one package's installation completed at each phase.")

(defconst homebrew--progress-interval 0.1
  "Seconds between spinner frames.")

(defvar homebrew--progress-timer nil
  "Timer redrawing the progress bar of the running Homebrew process.")

(defvar-local homebrew--progress nil
  "Progress state of a Homebrew process buffer, or nil.")

(defun homebrew--progress-get (key)
  "Return KEY from the progress state of the current buffer."
  (plist-get homebrew--progress key))

(defun homebrew--progress-put (key value)
  "Set KEY to VALUE in the progress state of the current buffer."
  (setq homebrew--progress (plist-put homebrew--progress key value)))

(defun homebrew--progress-count-names (sentence)
  "Return how many package names SENTENCE lists."
  (length
   (split-string
    sentence
    "\\(?:,[[:space:]]*\\(?:and[[:space:]]+\\)?\\|[[:space:]]+and[[:space:]]+\\)"
    t "[[:space:]]*")))

(defun homebrew--progress-grow-total (n)
  "Raise the expected package count to N when N is larger."
  (when (> n (homebrew--progress-get :total))
    (homebrew--progress-put :total n)))

(defun homebrew--progress-phase (phase &optional name)
  "Record PHASE and, when NAME is non-nil, the package it applies to."
  ;; Moving on from a finished package without being told the next one's name is better reported as.
  (when (and (null name) (eq (homebrew--progress-get :phase) 'complete))
    (homebrew--progress-put :name nil))
  (homebrew--progress-put :phase phase)
  (when name
    (homebrew--progress-put :name name))
  (unless (eq phase 'download)
    (homebrew--progress-put :percent nil)))

(defun homebrew--progress-scan-line (line)
  "Update the progress state from output LINE."
  (save-match-data
    (cond
     ((or (string-prefix-p "🍺" line)
          (string-match-p "was successfully installed!" line)
          (string-match-p "/Cellar/[^ ]+: [0-9]+ files?," line))
      (let ((done (1+ (homebrew--progress-get :done))))
        (homebrew--progress-put :done done)
        (homebrew--progress-grow-total done)
        (homebrew--progress-phase 'complete)))
     ;; Announcements of how much work there is in total.
     ((string-match "\\`==> Fetching downloads for:[[:space:]]*\\(.+\\)\\'" line)
      (homebrew--progress-grow-total
       (homebrew--progress-count-names (match-string 1 line))))
     ((string-match
       "\\`==> Installing dependencies\\(?: for [^:]*\\)?:[[:space:]]*\\(.+\\)\\'"
       line)
      (homebrew--progress-grow-total
       (1+ (homebrew--progress-count-names (match-string 1 line)))))
     ((string-match "\\`==> Upgrading \\([0-9]+\\) outdated" line)
      (homebrew--progress-grow-total (string-to-number (match-string 1 line))))
     ;; A download finished.
     ((string-match
       "\\`[✔✓][^ ]* +\\(Bottle Manifest\\|Bottle\\|Cask\\|Formula\\|Resource\\|Patch\\) +\\([^ ]+\\)"
       line)
      (when (member (match-string 1 line) '("Bottle" "Cask" "Formula"))
        (let ((downloaded (1+ (homebrew--progress-get :downloaded))))
          (homebrew--progress-put :downloaded downloaded)
          (homebrew--progress-grow-total downloaded)
          (homebrew--progress-put :percent nil))))
     ;; Per-package phases.
     ((string-match "\\`==> Fetching \\([^ ]+\\)" line)
      (homebrew--progress-phase 'fetch (match-string 1 line)))
     ((string-prefix-p "==> Downloading" line)
      (homebrew--progress-phase 'download))
     ((string-match "\\`==> Installing [^ ]+ dependency: \\([^ ]+\\)" line)
      (homebrew--progress-phase 'install (match-string 1 line)))
     ((string-match
       "\\`==> \\(?:Installing Cask\\|Installing\\|Upgrading\\) \\([^ ]+\\)" line)
      (homebrew--progress-phase 'install (match-string 1 line)))
     ((string-prefix-p "==> Pouring" line)
      (homebrew--progress-phase 'pour))
     ((string-match-p
       "\\`==> \\(?:Running\\|Linking\\|Moving\\|Symlinking\\|Caveats\\|Summary\\)"
       line)
      (homebrew--progress-phase 'link))
     ;; A real percentage, on the rare runs where curl is not silenced.
     ((string-match "\\([0-9]+\\(?:\\.[0-9]+\\)?\\)%" line)
      (homebrew--progress-put
       :percent (/ (string-to-number (match-string 1 line)) 100.0))))))

(defun homebrew--progress-recompute ()
  "Recompute and store the overall progress fraction."
  (let* ((total (max 1 (homebrew--progress-get :total)))
         (done (min (homebrew--progress-get :done) total))
         (phase (homebrew--progress-get :phase))
         (percent (homebrew--progress-get :percent))
         (weight (or (alist-get phase homebrew--progress-phase-weights) 0.0))
         ;; Reaching an install phase proves the package downloaded.
         (downloaded (min total
                          (max (homebrew--progress-get :downloaded)
                               (if (memq phase '(install pour link))
                                   (1+ done)
                                 done))))
         (downloaded (if (and (< downloaded total) (memq phase '(fetch download)))
                         (+ downloaded (or percent 0.5))
                       (float downloaded)))
         (share (min 1.0 (max 0.0 homebrew-progress-download-share)))
         (fetch (* share (min (float total) downloaded)))
         (install (* (- 1.0 share)
                     (+ done (if (< done total) weight 0.0))))
         (fraction (min 1.0 (max 0.0 (/ (+ fetch install) (float total))))))
    ;; A dependency discovered mid-run raises :total; never let that drag the bar backwards.
    (homebrew--progress-put
     :fraction (max fraction (homebrew--progress-get :fraction)))))

(defun homebrew--progress-bar (fraction)
  "Return a bar of `homebrew-progress-width' filled to FRACTION."
  (let* ((width (max 4 homebrew-progress-width))
         (filled (min width (round (* fraction width)))))
    (concat
     (propertize (make-string filled (car homebrew-progress-characters))
                 'face 'homebrew-progress-bar)
     (propertize (make-string (- width filled) (cdr homebrew-progress-characters))
                 'face 'homebrew-progress-bar-empty))))

(defun homebrew--progress-indicator ()
  "Return the spinner, or the outcome mark once brew has exited."
  (let ((status (homebrew--progress-get :status))
        (frames homebrew-progress-spinner-frames))
    (cond
     ((eq status 0) (propertize "✓" 'face 'success))
     (status (propertize "✗" 'face 'error))
     ((null frames) "")
     (t (propertize (nth (mod (homebrew--progress-get :spinner) (length frames))
                         frames)
                    'face 'homebrew-progress-spinner)))))

(defun homebrew--progress-detail ()
  "Return what brew is doing now, or nil when there is nothing to say."
  (let ((status (homebrew--progress-get :status))
        (phase (homebrew--progress-get :phase))
        (name (homebrew--progress-get :name)))
    (cond
     ((eq status 0) nil)
     (status (format "exit %s" status))
     (t (when-let* ((verb (pcase phase
                            ('fetch "fetching")
                            ('download "downloading")
                            ('install "installing")
                            ('pour "pouring")
                            ('link "linking")
                            ('complete "installed")
                            (_ nil))))
          (concat verb (if name (concat " " name) "")))))))

(defun homebrew--progress-time (seconds)
  "Format SECONDS as a short elapsed time."
  (format-seconds (if (>= seconds 3600) "%h:%02m:%02s" "%m:%02s")
                  (floor seconds)))

(defun homebrew--progress-fit (variants width)
  "Return the first of VARIANTS that fits WIDTH columns."
  (let (line)
    (catch 'fit
      (dolist (variant variants line)
        (setq line (string-join (delq nil variant) "  "))
        (when (<= (string-width line) width)
          (throw 'fit line))))))

(defun homebrew--progress-header ()
  "Return the header line for the tracked Homebrew process, or nil."
  (when homebrew--progress
    (let* ((fraction (homebrew--progress-get :fraction))
           (total (homebrew--progress-get :total))
           (done (min (homebrew--progress-get :done) total))
           (elapsed (- (or (homebrew--progress-get :end) (float-time))
                       (homebrew--progress-get :start)))
           (mark (homebrew--progress-indicator))
           (label (homebrew--progress-get :label))
           (bar (homebrew--progress-bar fraction))
           ;; Padded: this header redraws several times a second.
           (percent (format "%3d%%" (round (* 100 fraction))))
           (count (and (> total 1) (format "%d/%d" done total)))
           (detail (when-let* ((text (homebrew--progress-detail)))
                     (propertize text 'face 'homebrew-progress-phase)))
           (time (propertize (homebrew--progress-time elapsed)
                             'face 'homebrew-progress-phase)))
      ;; Shed the bar and its neighbours rather than let the header wrap.
      (concat " "
              (homebrew--progress-fit
               (list (list mark label bar percent count detail time)
                     (list mark label bar percent count time)
                     (list mark label bar percent time)
                     (list mark bar percent time)
                     (list mark percent time)
                     (list mark time))
               (max 0 (1- (window-body-width))))))))

(defun homebrew--progress-cancel-timer ()
  "Cancel the progress refresh timer, if any."
  (when (timerp homebrew--progress-timer)
    (cancel-timer homebrew--progress-timer))
  (setq homebrew--progress-timer nil))

(defun homebrew--progress-tick (buffer)
  "Advance the spinner and redraw the progress header in BUFFER."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (when homebrew--progress
          (homebrew--progress-put :spinner (1+ (homebrew--progress-get :spinner))))
        (force-mode-line-update))
    (homebrew--progress-cancel-timer)))

(defun homebrew--progress-label (args)
  "Return a compact progress label for brew ARGS."
  (let ((names (cl-remove-if (lambda (a) (string-prefix-p "-" a)) args)))
    (string-join (if (> (length names) 4)
                     (append (seq-take names 4) '("..."))
                   names)
                 " ")))

(defun homebrew--progress-start (buffer args)
  "Start tracking progress of brew ARGS in BUFFER."
  (homebrew--progress-cancel-timer)
  (with-current-buffer buffer
    (setq homebrew--progress
          (list :label (homebrew--progress-label args)
                :phase nil :name nil :total 1 :downloaded 0 :done 0
                :percent nil :fraction 0.0 :spinner 0 :pending ""
                :start (float-time) :end nil :status nil))
    (setq-local header-line-format '(:eval (homebrew--progress-header)))
    (force-mode-line-update))
  (setq homebrew--progress-timer
        (run-at-time homebrew--progress-interval homebrew--progress-interval
                     #'homebrew--progress-tick buffer)))

(defun homebrew--progress-stop (buffer)
  "Disable progress tracking in BUFFER."
  (homebrew--progress-cancel-timer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq homebrew--progress nil)
      (setq-local header-line-format nil)
      (force-mode-line-update))))

(defun homebrew--progress-finish (buffer status)
  "Freeze the progress bar in BUFFER after the process exited with STATUS."
  (homebrew--progress-cancel-timer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when homebrew--progress
        (homebrew--progress-put :status status)
        (homebrew--progress-put :end (float-time))
        (homebrew--progress-phase nil)
        (when (eq status 0)
          (homebrew--progress-put :fraction 1.0))
        (force-mode-line-update)))))

(defun homebrew--progress-update (string)
  "Feed process output STRING to the progress tracker."
  (when homebrew--progress
    (let* ((text (concat (homebrew--progress-get :pending) string))
           (lines (split-string text "[\r\n]")))
      (homebrew--progress-put :pending (car (last lines)))
      (dolist (line (butlast lines))
        (let ((line (string-trim line)))
          (unless (string-empty-p line)
            (homebrew--progress-scan-line line))))
      (homebrew--progress-recompute)
      (force-mode-line-update))))

;;;; Process / info buffers

(defface homebrew-heading '((t :inherit outline-1 :weight bold))
  "Face for section headings in Homebrew buffers."
  :group 'homebrew)

(defface homebrew-field '((t :inherit font-lock-builtin-face))
  "Face for field labels in Homebrew buffers."
  :group 'homebrew)

(defface homebrew-outdated '((t :inherit warning))
  "Face for outdated version markers."
  :group 'homebrew)

(defface homebrew-version '((t :inherit shadow))
  "Face for version strings."
  :group 'homebrew)

(define-derived-mode homebrew-mode special-mode "Homebrew"
  "Major mode for Homebrew process and info buffers."
  :interactive nil
  (goto-address-mode 1))

(keymap-set homebrew-mode-map "TAB" #'forward-button)
(keymap-set homebrew-mode-map "<backtab>" #'backward-button)

;;;; Buttons

(defun homebrew--button (label &rest properties)
  "Insert a button labeled LABEL with PROPERTIES at point."
  (let ((text (if (display-graphic-p) label (concat "[" label "]")))
        (face (if (display-graphic-p)
                  (progn (require 'cus-edit) 'custom-button)
                'link)))
    (apply #'insert-text-button text 'face face 'follow-link t properties)))

(defun homebrew--button-action (button)
  "Run BUTTON's Homebrew command on BUTTON's package."
  (funcall (button-get button 'homebrew-command)
           (button-get button 'homebrew-pkg)))

(defun homebrew--fontify-region (start end)
  "Make URLs/addresses clickable between START and END."
  (when (and (bound-and-true-p goto-address-mode)
             (> end start))
    (goto-address-fontify start end)))

(defun homebrew--process-buffer ()
  "Return the Homebrew process buffer, creating it if needed."
  (let ((buf (get-buffer-create homebrew-process-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'homebrew-mode)
        (homebrew-mode))
      (setq buffer-read-only t))
    buf))

(defun homebrew--insert-output (string)
  "Insert STRING at the end of the current buffer, honoring carriage returns."
  (goto-char (point-max))
  (let ((chunks (split-string (string-replace "\r\n" "\n" string) "\r")))
    (insert (car chunks))
    (dolist (chunk (cdr chunks))
      (delete-region (line-beginning-position) (point))
      (insert chunk))))

(defun homebrew--refresh-status-buffers ()
  "Refresh any live Homebrew status buffer."
  (when-let* ((buf (get-buffer homebrew-status-buffer-name)))
    (with-current-buffer buf
      (when (derived-mode-p 'homebrew-status-mode)
        (homebrew-status-refresh t)))))

(defun homebrew--refresh-info-buffer ()
  "Redraw the info buffer, so its action buttons match the new state."
  (when-let* ((buf (get-buffer homebrew-info-buffer-name))
              (pkg (buffer-local-value 'homebrew--info-package buf)))
    (with-demoted-errors "Homebrew: %S"
      (homebrew--show-info (homebrew--pkg-name pkg)
                           (homebrew--pkg-type pkg)
                           t))))

(defun homebrew--run-async (args &optional on-success)
  "Run brew ARGS asynchronously in the process buffer."
  (when (homebrew--busy-p)
    (user-error "Homebrew is already running; see %s"
                homebrew-process-buffer-name))
  (let* ((buf (homebrew--process-buffer))
         (command (cons (homebrew--exe) args)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (start (point-min)))
        (erase-buffer)
        (insert (format-time-string "[%F %T] ")
                "$ " (mapconcat #'shell-quote-argument command " ")
                "\n\n")
        (homebrew--fontify-region start (point-max))))
    (if homebrew-progress
        (homebrew--progress-start buf args)
      (homebrew--progress-stop buf))
    (display-buffer buf)
    (setq homebrew--process
          (make-process
           :name "homebrew"
           :buffer buf
           :command command
           :connection-type 'pipe
           :filter
           (lambda (proc string)
             (when (buffer-live-p (process-buffer proc))
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t)
                       (at-end (eq (point) (point-max)))
                       (start (point-max)))
                   (save-excursion
                     (homebrew--insert-output string)
                     (homebrew--fontify-region (min start (point-max))
                                               (point-max)))
                   (when at-end
                     (goto-char (point-max))))
                 (homebrew--progress-update string))))
           :sentinel
           (lambda (proc _event)
             (let ((status (process-exit-status proc))
                   (buf (process-buffer proc))
                   (exited (eq (process-status proc) 'exit)))
               (when (memq (process-status proc) '(exit signal))
                 (setq homebrew--process nil)
                 (homebrew--progress-finish buf status)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let ((inhibit-read-only t)
                           (start (point-max)))
                       (goto-char (point-max))
                       (insert (format "\n[exit %s] %s\n"
                                       status
                                       (format-time-string "%F %T")))
                       (homebrew--fontify-region start (point-max)))))
                 (if (and exited (eq status 0))
                     (progn
                       (homebrew--invalidate-cache)
                       (homebrew--refresh-status-buffers)
                       (homebrew--refresh-info-buffer)
                       (when on-success (funcall on-success))
                       (message "brew %s: done" (string-join args " ")))
                   (message "brew %s: failed (exit %s)"
                            (string-join args " ")
                            status)))))))))

(defun homebrew--insert-actions (name type installed)
  "Insert the action buttons for package NAME of TYPE."
  (let ((pkg (homebrew--pkg name type))
        (first t))
    (insert "Actions:     ")
    (dolist (spec (if installed
                      (append '(("Uninstall" homebrew-uninstall))
                              (when (eq type 'cask)
                                '(("Uninstall (zap)" homebrew-uninstall-zap))))
                    '(("Install" homebrew-install))))
      (if first (setq first nil) (insert "  "))
      (homebrew--button (car spec)
                        'action #'homebrew--button-action
                        'homebrew-command (cadr spec)
                        'homebrew-pkg pkg
                        'help-echo (format "%s %s" (car spec) name)))
    (insert "\n")))

(defun homebrew--format-formula (f)
  "Insert a human-readable description of formula alist F."
  (let* ((name (alist-get 'name f))
         (desc (alist-get 'desc f))
         (home (alist-get 'homepage f))
         (versions (alist-get 'versions f))
         (stable (alist-get 'stable versions))
         (installed (alist-get 'installed f))
         (outdated (alist-get 'outdated f))
         (deps (alist-get 'dependencies f))
         (caveats (alist-get 'caveats f))
         (inst-ver (and installed
                        (alist-get 'version (car installed)))))
    (insert (format "Name:        %s  (formula)\n" name))
    (when desc (insert (format "Description: %s\n" desc)))
    (when home (insert (format "Homepage:    %s\n" home)))
    (when stable (insert (format "Version:     %s\n" stable)))
    (insert (format "Installed:   %s%s\n"
                    (if installed (or inst-ver "yes") "no")
                    (if outdated "  [outdated]" "")))
    (when deps
      (insert "Depends on:  " (string-join deps ", ") "\n"))
    (homebrew--insert-actions name 'formula installed)
    (when (and caveats (not (string-empty-p caveats)))
      (insert "\nCaveats:\n" caveats
              (if (string-suffix-p "\n" caveats) "" "\n")))))

(defun homebrew--format-cask (c)
  "Insert a human-readable description of cask alist C."
  (let* ((token (alist-get 'token c))
         (name (alist-get 'name c))
         (desc (alist-get 'desc c))
         (home (alist-get 'homepage c))
         (version (alist-get 'version c))
         (installed (alist-get 'installed c))
         (outdated (alist-get 'outdated c))
         (caveats (alist-get 'caveats c))
         (display-name (if (listp name) (car name) name)))
    (insert (format "Name:        %s  (cask)\n" token))
    (when display-name (insert (format "App:         %s\n" display-name)))
    (when desc (insert (format "Description: %s\n" desc)))
    (when home (insert (format "Homepage:    %s\n" home)))
    (when version (insert (format "Version:     %s\n" version)))
    (insert (format "Installed:   %s%s\n"
                    (if installed
                        (if (listp installed)
                            (string-join (mapcar #'format installed) ", ")
                          (format "%s" installed))
                      "no")
                    (if outdated "  [outdated]" "")))
    (homebrew--insert-actions token 'cask installed)
    (when (and caveats (not (string-empty-p caveats)))
      (insert "\nCaveats:\n" caveats
              (if (string-suffix-p "\n" caveats) "" "\n")))))

(defun homebrew--show-info (name type &optional no-display)
  "Fetch and display info for package NAME of TYPE."
  (let* ((args (append '("info" "--json=v2")
                       (homebrew--type-flag type)
                       (list name)))
         (data (apply #'homebrew--call-json args))
         (formulae (alist-get 'formulae data))
         (casks (alist-get 'casks data))
         (buf (get-buffer-create homebrew-info-buffer-name)))
    (with-current-buffer buf
      (let ((point (and no-display (point))))
        (homebrew-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (cond
           (formulae (homebrew--format-formula (car formulae)))
           (casks (homebrew--format-cask (car casks)))
           (t (insert (format "No info for %s\n" name))))
          (homebrew--fontify-region (point-min) (point-max))
          (goto-char (min (or point (point-min)) (point-max)))))
      (setq-local homebrew--info-package (homebrew--pkg name type))
      (setq-local truncate-lines nil))
    (unless no-display
      (display-buffer buf))
    buf))

(defun homebrew--resolve-search-hit (pick)
  "Resolve search hit PICK to package plist."
  (let ((type (homebrew--type-arg)))
    (homebrew--pkg
     pick
     (cond
      ((string= type "formula") 'formula)
      ((string= type "cask") 'cask)
      ((gethash pick (homebrew--cached 'formula-hash)) 'formula)
      ((gethash pick (homebrew--cached 'cask-hash)) 'cask)
      (t 'formula)))))

;;;; Package at point

(defun homebrew--package-at-point (&optional no-info-buffer)
  "Return the package plist at point, or nil."
  (or (get-text-property (point) 'homebrew-package)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'homebrew-package))
      (and (not no-info-buffer) homebrew--info-package)))

(defun homebrew--require-package (&optional kind prompt)
  "Return package at point or read one for KIND with PROMPT."
  (or (homebrew--package-at-point)
      (homebrew--read (or prompt "Package") (or kind 'all))))

;;;; Commands

;;;###autoload
(defun homebrew-search (query)
  "Search Homebrew for QUERY and show info for the chosen hit."
  (interactive "sHomebrew search: ")
  (when (string-empty-p (string-trim query))
    (user-error "Empty search query"))
  (let* ((type (homebrew--type-arg))
         (args (pcase type
                 ("formula" (list "search" "--formula" query))
                 ("cask" (list "search" "--cask" query))
                 (_ (list "search" query))))
         (lines (apply #'homebrew--lines args))
         (hits (cl-remove-if
                (lambda (l)
                  (or (string-prefix-p "==>" l)
                      (string-prefix-p " " l)
                      (string-empty-p l)))
                lines)))
    (unless hits
      (user-error "No results for %s" query))
    (let* ((pick (completing-read "Package: " hits nil t nil
                                  'homebrew-package-history))
           (pkg (homebrew--resolve-search-hit pick)))
      (homebrew--show-info (homebrew--pkg-name pkg)
                           (homebrew--pkg-type pkg)))))

;;;###autoload
(defun homebrew-info (&optional package)
  "Show info for PACKAGE or package at point / chosen package."
  (interactive)
  (let ((pkg (or package
                 (homebrew--package-at-point t)
                 (homebrew--read "Info" 'all))))
    (homebrew--show-info (homebrew--pkg-name pkg)
                         (homebrew--pkg-type pkg))))

;;;###autoload
(defun homebrew-install (&optional package)
  "Install PACKAGE or a chosen package."
  (interactive)
  (let* ((pkg (or package (homebrew--read "Install" 'all)))
         (flags (homebrew--current-flags)))
    (homebrew--run-async
     (append '("install")
             flags
             (homebrew--type-flag (homebrew--pkg-type pkg))
             (list (homebrew--pkg-name pkg))))))

(defun homebrew--uninstall (&optional zap package)
  "Uninstall PACKAGE; with ZAP pass `--zap'."
  (let* ((pkg (or package
                  (homebrew--require-package
                   'installed
                   (if zap "Uninstall --zap" "Uninstall"))))
         (name (homebrew--pkg-name pkg))
         (type (homebrew--pkg-type pkg))
         (flags (homebrew--current-flags)))
    (when (yes-or-no-p
           (format "%s %s (%s)? "
                   (if zap "Uninstall --zap" "Uninstall")
                   name type))
      (homebrew--run-async
       (append '("uninstall")
               (when zap '("--zap"))
               flags
               (homebrew--type-flag type)
               (list name))))))

;;;###autoload
(defun homebrew-uninstall (&optional package)
  "Uninstall PACKAGE or package at point / chosen package."
  (interactive)
  (homebrew--uninstall nil package))

;;;###autoload
(defun homebrew-uninstall-zap (&optional package)
  "Uninstall PACKAGE with `--zap'."
  (interactive)
  (homebrew--uninstall t package))

;;;###autoload
(defun homebrew-upgrade (&optional package)
  "Upgrade PACKAGE or an outdated package at point / chosen."
  (interactive)
  (unless (homebrew--candidates 'outdated)
    (user-error "Nothing outdated"))
  (let* ((pkg (or package
                  (let ((at (homebrew--package-at-point)))
                    (if (and at (member (homebrew--pkg-name at)
                                        (mapcar #'homebrew--pkg-name
                                                (append
                                                 (homebrew--cached 'outdated-formula-info)
                                                 (homebrew--cached 'outdated-cask-info)))))
                        at
                      (homebrew--read "Upgrade" 'outdated)))))
         (flags (homebrew--current-flags)))
    (homebrew--run-async
     (append '("upgrade")
             flags
             (homebrew--type-flag (homebrew--pkg-type pkg))
             (list (homebrew--pkg-name pkg))))))

;;;###autoload
(defun homebrew-upgrade-all ()
  "Upgrade all outdated Homebrew packages in the current type scope."
  (interactive)
  (unless (homebrew--candidates 'outdated)
    (user-error "Nothing outdated"))
  (let* ((type (homebrew--type-arg))
         (flags (homebrew--current-flags))
         (args (append
                '("upgrade")
                flags
                (pcase type
                  ("formula" '("--formula"))
                  ("cask" '("--cask"))
                  (_ nil))))
         (label (pcase type
                  ("formula" "all outdated formulae")
                  ("cask" "all outdated casks")
                  (_ "all outdated packages"))))
    (when (yes-or-no-p (format "Upgrade %s? " label))
      (homebrew--run-async args))))

;;;###autoload
(defun homebrew-update ()
  "Run `brew update' (refresh tap formulae)."
  (interactive)
  (homebrew--run-async
   (append '("update") (homebrew--current-flags))))

;;;###autoload
(defun homebrew-refresh-cache ()
  "Force-refresh the Homebrew package name cache and status buffer."
  (interactive)
  (homebrew--ensure-cache t)
  (homebrew--refresh-status-buffers)
  (message "Homebrew cache refreshed (%d formulae, %d casks)"
           (length (homebrew--cached 'formulae))
           (length (homebrew--cached 'casks))))

;;;###autoload
(defun homebrew-visit-homepage (&optional package)
  "Open the homepage for PACKAGE or package at point."
  (interactive)
  (let* ((pkg (or package (homebrew--require-package 'all "Homepage")))
         (data (apply #'homebrew--call-json
                      (append '("info" "--json=v2")
                              (homebrew--type-flag (homebrew--pkg-type pkg))
                              (list (homebrew--pkg-name pkg)))))
         (home (or (alist-get 'homepage (car (alist-get 'formulae data)))
                   (alist-get 'homepage (car (alist-get 'casks data))))))
    (unless home
      (user-error "No homepage for %s" (homebrew--pkg-name pkg)))
    (browse-url home)))

;;;; Status buffer


(defun homebrew--prefix ()
  "Return the Homebrew prefix, derived from the executable's path."
  (directory-file-name
   (file-name-directory
    (directory-file-name (file-name-directory (homebrew--exe))))))

(defun homebrew--insert-field (label value)
  "Insert a `LABEL: VALUE' line of the status header."
  (insert "  "
          (propertize (format "%-10s" label) 'face 'homebrew-field)
          " " value "\n"))

(defun homebrew--insert-package-line (pkg)
  "Insert a status line for package plist PKG."
  (let* ((name (homebrew--pkg-name pkg))
         (old (plist-get pkg :old))
         (new (plist-get pkg :new))
         (ver (plist-get pkg :version))
         (desc (plist-get pkg :desc))
         (start (point)))
    (insert "  ")
    (insert-text-button name
                        'face 'link
                        'follow-link t
                        'action #'homebrew--button-action
                        'homebrew-command #'homebrew-info
                        'homebrew-pkg pkg
                        'help-echo (format "Show info for %s" name))
    (insert (make-string (max 1 (- 30 (string-width name))) ?\s))
    (let* ((version (cond ((and old new) (cons (format "%s -> %s" old new)
                                               'homebrew-outdated))
                          (ver (cons ver 'homebrew-version))
                          (t (cons "" nil))))
           (pin (and (plist-get pkg :pinned)
                     (propertize "[pinned]" 'face 'homebrew-outdated)))
           (desc (and desc (not (string-empty-p desc))
                      (truncate-string-to-width desc 48 0 nil "...")))
           (tail (and (or pin desc) t))
           (text (if tail
                     (truncate-string-to-width (car version) 24 0 nil "...")
                   (car version))))
      (insert (if (cdr version) (propertize text 'face (cdr version)) text))
      (when tail
        (insert (make-string (max 1 (- 24 (string-width text))) ?\s)))
      (when pin (insert pin (if desc " " "")))
      (when desc (insert desc)))
    (insert "\n")
    (put-text-property start (point) 'homebrew-package pkg)))

(defun homebrew--insert-status-section (heading packages)
  "Insert a section titled HEADING listing PACKAGES."
  (insert (propertize (format "%s (%d)\n" heading (length packages))
                      'face 'homebrew-heading))
  (if (null packages)
      (insert "  (none)\n")
    (dolist (p packages)
      (homebrew--insert-package-line p)))
  (insert "\n"))

(defun homebrew--insert-status-header ()
  "Insert the counts and settings at the top of the status buffer."
  (insert (propertize "Homebrew\n" 'face 'homebrew-heading))
  (homebrew--insert-field "Prefix:" (homebrew--prefix))
  (homebrew--insert-field "Scope:" (homebrew--type-arg))
  (homebrew--insert-field
   "Formulae:"
   (format "%d known, %d installed, %d leaves, %d outdated"
           (length (homebrew--cached 'formulae))
           (length (homebrew--cached 'installed-formulae))
           (length (homebrew--cached 'leaves))
           (length (homebrew--cached 'outdated-formulae))))
  (homebrew--insert-field
   "Casks:"
   (format "%d known, %d installed, %d outdated"
           (length (homebrew--cached 'casks))
           (length (homebrew--cached 'installed-casks))
           (length (homebrew--cached 'outdated-casks))))
  (homebrew--insert-field "Updated:" (format-time-string "%F %T"))
  (insert "\n"))

(defun homebrew-status-refresh (&optional keep-position)
  "Redraw the Homebrew status buffer."
  (interactive)
  (unless (derived-mode-p 'homebrew-status-mode)
    (user-error "Not in a Homebrew status buffer"))
  (let ((line (and keep-position (line-number-at-pos)))
        (column (and keep-position (current-column)))
        (inhibit-read-only t))
    (homebrew--ensure-cache)
    (erase-buffer)
    (homebrew--insert-status-header)
    (when (homebrew--include-formulae-p)
      (homebrew--insert-status-section
       "Outdated formulae" (homebrew--cached 'outdated-formula-info)))
    (when (homebrew--include-casks-p)
      (homebrew--insert-status-section
       "Outdated casks" (homebrew--cached 'outdated-cask-info)))
    (when (homebrew--include-formulae-p)
      (let* ((installed (homebrew--cached 'installed-formula-info))
             (by-name (let ((h (make-hash-table :test #'equal)))
                        (dolist (p installed)
                          (puthash (homebrew--pkg-name p) p h))
                        h)))
        (homebrew--insert-status-section
         "Leaves"
         (delq nil (mapcar (lambda (n) (gethash n by-name))
                           (homebrew--cached 'leaves))))
        (homebrew--insert-status-section
         "Installed formulae (on request)"
         (cl-remove-if-not (lambda (p) (plist-get p :on-request)) installed))
        (homebrew--insert-status-section
         "Installed formulae (dependencies)"
         (cl-remove-if (lambda (p) (plist-get p :on-request)) installed))))
    (when (homebrew--include-casks-p)
      (homebrew--insert-status-section
       "Installed casks" (homebrew--cached 'installed-cask-info)))
    (goto-char (point-min))
    (when line
      (forward-line (1- line))
      (move-to-column (or column 0)))
    (set-buffer-modified-p nil))
  (when (called-interactively-p 'interactive)
    (message "Homebrew status refreshed")))

(defvar-keymap homebrew-status-mode-map
  :doc "Keymap for `homebrew-status-mode'."
  :parent homebrew-mode-map
  "g"   #'homebrew-status-refresh
  "r"   #'homebrew-refresh-cache
  "?"   #'homebrew-dispatch
  "h"   #'homebrew-dispatch
  "RET" #'push-button)

;;;###autoload
(define-derived-mode homebrew-status-mode homebrew-mode "Homebrew-Status"
  "Major mode for the Homebrew status buffer."
  :interactive nil
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (homebrew-status-refresh t))))

;;;###autoload
(defun homebrew-status ()
  "Print the current Homebrew state into a status buffer."
  (interactive)
  (let ((buf (get-buffer-create homebrew-status-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'homebrew-status-mode)
        (homebrew-status-mode))
      (homebrew-status-refresh))
    (pop-to-buffer-same-window buf)))

;;;; Transient

(defun homebrew--read-type (prompt _initial-input _history)
  "Read a type scope with PROMPT."
  (completing-read prompt '("formula" "cask" "both") nil t
                   nil nil (homebrew--type-arg)))

(transient-define-infix homebrew--infix-type ()
  :class 'transient-lisp-variable
  :variable 'homebrew--type-scope
  :reader #'homebrew--read-type
  :prompt "Type: "
  :description
  (lambda ()
    (format "type [%s]" (homebrew--type-arg))))

(transient-define-argument homebrew--arg-force ()
  :description "force"
  :class 'transient-switch
  :key "-f"
  :argument "--force")

(transient-define-argument homebrew--arg-dry-run ()
  :description "dry-run"
  :class 'transient-switch
  :key "-n"
  :argument "--dry-run")

(transient-define-argument homebrew--arg-verbose ()
  :description "verbose"
  :class 'transient-switch
  :key "-v"
  :argument "--verbose")

;;;###autoload
(transient-define-prefix homebrew-uninstall-menu ()
  "Uninstall a Homebrew package."
  ["Uninstall"
   ("u" "uninstall" homebrew-uninstall)
   ("z" "uninstall --zap" homebrew-uninstall-zap)])

;;;###autoload
(transient-define-prefix homebrew-dispatch ()
  "Homebrew commands."
  ["Scope"
   ("t" homebrew--infix-type)]
  ["Arguments"
   (homebrew--arg-force)
   (homebrew--arg-dry-run)
   (homebrew--arg-verbose)]
  ["Lookup"
   ("s" "search" homebrew-search)
   ("I" "info" homebrew-info)
   ("o" "homepage" homebrew-visit-homepage)
   ("b" "status buffer" homebrew-status)]
  ["Packages"
   ("i" "install" homebrew-install)
   ("u" "uninstall" homebrew-uninstall-menu)
   ("U" "upgrade" homebrew-upgrade)
   ("A" "upgrade all" homebrew-upgrade-all)
   ("g" "update (brew update)" homebrew-update)]
  ["Cache"
   ("r" "refresh cache" homebrew-refresh-cache)])

;;;; Evil (optional)

(defun homebrew--setup-evil ()
  "Use Emacs state in Homebrew UI buffers when Evil is present."
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'homebrew-status-mode 'emacs)
    (evil-set-initial-state 'homebrew-mode 'emacs)))

(if (featurep 'evil)
    (homebrew--setup-evil)
  (with-eval-after-load 'evil
    (homebrew--setup-evil)))

(provide 'homebrew)
;;; homebrew.el ends here
