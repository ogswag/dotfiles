;;; py.el --- Which Python a Python file runs in -*- lexical-binding: t; -*-

;;; Commentary:
;; find the project's venv and stick it on path for run/repl/tools.

;;; Code:

(require 'seq)

;;;; Forward declarations

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(declare-function my/task--environment "task" (vars &optional environment))
(declare-function my/task--exec-path "task" (vars path))

(defvar my/task-state-change-hook)
(defvar python-shell-interpreter)
(defvar python-shell-virtualenv-root)

;;;; Options

(defgroup my/python nil
  "Which Python a Python file runs in."
  :group 'my/task
  :prefix "my/python-")

(defcustom my/python-env-search-names '(".venv" "venv" "env" ".direnv/*")
  "Directory names a virtualenv is looked for under, nearest first."
  :type '(repeat string)
  :group 'my/python)

(defcustom my/python-env-generic-names '(".venv" "venv" "env")
  "Names too common to put on a button."
  :type '(repeat string)
  :group 'my/python)

(defcustom my/python-env-directories
  '("~/.virtualenvs"                    ; virtualenvwrapper, pyenv-virtualenv
    "~/.local/share/virtualenvs"        ; pipenv
    "~/Library/Caches/pypoetry/virtualenvs" ; poetry, macOS
    "~/.cache/pypoetry/virtualenvs"     ; poetry, elsewhere
    "~/.conda/envs" "~/miniconda3/envs" "~/anaconda3/envs" "~/miniforge3/envs"
    "~/.pyenv/versions"
    "~/.local/share/uv/python")         ; uv's managed interpreters
  "Directories whose children are offered by `my/python-env-select'. Only for the picker: discovery never looks here."
  :type '(repeat directory)
  :group 'my/python)

(defcustom my/python-env-probe t
  "Whether to ask poetry, pipenv, pdm and conda where they keep an environment."
  :type 'boolean
  :group 'my/python)

(defcustom my/python-env-auto-label "Automatic"
  "The picker entry that goes back to working the environment out."
  :type 'string
  :group 'my/python)

(defcustom my/python-env-system-label "System python3"
  "The picker entry that means no environment at all."
  :type 'string
  :group 'my/python)

(defcustom my/python-env-other-label "Other directory..."
  "The picker entry that asks for an environment by path."
  :type 'string
  :group 'my/python)

;;;; What an environment is

(defun my/python--bin (env)
  "ENV's directory of executables."
  (file-name-as-directory
   (expand-file-name (if (eq system-type 'windows-nt) "Scripts" "bin") env)))

(defun my/python--interpreter (env)
  "ENV's Python."
  (expand-file-name (if (eq system-type 'windows-nt) "python.exe" "python")
                    (my/python--bin env)))

(defun my/python-env-p (env)
  "Non-nil when ENV is a directory with a Python in it."
  (and (stringp env)
       (file-executable-p (my/python--interpreter env))))

(defun my/python--name (env)
  "What to call ENV on a button."
  (if (null env)
      "system"
    (let* ((directory (directory-file-name env))
           (name (file-name-nondirectory directory)))
      (if (member name my/python-env-generic-names)
          (file-name-nondirectory
           (directory-file-name (file-name-directory directory)))
        name))))

(defun my/python--version (env)
  "ENV's Python version, or nil."
  (when-let* ((cfg (expand-file-name "pyvenv.cfg" env))
              ((file-readable-p cfg)))
    (with-temp-buffer
      (insert-file-contents cfg)
      (goto-char (point-min))
      (when (re-search-forward "^ *version\\(?:_info\\)? *= *\\(.+?\\) *$" nil t)
        (match-string 1)))))

(defun my/python--conda-p (env)
  "Non-nil when ENV is a conda environment rather than a virtualenv."
  (file-directory-p (expand-file-name "conda-meta" env)))

(defun my/python--vars (env)
  "The environment variables that put ENV in force, for m/task.el's `:env'."
  (list (cons (if (my/python--conda-p env) "CONDA_PREFIX" "VIRTUAL_ENV")
              (directory-file-name env))
        (cons "PATH" (concat (directory-file-name (my/python--bin env))
                             path-separator
                             (getenv-internal "PATH"
                                              (default-value 'process-environment))))
        (cons "PYTHONHOME" nil)))

;;;; Where to look

(defun my/python--root (&optional directory)
  "The project root DIRECTORY belongs to, or DIRECTORY itself."
  (let ((directory (expand-file-name (or directory default-directory))))
    (file-name-as-directory
     (or (when-let* (((require 'project nil t))
                     (project (project-current nil directory)))
           (expand-file-name (project-root project)))
         directory))))

(defun my/python--named-in (directory name)
  "The candidate environments NAME picks out inside DIRECTORY."
  (if (string-suffix-p "/*" name)
      (let ((parent (expand-file-name (substring name 0 -2) directory)))
        (when (file-directory-p parent)
          (directory-files parent t directory-files-no-dot-files-regexp)))
    (list (expand-file-name name directory))))

(defun my/python--find-venv (directory root)
  "The nearest virtualenv at or above DIRECTORY, going no higher than ROOT."
  (let ((directory (file-name-as-directory (expand-file-name directory)))
        (root (file-name-as-directory (expand-file-name root)))
        (found nil))
    (while (and (null found) directory)
      (setq found (seq-find #'my/python-env-p
                            (mapcan (lambda (name)
                                      (my/python--named-in directory name))
                                    my/python-env-search-names)))
      (setq directory
            (and (not (equal directory root))
                 (let ((up (file-name-directory (directory-file-name directory))))
                   (and (not (equal up directory)) up)))))
    found))

(defun my/python--discover (root &optional directory)
  "Work out ROOT's environment from the filesystem alone, or return nil."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (directory (if (string-prefix-p root directory) directory root)))
    (seq-find #'my/python-env-p
              (list (my/python--find-venv directory root)
                    (getenv "VIRTUAL_ENV")
                    (getenv "CONDA_PREFIX")
                    (my/python--pyenv-version root directory)))))

(defun my/python--pyenv-version (root directory)
  "The interpreter a .python-version at or above DIRECTORY names, or nil."
  (when-let* ((found (my/python--dominating ".python-version" directory root))
              (version (with-temp-buffer
                         (insert-file-contents found)
                         (string-trim (buffer-string))))
              ((not (string-empty-p version))))
    (expand-file-name version (expand-file-name "~/.pyenv/versions"))))

(defun my/python--dominating (name directory root)
  "NAME at or above DIRECTORY, no higher than ROOT, as a full path or nil."
  (when-let* ((directory (locate-dominating-file directory name))
              ((string-prefix-p (file-name-as-directory (expand-file-name root))
                                (file-name-as-directory
                                 (expand-file-name directory)))))
    (expand-file-name name directory)))

;;;; What is settled

(defvar my/python-env-selection nil
  "Environments chosen by hand, an alist of (ROOT .")

(defvar my/python--cache (make-hash-table :test #'equal)
  "Project root -> the environment found there, or `none'.")

(defvar my/python--probed (make-hash-table :test #'equal)
  "Project roots whose tools have already been asked.")

(defvar-local my/python--buffer-root nil
  "This buffer's project root, resolved once by `my/python-setup'. `my/python--root' calls `project-current', which walks.")

;;;; Asking the tools that hide envs

(defconst my/python--probes
  '((poetry (:pyproject "tool.poetry") ("poetry" "env" "info" "--path"))
    (pdm    (:pyproject "tool.pdm")    ("pdm" "venv" "--path" "in-project"))
    (pipenv (:file "Pipfile")          ("pipenv" "--venv"))
    (conda  (:file "environment.yml" "environment.yaml")
            ("conda" "env" "list" "--json")))
  "Tools that keep an environment somewhere only they know.")

(defun my/python--marker (root name)
  "ROOT's NAME, as a full path, when it is there to be read."
  (let ((file (expand-file-name name root)))
    (and (file-readable-p file) file)))

(defun my/python--pyproject-table-p (root table)
  "Non-nil when ROOT's pyproject.toml has a TABLE, or one under it."
  (when-let* ((file (my/python--marker root "pyproject.toml")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (and (re-search-forward (concat "^ *\\[" (regexp-quote table) "[].]")
                              nil t)
           t))))

(defun my/python--applicable-probes (root)
  "The probes worth running in ROOT, each as (TOOL . COMMAND)."
  (when my/python-env-probe
    (seq-keep
     (pcase-lambda (`(,tool ,when ,command))
       (and (executable-find (car command))
            (pcase when
              (`(:pyproject ,table) (my/python--pyproject-table-p root table))
              (`(:file . ,names)
               (seq-some (lambda (name) (my/python--marker root name)) names)))
            (cons tool command)))
     my/python--probes)))

(defun my/python--conda-name (root)
  "The environment name ROOT's environment.yml asks for, or nil."
  (when-let* ((file (or (my/python--marker root "environment.yml")
                        (my/python--marker root "environment.yaml"))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^ *name: *\\(.+?\\) *$" nil t)
        (match-string 1)))))

(defun my/python--parse-probe (tool output root)
  "The environment TOOL's OUTPUT names for ROOT, or nil."
  (if (eq tool 'conda)
      (when-let* ((name (my/python--conda-name root))
                  (envs (ignore-errors
                          (gethash "envs" (json-parse-string output)))))
        (seq-find (lambda (env)
                    (equal name (file-name-nondirectory
                                 (directory-file-name env))))
                  (append envs nil)))
    (seq-find (lambda (line) (not (string-blank-p line)))
              (split-string (string-trim output) "\n"))))

(defun my/python--probe-answer (tool output root)
  "TOOL's OUTPUT as an environment for ROOT, once it is known to be one."
  (when-let* ((env (my/python--parse-probe tool output root))
              (env (expand-file-name (string-trim env)))
              ((my/python-env-p env)))
    env))

(defun my/python--probe-now (root)
  "Ask ROOT's tools where its environment is and wait for the answer."
  (seq-some
   (pcase-lambda (`(,tool . ,command))
     (with-temp-buffer
       (let ((default-directory root))
         (when (eq 0 (apply #'call-process (car command) nil t nil (cdr command)))
           (my/python--probe-answer tool (buffer-string) root)))))
   (my/python--applicable-probes root)))

(defun my/python--probe-later (root)
  "Ask ROOT's tools where its environment is, and get on with things."
  (dolist (probe (my/python--applicable-probes root))
    (let* ((tool (car probe))
           (buffer (generate-new-buffer " *python env probe*"))
           (default-directory root))
      (make-process
       :name (format "python-env-%s" tool)
       :buffer buffer
       :command (cdr probe)
       :noquery t
       :connection-type 'pipe
       :sentinel
       (lambda (process _event)
         (unless (process-live-p process)
           (let ((output (with-current-buffer buffer (buffer-string)))
                 (ok (eq 0 (process-exit-status process))))
             (kill-buffer buffer)
             (when-let* ((ok)
                         (env (my/python--probe-answer tool output root))
                         ;; Not if a faster probe, or a choice, got there first.
                         ((not (stringp (gethash root my/python--cache)))))
               (puthash root env my/python--cache)
               (my/python--reapply root)
               (run-hooks 'my/task-state-change-hook)))))))))

;;;; Settling on one

(defun my/python--resolve (root sync)
  "Work out ROOT's environment, remember it, and return it or `none'."
  (let ((answer (or (my/python--discover root)
                    (and sync (my/python--probe root t))
                    'none)))
    (puthash root answer my/python--cache)
    (unless (stringp answer) (my/python--probe root))
    answer))

(defun my/python--probe (root &optional sync)
  "Ask ROOT's tools where its environment is, once a session."
  (unless (gethash root my/python--probed)
    (puthash root t my/python--probed)
    (if sync (my/python--probe-now root) (my/python--probe-later root) nil)))

(defun my/python-env (&optional root sync)
  "The environment ROOT's Python files run in, or nil for the system's."
  (let* ((root (or root my/python--buffer-root (my/python--root)))
         (chosen (assoc-default root my/python-env-selection))
         (answer (cond ((eq chosen 'none) 'none)
                       ((my/python-env-p chosen) chosen)
                       (t (or (gethash root my/python--cache)
                              (my/python--resolve root sync))))))
    (when (and sync (not (stringp answer)) (null chosen))
      (setq answer (or (my/python--probe root t) answer))
      (puthash root answer my/python--cache))
    (and (stringp answer) answer)))

;;;; The provider m/task.el asks for

(defun my/python-env-vars ()
  "The environment this buffer's Run and Build should use.  m/task.el's `:vars'."
  (when-let* ((env (my/python-env nil 'sync)))
    (my/python--vars env)))

(defun my/python-env-label ()
  "The environment's name, for the tool bar's button."
  (let ((answer (and my/python--buffer-root
                     (or (assoc-default my/python--buffer-root
                                        my/python-env-selection)
                         (gethash my/python--buffer-root my/python--cache)))))
    (my/python--name (and (stringp answer) answer))))

;;;; Choosing one

(defun my/python--known-envs (root)
  "Every environment worth offering for ROOT, the project's own first."
  (delete-dups
   (delq nil
         (append (list (my/python--discover root) (my/python-env root))
                 (mapcan
                  (lambda (directory)
                    (let ((directory (expand-file-name directory)))
                      (when (file-directory-p directory)
                        (seq-filter
                         #'my/python-env-p
                         (directory-files directory t
                                          directory-files-no-dot-files-regexp)))))
                  my/python-env-directories)))))

(defun my/python--annotate (env detected)
  "The line shown beside ENV in the picker."
  (string-join
   (delq nil (list (my/python--version env)
                   (and (equal env detected) "detected")))
   "  "))

(defun my/python--completion-table (candidates)
  "A completion table over CANDIDATES, an alist of (DISPLAY ."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata
          (category . my/python-env)
          (display-sort-function . identity)
          (cycle-sort-function . identity)
          (annotation-function
           . ,(lambda (display)
                (when-let* ((annotation (alist-get display candidates
                                                   nil nil #'equal))
                            ((not (string-empty-p annotation))))
                  (concat "  " (propertize annotation
                                           'face 'completions-annotations))))))
      (complete-with-action action candidates string predicate))))

(defun my/python-env-select ()
  "Choose which Python this project's files run in."
  (interactive)
  (let* ((root (or my/python--buffer-root (my/python--root)))
         (detected (my/python--discover root))
         (envs (my/python--known-envs root))
         (choices (append
                   (mapcar (lambda (env)
                             (cons (abbreviate-file-name (directory-file-name env))
                                   env))
                           envs)
                   (list (cons my/python-env-auto-label 'auto)
                         (cons my/python-env-system-label 'none)
                         (cons my/python-env-other-label 'other))))
         (annotations (mapcar (lambda (choice)
                                (cons (car choice)
                                      (if (stringp (cdr choice))
                                          (my/python--annotate (cdr choice) detected)
                                        "")))
                              choices))
         (current (when-let* ((env (my/python-env root)))
                    (abbreviate-file-name (directory-file-name env))))
         (answer (alist-get (completing-read "Python: "
                                             (my/python--completion-table annotations)
                                             nil t nil nil current)
                            choices nil nil #'equal)))
    (when (eq answer 'other)
      (setq answer (expand-file-name
                    (read-directory-name "Environment directory: ")))
      (unless (my/python-env-p answer)
        (user-error "No Python in %s"
                    (abbreviate-file-name (my/python--bin answer)))))
    (if (eq answer 'auto)
        (setq my/python-env-selection
              (assoc-delete-all root my/python-env-selection))
      (setf (alist-get root my/python-env-selection nil nil #'equal)
            (if (stringp answer) (directory-file-name answer) 'none)))
    (my/python--reapply root)
    (run-hooks 'my/task-state-change-hook)
    (message "Python: %s" (my/python--name (my/python-env root)))))

(defun my/python-env-refresh ()
  "Look for this project's environment again."
  (interactive)
  (let ((root (or my/python--buffer-root (my/python--root))))
    (remhash root my/python--cache)
    (remhash root my/python--probed)
    (my/python-env root 'sync)
    (my/python--reapply root)
    (run-hooks 'my/task-state-change-hook)
    (message "Python: %s" (my/python--name (my/python-env root)))))

;;;; Pointing Emacs's own Python at it

(defun my/python--apply ()
  "Point this buffer's Python at `my/python-env'."
  (when (derived-mode-p 'python-base-mode)
    (let* ((env (my/python-env))
           (vars (and env (my/python--vars env))))
      (setq-local python-shell-interpreter
                  (if env
                      (my/python--interpreter env)
                    (default-value 'python-shell-interpreter))
                  python-shell-virtualenv-root (and env (directory-file-name env))
                  exec-path (my/task--exec-path vars (default-value 'exec-path))
                  process-environment (my/task--environment
                                       vars (default-value 'process-environment))))))

(defun my/python--reapply (root)
  "Apply ROOT's environment to every Python buffer that belongs to it."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (equal my/python--buffer-root root)
        (my/python--apply)))))

;;;; Setup

(defun my/python-setup ()
  "Settle this buffer's project root and environment.  On the mode hook."
  (setq-local my/python--buffer-root (my/python--root))
  (my/python--apply))

(add-hook 'python-base-mode-hook #'my/python-setup)

;;; py.el ends here
