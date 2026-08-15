;;; task.el --- Running and building the file you are editing -*- lexical-binding: t; -*-

;;; Commentary:
;; run/build/stop for the current file. bar.el draws the buttons.

;;; Code:

(require 'format-spec)

;;;; Forward declarations

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(declare-function compilation-start "compile"
                  (command &optional mode name-function highlight-regexp))
(declare-function kill-compilation "compile" ())

(declare-function my/term--open "term" (directory backend))
(declare-function my/term--show-panel "term" (&optional select))
(declare-function my/term--gc "term" ())
(declare-function my/term--busy "term" (session))
(declare-function my/term-rename "term" (session new-name))
(declare-function my/term-session-name "term" (session))
(declare-function my/term-session-buffer "term" (session))

(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-interrupt-process "esh-proc" ())

(declare-function TeX-active-process "tex" ())
(declare-function TeX-kill-job "tex" ())
(declare-function TeX-command "tex" (name file &optional override-confirm))
(declare-function TeX-master-file "tex" (&optional extension nondirectory ask))

(defvar my/term--sessions)
(defvar my/term-backend)
(defvar my/term--current)
(defvar my/term--name-the-command)
(defvar TeX-command-default)
(defvar compilation-always-kill)
(defvar compilation-buffer-name-function)

;;;; Options

(defgroup my/task nil
  "Running and building the file you are editing."
  :group 'tools
  :prefix "my/task-")

(defcustom my/task-run-target 'terminal
  "Where a run command's output goes."
  :type '(choice (const terminal) (const compile))
  :group 'my/task)

(defcustom my/task-build-target 'compile
  "Where a build command's output goes.  See `my/task-run-target'."
  :type '(choice (const terminal) (const compile))
  :group 'my/task)

(defcustom my/task-terminal-name "run"
  "Name of the terminal session runs are sent to."
  :type 'string
  :group 'my/task)

(defcustom my/task-build-buffer-name "*build*"
  "Name of the compilation buffer builds go to."
  :type 'string
  :group 'my/task)

(defcustom my/task-save-before-run t
  "Whether to save the buffer before running or building it."
  :type 'boolean
  :group 'my/task)

(defcustom my/task-focus-terminal t
  "Whether starting a run moves point into the terminal."
  :type 'boolean
  :group 'my/task)

(defvar my/task-state-change-hook nil
  "Run when a task starts, is stopped, or a different one is picked.")

;;;; The table

(defcustom my/task-commands
  '((python-base-mode
     :dir file
     ;; `python3' and `pytest' unqualified.
     :env (:vars   my/python-env-vars
           :label  my/python-env-label
           :select my/python-env-select)
     ;; -u: unbuffered, or output arrives in 8K lumps long after it was printed.
     :run   (("Run file"      "python3 -u %f")
             ("Debug"         "python3 -u -m pdb %f")
             ("Test file"     "pytest %f")
             ("Test all"      "pytest" :dir project))
     :build (("Byte compile"  "python3 -m compileall %f")))

    ((go-mode go-ts-mode)
     :dir project
     :run   (("Run package"   "go run .")
             ("Run file"      "go run %f" :dir file))
     :build (("Build"         "go build ./...")
             ("Test"          "go test ./...")
             ("Vet"           "go vet ./...")))

    ((c-mode c-ts-mode)
     :dir file
     :run   (("Compile & run" "cc -std=c17 -Wall -Wextra -g -o %b %f && ./%b")
             ("Run binary"    "./%b"))
     :build (("Compile"       "cc -std=c17 -Wall -Wextra -g -o %b %f")
             ("Make"          "make" :dir project)))

    ((c++-mode c++-ts-mode)
     :dir file
     :run   (("Compile & run" "c++ -std=c++20 -Wall -Wextra -g -o %b %f && ./%b")
             ("Run binary"    "./%b"))
     :build (("Compile"       "c++ -std=c++20 -Wall -Wextra -g -o %b %f")
             ("Make"          "make" :dir project)))

    ((rust-mode rust-ts-mode)
     :dir project
     :run   (("Run"           "cargo run"))
     :build (("Build"         "cargo build")
             ("Test"          "cargo test")
             ("Clippy"        "cargo clippy")))

    (zig-mode
     :dir file
     :run   (("Run file"      "zig run %f")
             ("Test file"     "zig test %f"))
     :build (("Build"         "zig build" :dir project)
             ("Build exe"     "zig build-exe %f")))

    ((sh-mode bash-ts-mode)
     :dir file
     :run   (("Run"           "sh %f")
             ("Run with bash" "bash %f"))
     :build (("Shellcheck"    "shellcheck %f")))

    (emacs-lisp-mode
     :dir file
     :run   (("Eval buffer"   eval-buffer)
             ("Load file"     load-file))
     :build (("Byte compile"  "emacs -Q --batch -f batch-byte-compile %f")))

    (TeX-mode
     :build (("Build"            my/task-tex-build)
             ("Build all passes" TeX-command-run-all)))

    (prog-mode
     :dir project
     :build (("Make"          "make")
             ("Ask"           compile))))
  "Run and build commands, per major mode."
  :type '(alist :key-type sexp :value-type sexp)
  :group 'my/task)

;;;; Which entry applies

(defvar my/task-user-commands nil
  "Commands added from the picker, an alist shaped like `my/task-commands'. Keyed by the major mode they were added in rather than by the table key that mode resolved to. A command added in a `js-ts-mode' buffer, where the only entry that matches is the `prog-mode' fallback at the bottom of the table, belongs to JavaScript -- keying it by what matched would put it in every C and Go buffer as well. Saved by `savehist-additional-variables' \\(m/persist.el).")

(defun my/task--kind-key (kind)
  "The plist key an entry keeps KIND's -- `run' or `build' -- tasks under."
  (if (eq kind 'run) :run :build))

(defun my/task--key-matches-p (key mode)
  "Non-nil when table KEY -- a mode or a list of them -- names MODE."
  (if (listp key) (memq mode key) (eq key mode)))

(defun my/task--table-entry (mode)
  "The `my/task-commands' entry that applies to MODE, or nil."
  (seq-some (lambda (parent)
              (seq-find (lambda (entry)
                          (my/task--key-matches-p (car entry) parent))
                        my/task-commands))
            (derived-mode-all-parents mode)))

(defun my/task--user-tasks (kind mode)
  "Every task of KIND added for MODE or a mode it derives from."
  (mapcan (lambda (parent)
            (copy-sequence
             (plist-get (alist-get parent my/task-user-commands)
                        (my/task--kind-key kind))))
          (derived-mode-all-parents mode)))

(defun my/task--entry (&optional mode)
  "The task entry that applies to MODE, or nil."
  (let* ((mode (or mode major-mode))
         (entry (my/task--table-entry mode)))
    (if (null my/task-user-commands)
        entry
      (let ((run (append (plist-get (cdr entry) :run)
                         (my/task--user-tasks 'run mode)))
            (build (append (plist-get (cdr entry) :build)
                           (my/task--user-tasks 'build mode))))
        (when (or run build)
          (list (or (car entry) mode)
                :dir (plist-get (cdr entry) :dir)
                :env (plist-get (cdr entry) :env)
                :run run
                :build build))))))

(defvar my/task--last-buffer nil
  "The buffer the last task was started from.")

(defun my/task--source-buffer ()
  "The buffer whose tasks Run, Build and the picker should act on."
  (cond ((my/task--entry) (current-buffer))
        ((and (buffer-live-p my/task--last-buffer)
              (with-current-buffer my/task--last-buffer (my/task--entry)))
         my/task--last-buffer)))

(defun my/task-tasks (kind &optional entry)
  "The tasks of KIND -- `run' or `build' -- available here."
  (plist-get (cdr (or entry (my/task--entry))) (my/task--kind-key kind)))

(defun my/task-available-p (kind)
  "Non-nil when this buffer has any task of KIND."
  (and (my/task-tasks kind) t))

;;;; Which one is picked

(defvar my/task-selection nil
  "Remembered pick per mode: an alist of (KEY .")

(defun my/task-current (kind &optional entry)
  "The task of KIND that Run or Build would start, or nil."
  (let* ((entry (or entry (my/task--entry)))
         (tasks (my/task-tasks kind entry))
         (label (alist-get kind (assoc-default (car entry) my/task-selection))))
    (or (and label (assoc label tasks)) (car tasks))))

(defun my/task-label (kind)
  "The label of the picked task of KIND, or nil when there is none."
  (car (my/task-current kind)))

(defcustom my/task-new-label "New command..."
  "The picker's last entry, which adds a command rather than choosing one."
  :type 'string
  :group 'my/task)

(defun my/task--completion-table (tasks &optional extra)
  "A completion table over TASKS' labels, annotated with what each runs."
  (let ((annotations
         (append (mapcar (lambda (task)
                           (cons (car task)
                                 (let ((command (nth 1 task)))
                                   (if (symbolp command)
                                       (symbol-name command)
                                     command))))
                         tasks)
                 (and extra (list extra)))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata
            (category . my/task)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (annotation-function
             . ,(lambda (label)
                  (when-let* ((command (alist-get label annotations
                                                  nil nil #'equal)))
                    (concat "  " (propertize command 'face 'completions-annotations))))))
        (complete-with-action action annotations string predicate)))))

(defun my/task--remember (kind key label)
  "Remember that KIND's button starts LABEL in the mode KEY names."
  (let ((per-mode (assoc-default key my/task-selection)))
    (setf (alist-get kind per-mode) label)
    (setf (alist-get key my/task-selection nil nil #'equal) per-mode)
    (run-hooks 'my/task-state-change-hook)))

(defun my/task-select (kind)
  "Pick which task of KIND -- `run' or `build' -- this mode's button starts."
  (interactive (list (intern (completing-read "Which button: "
                                              '("run" "build") nil t))))
  (with-current-buffer (or (my/task--source-buffer) (current-buffer))
    (let* ((entry (my/task--entry))
           (tasks (my/task-tasks kind entry)))
      (if (null tasks)
          (my/task-add kind)
        (let ((label (completing-read
                      (format "%s command: " (capitalize (symbol-name kind)))
                      (my/task--completion-table
                       tasks (cons my/task-new-label "add one"))
                      nil t nil nil (my/task-label kind))))
          (if (equal label my/task-new-label)
              (my/task-add kind)
            (my/task--remember kind (car entry) label)
            (message "%s: %s" (capitalize (symbol-name kind)) label)))))))

(defun my/task-add (kind &optional ask-directory)
  "Add a command of KIND -- `run' or `build' -- for this mode, and pick it."
  (interactive (list (intern (completing-read "Add a command for: "
                                              '("run" "build") nil t))
                     current-prefix-arg))
  (with-current-buffer (or (my/task--source-buffer) (current-buffer))
    (let* ((entry (my/task--entry))
           (current (nth 1 (my/task-current kind entry)))
           (command (read-shell-command
                     (format "%s command (%%f this file, %%p the project root): "
                             (capitalize (symbol-name kind)))
                     (and (stringp current) current)))
           (default (progn
                      (when (string-blank-p command)
                        (user-error "No command given"))
                      (capitalize (car (split-string command)))))
           (label (read-string (format "Label (%s): " default) nil nil default))
           (directory
            (when ask-directory
              (intern (completing-read
                       "Run it in: " '("file" "project") nil t nil nil
                       (symbol-name (or (plist-get (cdr entry) :dir) 'project)))))))
      (when (or (equal label my/task-new-label)
                (assoc label (my/task-tasks kind entry)))
        (user-error "There is already a %s command called %s" kind label))
      (let* ((key (my/task--kind-key kind))
             (per-mode (alist-get major-mode my/task-user-commands))
             (task (append (list label command)
                           (when directory (list :dir directory)))))
        (setf (plist-get per-mode key)
              (append (plist-get per-mode key) (list task)))
        (setf (alist-get major-mode my/task-user-commands) per-mode)
        ;; After the add, not before: in a mode the table had nothing for, this command is what brings.
        (my/task--remember kind (car (my/task--entry)) label)
        (message "%s: %s" (capitalize (symbol-name kind)) label)))))

(defun my/task-forget (kind)
  "Drop a command of KIND that was added with `my/task-add'. Only those: the table's own commands are not the picker's to."
  (interactive (list (intern (completing-read "Forget a command for: "
                                              '("run" "build") nil t))))
  (with-current-buffer (or (my/task--source-buffer) (current-buffer))
    (let* ((key (my/task--kind-key kind))
           (per-mode (alist-get major-mode my/task-user-commands))
           (tasks (plist-get per-mode key)))
      (unless tasks
        (user-error "No %s command was added in %s" kind
                    (format-mode-line mode-name)))
      (let ((label (completing-read (format "Forget which %s command: " kind)
                                    (my/task--completion-table tasks) nil t)))
        (setf (plist-get per-mode key) (assoc-delete-all label tasks))
        (setf (alist-get major-mode my/task-user-commands) per-mode)
        (when-let* ((entry (my/task--entry))
                    (per (assoc-default (car entry) my/task-selection))
                    ((equal label (alist-get kind per))))
          (setf (alist-get kind per) nil)
          (setf (alist-get (car entry) my/task-selection nil nil #'equal) per))
        (run-hooks 'my/task-state-change-hook)
        (message "Forgot %s" label)))))

(defun my/task-select-run ()
  "Pick which command the Run button starts in this mode."
  (interactive)
  (my/task-select 'run))

(defun my/task-select-build ()
  "Pick which command the Build button starts in this mode."
  (interactive)
  (my/task-select 'build))

;;;; Turning a task into a command line

(defun my/task--directory (task entry)
  "Where TASK from ENTRY should run."
  (let* ((where (or (plist-get (cddr task) :dir)
                    (plist-get (cdr entry) :dir)
                    'project))
         (dir (if buffer-file-name
                  (file-name-directory (expand-file-name buffer-file-name))
                (expand-file-name default-directory)))
         (root (and (eq where 'project)
                    (require 'project nil t)
                    (when-let* ((project (project-current nil dir)))
                      (expand-file-name (project-root project))))))
    (file-name-as-directory (or root dir))))

(defconst my/task--file-specifiers "%[frbBd]"
  "Regexp for the specifiers that need the buffer to be visiting a file.")

(defun my/task--quote (string)
  "STRING, shell-quoted, or the empty string when it is nil."
  (shell-quote-argument (or string "")))

(defun my/task--expand (command directory)
  "Expand COMMAND's %-specifiers for this buffer, to be run in DIRECTORY."
  (let ((file (and buffer-file-name (expand-file-name buffer-file-name))))
    (when (and (null file) (string-match-p my/task--file-specifiers command))
      (user-error "This command needs a file; save the buffer first"))
    (format-spec
     command
     `((?f . ,(my/task--quote file))
       (?r . ,(my/task--quote (and file (file-relative-name file directory))))
       (?b . ,(my/task--quote (and file (file-name-base file))))
       (?B . ,(my/task--quote (and file (file-name-nondirectory file))))
       (?d . ,(my/task--quote (and file (file-name-directory file))))
       (?p . ,(my/task--quote
               (or (when-let* (((require 'project nil t))
                               (project (project-current nil directory)))
                     (expand-file-name (project-root project)))
                   directory)))))))

;;;; The environment a task runs in

(defun my/task--env-plist (&optional entry)
  "The `:env' plist of ENTRY, or of the one that applies here."
  (plist-get (cdr (or entry (my/task--entry))) :env))

(defun my/task--env-function (part &optional entry)
  "PART -- `:vars', `:label' or `:select' -- of this entry's env provider."
  (when-let* ((plist (my/task--env-plist entry))
              (symbol (plist-get plist part))
              ((fboundp symbol)))
    symbol))

(defun my/task-env-available-p ()
  "Non-nil when this buffer's tasks run in an environment that can be chosen."
  (and (my/task--env-function :select) t))

(defun my/task-env-vars (&optional entry)
  "The environment ENTRY's tasks should run in, as an alist, or nil."
  (when-let* ((resolve (my/task--env-function :vars entry)))
    (funcall resolve)))

(defun my/task-env-label ()
  "What the environment button should read, or nil when there is none."
  (when-let* ((label (my/task--env-function :label)))
    (funcall label)))

(defun my/task-env-select ()
  "Choose the environment this buffer's tasks run in."
  (interactive)
  (with-current-buffer (or (my/task--source-buffer) (current-buffer))
    (call-interactively
     (or (my/task--env-function :select)
         (user-error "No environment to choose in %s"
                     (format-mode-line mode-name))))))

(defun my/task--environment (vars &optional environment)
  "ENVIRONMENT -- `process-environment' by default -- with VARS applied."
  (let ((environment (or environment process-environment)))
    (dolist (pair vars environment)
      (setq environment
            (cons (if (cdr pair)
                      (concat (car pair) "=" (cdr pair))
                    (car pair))
                  environment)))))

(defun my/task--exec-path (vars path)
  "PATH with the directories VARS' own PATH puts in front of it."
  (if-let* ((value (cdr (assoc "PATH" vars))))
      (append (seq-difference (split-string value path-separator t) path) path)
    path))

;;;; Running it: the compilation buffer

(defun my/task--compile (command directory &optional vars)
  "Start COMMAND in DIRECTORY in the build buffer, with VARS in force."
  (let ((default-directory directory)
        (compilation-always-kill t)
        (compilation-buffer-name-function
         (lambda (&rest _) my/task-build-buffer-name))
        (process-environment (my/task--environment vars))
        (exec-path (my/task--exec-path vars exec-path)))
    (compilation-start command)))

;;;; Running it: the terminal panel

(defun my/task--session ()
  "The session runs are sent to, or nil when it has not been opened yet."
  (when (bound-and-true-p my/term--sessions)
    (my/term--gc)
    (seq-find (lambda (session)
                (equal (my/term-session-name session) my/task-terminal-name))
              my/term--sessions)))

(defun my/task--ensure-session (directory)
  "The run session, opened in DIRECTORY if it does not exist yet."
  (or (my/task--session)
      (let ((session (my/term--open directory my/term-backend)))
        (my/term-rename session my/task-terminal-name)
        session)))

(defun my/task--terminal-busy-p (session)
  "Non-nil when SESSION's shell has handed the terminal to a command."
  (and session
       (ignore-errors
         (let ((my/term--name-the-command nil))
           (and (my/term--busy session) t)))))

(defun my/task--send (session line)
  "Type LINE into SESSION and press return."
  (when-let* ((buffer (my/term-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (if (derived-mode-p 'eshell-mode)
          (progn (goto-char (point-max))
                 (insert line)
                 (eshell-send-input))
        (let ((process (get-buffer-process buffer)))
          (unless (process-live-p process)
            (user-error "The %s terminal has no shell running" my/task-terminal-name))
          (process-send-string process (concat line "\n")))))))

(defun my/task--eshell-p (session)
  "Non-nil when SESSION's buffer is an eshell rather than a shell."
  (when-let* ((buffer (and session (my/term-session-buffer session)))
              ((buffer-live-p buffer)))
    (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                             'eshell-mode)))

(defun my/task--sh-quote (value)
  "VALUE as one shell word, in single quotes."
  (concat "'" (string-replace "'" "'\\''" value) "'"))

(defvar my/task--exported nil
  "Names of the variables the last run exported into the session.")

(defun my/task--env-commands (vars eshell)
  "Shell commands that put VARS into the run session, in order."
  (let* ((set (seq-filter #'cdr vars))
         (unset (append (seq-difference my/task--exported (mapcar #'car set))
                        (mapcar #'car (seq-remove #'cdr vars)))))
    (setq my/task--exported (mapcar #'car set))
    (if eshell
        (append (mapcar (lambda (name) (concat "setenv " name)) unset)
                (mapcar (lambda (pair)
                          (format "setenv %s %s" (car pair)
                                  (my/task--sh-quote (cdr pair))))
                        set))
      (delq nil
            (list (when unset (concat "unset " (string-join unset " ")))
                  (when set
                    (concat "export "
                            (string-join
                             (mapcar (lambda (pair)
                                       (concat (car pair) "="
                                               (my/task--sh-quote (cdr pair))))
                                     set)
                             " "))))))))

(defun my/task--terminal (command directory &optional vars)
  "Send COMMAND to the run session, opening the panel on it first."
  (let* ((session (my/task--ensure-session directory))
         (line (string-join
                (append (list (format "cd %s" (shell-quote-argument directory)))
                        (my/task--env-commands vars (my/task--eshell-p session))
                        (list command))
                " && ")))
    (setq my/term--current session)
    (my/term--show-panel my/task-focus-terminal)
    (if (my/task--terminal-busy-p session)
        (progn (my/task--interrupt session)
               (run-at-time 0.2 nil #'my/task--send session line))
      (my/task--send session line))))

;;;; Stopping

(defun my/task--tex-process ()
  "The AUCTeX process this buffer's document is being built by, or nil."
  (and (derived-mode-p 'TeX-mode)
       (fboundp 'TeX-active-process)
       (bound-and-true-p TeX-master)
       (ignore-errors (TeX-active-process))))

(defun my/task--build-process ()
  "The live process in the build buffer, or nil."
  (when-let* ((buffer (get-buffer my/task-build-buffer-name))
              (process (get-buffer-process buffer))
              ((process-live-p process)))
    process))

(defun my/task-running-p ()
  "Non-nil when a task started from here is still going."
  (or (and (my/task--tex-process) t)
      (and (my/task--build-process) t)
      (my/task--terminal-busy-p (my/task--session))))

(defun my/task--interrupt (session &optional hard)
  "Interrupt whatever SESSION is running; HARD escalates."
  (when-let* ((buffer (my/term-session-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (if (derived-mode-p 'eshell-mode)
          (eshell-interrupt-process)
        (when-let* ((process (get-buffer-process buffer))
                    ((process-live-p process)))
          (let ((child (process-running-child-p process)))
            (cond
             ((not hard) (process-send-string process "\003"))
             ((integerp child) (signal-process child 'KILL))
             (t (process-send-string process "\034")))))))))

(defun my/task-stop ()
  "Interrupt whatever this buffer's Run and Build buttons started."
  (interactive)
  (let ((hard (eq last-command 'my/task-stop))
        (stopped nil))
    (when-let* ((process (my/task--tex-process)))
      (if hard (delete-process process) (TeX-kill-job))
      (push "the TeX run" stopped))
    (when-let* ((process (my/task--build-process)))
      (if hard
          (delete-process process)
        (with-current-buffer (process-buffer process) (kill-compilation)))
      (push "the build" stopped))
    (when-let* ((session (my/task--session))
                ((my/task--terminal-busy-p session)))
      (my/task--interrupt session hard)
      (push "the run" stopped))
    (run-hooks 'my/task-state-change-hook)
    (unless stopped (user-error "Nothing is running"))
    (message "%s %s" (if hard "Killed" "Interrupted")
             (string-join (nreverse stopped) " and "))))

;;;; Commands

(defun my/task-tex-build ()
  "Run one pass of the default TeX command on the master file."
  (interactive)
  (TeX-command TeX-command-default #'TeX-master-file 0))

(defun my/task--start (kind)
  "Start the picked task of KIND -- `run' or `build'."
  (with-current-buffer (or (my/task--source-buffer)
                           (user-error "No %s command for %s" kind
                                       (format-mode-line mode-name)))
    (let* ((entry (my/task--entry))
           (task (my/task-current kind entry)))
      (unless task
        (user-error "No %s command for %s" kind (format-mode-line mode-name)))
      (when (and my/task-save-before-run buffer-file-name (buffer-modified-p))
        (save-buffer))
      (setq my/task--last-buffer (current-buffer))
      (let ((command (nth 1 task))
            (directory (my/task--directory task entry))
            (vars (my/task-env-vars entry)))
        (if (symbolp command)
            ;; An interactive command, not a shell one: AUCTeX.
            (let ((process-environment (my/task--environment vars))
                  (exec-path (my/task--exec-path vars exec-path)))
              (call-interactively command))
          (let ((line (my/task--expand command directory))
                (target (if (eq kind 'run) my/task-run-target my/task-build-target)))
            (if (eq target 'terminal)
                (my/task--terminal line directory vars)
              (my/task--compile line directory vars)))))
      (run-hooks 'my/task-state-change-hook))))

(defun my/task-run (&optional pick)
  "Run this file, with whatever `my/task-commands' says that means. With PICK -- a prefix argument -- choose the command."
  (interactive "P")
  (when pick (my/task-select 'run))
  (my/task--start 'run))

(defun my/task-build (&optional pick)
  "Build this file, with whatever `my/task-commands' says that means. With PICK -- a prefix argument -- choose the command."
  (interactive "P")
  (when pick (my/task-select 'build))
  (my/task--start 'build))

;;;; Setup

(defun my/task--announce (&rest _)
  "Tell `my/task-state-change-hook' that something changed. Takes and ignores arguments: `compilation-finish-functions' passes two."
  (run-hooks 'my/task-state-change-hook))

;; A build started any other way -- `compile', `project-compile', AUCTeX --.
(add-hook 'compilation-start-hook #'my/task--announce)
(add-hook 'compilation-finish-functions #'my/task--announce)

;;; task.el ends here
