;;; dockerfile-dsl.el --- Spacemacs layers dependencies installation DSL File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(let ((default-directory  "/usr/local/spacemacs/lib/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'cl)
(require 'f)
(require 'dash)
(require 's)

(defconst env-fp "/etc/environment")
(defconst dump-layer-data-fp "/tmp/docker-spacemacs-layer-data.el")

(defvar silent nil
  "If non-nil disable verbose command and `!' print logging.")

(defalias 'l 'list)
(defalias 'v 'vector)
(defalias 'dir 'file-name-as-directory)
(defalias 'file 'directory-file-name)

(defmacro install (&rest packages)
  "Install PACKAGES."
  `(when (-non-nil ',packages)
     (bash-command-to-string
      (format "apt-get update && apt-get install %s"
              (mapconcat 'symbol-name
                         ',packages
                         " ")))))

(defmacro uninstall (&rest packages)
  "Uninstall PACKAGES."
  `(when (-non-nil ',packages)
     (bash-command-to-string
      (format "apt-get remove %s"
              (mapconcat 'symbol-name
                         ',packages
                         " ")))
     (cleanup)))

(defun cleanup ()
  "Clean package repository cache and dependencies."
  (bash-command-to-string "apt-get autoremove")
  (bash-command-to-string "apt-get clean")
  (bash-command-to-string "rm -rf /var/lib/apt/lists/*"))

(defun set-glob-env (variable &optional value)
  "Set value of VARIABLE to VALUE with `setenv'
and in the `env-fp' file (remove if VALUE nil). Create
constant $VARIABLE with value VALUE and return its symbol."
  (when variable
    (with-temp-file env-fp
      (insert-file-contents env-fp)
      (goto-char (point-min))
      (delete-matching-lines (format "^%s=.*" variable))
      (setenv variable value)
      (when value
        (goto-char (point-max))
        (insert (format "%s=\"%s\"\n" variable value)))
      (goto-char (point-min))))
  (! `("Set global variable \"%s\" to \"%s\"" ,variable ,value))
  (eval `(defconst ,(intern (concat "$" variable)) ,value)))

(defun set-glob-envs (&rest var-val-alists)
  "Call `set-glob-env' for each (variable . value)
in VAR-VAL-ALISTS."
  (when var-val-alists
    (dolist (vva var-val-alists)
      (set-glob-env (car vva) (cdr vva)))))

(defun get-glob-env (variable)
  "Get value of VARIABLE in `env-fp' file and create $VARIABLE constant.
Return its value (nil if VARIABLE not present.)"
  (when variable
    (with-temp-buffer
      (insert-file-contents env-fp)
      (goto-char (point-max))
      (let ((res))
        (when (re-search-backward (format "^%s=\"\\(.*\\)\"$" variable)
                                  nil
                                  t
                                  1)
          (setq res (match-string 1)))
        (! `("Get global variable \"%s\" value is \"%s\"" ,variable ,res))
        (symbol-value
         (eval `(defconst ,(intern (concat "$" variable)) ,res)))))))

(defun append-glob-var-val (var separator &rest vals)
  "Add VALS to the value of VAR variable in
 the `env-fp' file end export it with `set-glob-env'.
Use SEPARATOR to separate val (i.e. \":\" in PATH).
NOTE: Duplicates are removed."
  (let* ((old-val (get-glob-env var))
         (new-val (mapconcat
                   'identity
                   (-non-nil (cons old-val
                                   vals))
                   separator))
         (res-val (mapconcat 'identity
                             (remove-duplicates (split-string new-val
                                                              separator)
                                                :test 'string=)
                             separator)))
    (set-glob-env var res-val)))

(defun add-glob-paths (&rest paths)
  "Add PATHS to the value of PATH variable
with `append-glob-var-val'."
  (apply 'append-glob-var-val "PATH" ":" paths))

(defun $ (&rest cmds)
  "Apply `bash-command-to-string' to each CMD in CMDS.
Return the last result."
  (let ((retval nil))
    (dolist (cmd cmds retval)
      (setq retval (bash-command-to-string cmd)))))

(defun ! (msg)
  "Print MSG unless `silent' is non-nil."
  (unless silent
    (princ (if (s-ends-with? msg "\n")
               msg
             (s-append msg "\n")))))

;; Fix Docker build log mangling.
(add-hook 'kill-emacs-hook (lambda () (princ "\n")))

(defun flatten-smart-string (cmd)
  "Return CMD if it's a string.
(apply 'format cmd) if it's a list and
Recursivly applay itself to the each element of CMD
if it's a vector and then concat to string.
NOTE: \"smart\" like in \"smart code\"."
(cond ((stringp cmd)
       cmd)
      ((vectorp cmd)
       (mapconcat 'flatten-smart-string cmd " "))
      ((consp cmd)
       (apply 'format cmd))))

(defun flatten-smart-string-filter-args (args)
  "Map `flatten-smart-string' to each ARG in ARGS and return
the result."
  (mapcar 'flatten-smart-string args))

(defun rm (&rest paths)
  "Force delete PATHS."
  (dolist (path paths)
    (dolist (exp-path (f-glob path))
      (! `("Deleting \"%s\"" ,exp-path))
      (f-delete exp-path t))))

(defun mkdirp (&rest dirs)
  "Make each DIR in DIRS and their parents."
  (dolist (dir dirs)
    (! `("Creating directory \"%s\"" ,(f-expand dir)))
    (make-directory (f-expand dir) t)))

(defun mv (from to)
  "Move file or dir FROM TO."
  (dolist (item (f-glob from))
    (! `("Moving \"%s\" to \"%s\"" ,item ,to))
    (f-move item to)))

(defun cp (from to)
  "Copy file or dir FROM TO."
  (dolist (item (f-glob from))
    (! `("Copy \"%s\" to \"%s\"" ,item ,to))
    (f-copy item to)))

(dolist (func '(touch
                !
                $
                cd
                rm
                mv
                cp
                mkdirp))
  (advice-add func :filter-args  #'flatten-smart-string-filter-args))

(defun bash-command-to-string (command)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (when command
    (! `("Command:\n%s" ,command))
    (with-temp-buffer
      (let* ((exit-code
              (call-process "/bin/bash" nil (current-buffer) nil "-c" command))
             (rets (buffer-string)))
        (unless (= exit-code 0)
          (error (format (concat "Command:\n/bin/bash -c %s\n"
                                 "Output:\n%s\n"
                                 "Returned non-zero code(%s)\n")
                         command
                         rets
                         exit-code)))
        (! `("Output:\n%s" ,(if (s-blank-str? rets)
                                "<blank>"
                              rets)))
        rets))))

(defmacro checkpoint (&rest body)
  "Evaluate BODY and restore `env-fp' if an error
has occurred. Return BODY evaluation value.
NOTE: We can extend `checkpoint'. For example remove
all newly installed packages in case of error."
  (declare (indent 0))
  `(let ((etcenv-bkp
          (with-temp-buffer
            (insert-file-contents env-fp)
            (buffer-string)))
         (err t))
     (unwind-protect
         (prog1 (progn ,@body)
           (setq err nil))
       (when err
         (with-temp-file env-fp
           (insert etcenv-bkp))))))

(defmacro with-build-dir (spec &rest body)
  "Create PATH folder, set is as `default-directory',
bind VAR to PATH and evaluate BODY into return value.
Then remove the folder and set `default-directory' to
PATH parent folder.
Remove folder if BODY throws error.

\(fn (VAR PATH) BODY...)"
  (declare (indent 1))
  `(let ((,(car spec) ,(cadr spec)))
     (mkdir ,(car spec) t)
     (cd ,(car spec))
     (unwind-protect (progn ,@body)
       (cd (file-name-directory
            (directory-file-name
             ,(car spec))))
       (delete-directory ,(car spec) t))))

(defmacro with-installed (packages &rest body)
  "Makes sure that the packages from PACKAGES list are installed
during BODY evaluation and return its value.
Remove newly installed packages if BODY throws error."
  (declare (indent 1))
  `(let ((pkgs (mapcar 'symbol-name '(,@packages)))
         (installed nil))
     (dolist (pkg
              (split-string
               (bash-command-to-string
                (format
                 "bash sdisinstalled %s"
                 (mapconcat 'identity
                            pkgs
                            " ")))))
       (setq installed (cons pkg installed)))
     (dolist (pkg installed)
       (setq pkgs (remove pkg pkgs)))
     (let ((new-pkgs (mapconcat 'identity pkgs " ")))
       (if (not (s-blank-str? new-pkgs))
           (progn
             (bash-command-to-string
              (format "apt-get update && apt-get install %s" new-pkgs))
             (unwind-protect (progn ,@body)
               (progn (bash-command-to-string
                       (format "apt-get purge %s" new-pkgs))
                      (cleanup))))
         ,@body))))

(defun get-config (config-varible)
  "Return used Spacemacs layer CONFIG-VARIABLE value."
  (cdr (assoc config-varible used-layers-configs)))

(defun expose-all-glob-envs ()
  "Get values and names of all variables specified in `env-fp' file and
for each create $VARIABLE symbol and bind(as constant) it
to the variable's value. Also export them."
  (with-temp-buffer
    (insert-file-contents env-fp)
    (goto-char (point-max))
    (while (re-search-backward "^\\(.+\\)=\"\\(.+\\)\"$"
                               nil
                               t)
      (eval `(defconst ,(intern (concat "$" (match-string 1)))
               ,(match-string 2)))
      (setenv (match-string 1) (match-string 2)))))

(defconst dotfile-fp (format "%s/.spacemacs" (let ((silent t ))
                                               (get-glob-env "UHOME")))
  "Path to .spacemacs")

(expose-all-glob-envs)
