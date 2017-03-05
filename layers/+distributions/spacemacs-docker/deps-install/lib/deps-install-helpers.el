;;; deps-install-helpers.el --- Spacemacs layers deps. install helpers File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl)
(require 'package)

(load (format (concat "%s/.emacs.d/"
                      "layers/+distributions/spacemacs-docker/"
                      "config.el")
              (getenv "UHOME"))
      nil
      t)

(defvar silent nil
  "If non-nil disable verbose command and `!' print logging.")


;; Don't do this if we are evaluating buffer.
(when load-file-name
  (package-initialize)

  (unless (file-directory-p spacemacs-docker-temp-deps-elpa-dir)
    (message "Getting dependencies for the scripts...")
    (mkdir spacemacs-docker-temp-deps-elpa-dir t)
    (let ((package-archives '(("melpa" . "https://melpa.org/packages/")))
          (package-user-dir spacemacs-docker-temp-deps-elpa-dir))
      (package-refresh-contents)
      (package-install 's)
      (package-install 'dash)
      (package-install 'f)))

  (let ((default-directory  spacemacs-docker-temp-deps-elpa-dir))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'dash)
(require 'f)
(require 's)

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
and in the `spacemacs-docker-env-fp' file (remove if VALUE nil). Create
constant $VARIABLE with value VALUE and return its symbol."
  (when variable
    (with-temp-file spacemacs-docker-env-fp
      (insert-file-contents spacemacs-docker-env-fp)
      (goto-char (point-min))
      (delete-matching-lines (format "^%s=.*" variable))
      (setenv variable value)
      (when value
        (goto-char (point-max))
        (insert (format "%s=\"%s\"\n" variable value)))
      (goto-char (point-min))))
  (! `("Set global variable \"%s\" to \"%s\"" ,variable ,value))
  (eval `(defconst ,(intern (concat "$" variable)) ,value)))

(defun get-glob-env (variable)
  "Get value of VARIABLE in `spacemacs-docker-env-fp' file and create $VARIABLE constant.
Return its value (nil if VARIABLE not present.)"
  (when variable
    (with-temp-buffer
      (insert-file-contents spacemacs-docker-env-fp)
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

(defun set-glob-envs (&rest var-val-alists)
  "Call `set-glob-env' for each (variable . value)
in VAR-VAL-ALISTS."
  (when var-val-alists
    (dolist (vva var-val-alists)
      (set-glob-env (car vva) (cdr vva)))))

(defun append-glob-var-val (var separator &rest vals)
  "Add VALS to the value of VAR variable in
 the `spacemacs-docker-env-fp' file end export it with `set-glob-env'.
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
  "Run COMMAND with bash and return its output.
NOTE: Throws error if COMMAND returned non zero code."
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
  `(let* ((pkgs (sort (remove-duplicates (mapcar 'symbol-name
                                                 '(,@packages)))
                      'string<))
          (pkgs-only-installed
           (let ((silent t))
             (split-string
              (bash-command-to-string
               ;; FIXME: Make it properly detect packages
               ;; removed with apt-get remove as absent.
               (format (mapconcat 'identity
                                  '("pkgs=(%s)"
                                    "for pkg in \"${pkgs[@]}\"; do"
                                    "  sudo dpkg -s $pkg >/dev/null 2>&1 && {"
                                    "  printf \"$pkg \""
                                    "  }"
                                    "done"
                                    "printf \"\\n\"")
                                  "\n")
                       (mapconcat 'identity
                                  pkgs
                                  " ")))))))
     ;; (setq installed (cons pkg installed)))
     (dolist (pkg pkgs-only-installed)
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

(defun expose-all-glob-envs ()
  "Get values and names of all variables specified in `spacemacs-docker-env-fp' file and
for each create $VARIABLE symbol and bind(as constant) it
to the variable's value. Also export them."
  (with-temp-buffer
    (insert-file-contents spacemacs-docker-env-fp)
    (goto-char (point-max))
    (while (re-search-backward "^\\(.+\\)=\"\\(.+\\)\"$"
                               nil
                               t)
      (eval `(defconst ,(intern (concat "$" (match-string 1)))
               ,(match-string 2)))
      (setenv (match-string 1) (match-string 2)))))

(setq silent (let ((silent t))(get-glob-env "SILENT_INSTALL")))

(defun dotfile-append-sexp-to-defun (regexp comment sexp)
  "Append COMMENT and SEXP to the end of the first
defun that matched REGEXP in the `spacemacs-docker-dotfile-fp'."
  (! (v "Appending:\n%s\n%s\nTo:\n%s\n"
        comment
        sexp
        regexp))
  (with-temp-file spacemacs-docker-dotfile-fp
    (insert-file-contents spacemacs-docker-dotfile-fp)
    (goto-char (point-min))
    (re-search-forward regexp nil nil 1)
    (end-of-defun)
    (goto-char (- (point) 2))
    (insert (format "\n  ;;%s\n  %s" comment sexp))))

(defalias 'append-to-user-init (apply-partially
                                'dotfile-append-sexp-to-defun
                                "\(defun dotspacemacs/user-init")
  "Append COMMENT and SEXP to the end of dotspacemacs/user-init

\(fn COMMENT SEXP)")

(defalias 'append-to-user-config (apply-partially
                                  'dotfile-append-sexp-to-defun
                                  "\(defun dotspacemacs/user-config")
  "Append COMMENT and SEXP to the end of dotspacemacs/user-config

\(fn COMMENT SEXP)")

(defun dotfile-has-symbol-p (symbol)
  "Return t if SYMBOL present in the `spacemacs-docker-dotfile-fp'
ignoring comments and docstrings."
  (let ((name (symbol-name symbol)))
    (with-temp-buffer
      (block nil
        (insert-file-contents spacemacs-docker-dotfile-fp)
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote name)
                                  nil
                                  t)
          (goto-char (- (point) 1))
          ;;          Not at string or a comment.
          (when (and (not (or(nth 3 (syntax-ppss))
                             (nth 4 (syntax-ppss))))
                     (string= (thing-at-point 'symbol
                                              t)
                              name))
            (return t)))))))

(defun get-config (config-varible)
  "Return used Spacemacs layer CONFIG-VARIABLE value."
  (cdr (assoc config-varible used-layers-configs)))

(defun layer-used-p (layer)
  "Return t if LAYER is used."
  (member-ignore-case (if (symbolp layer)
                          (symbol-name layer)
                        layer)
                      (mapcar 'symbol-name used-layers)))

(defun layer-installer-excluded-p (layer)
  "Return t if LAYER dependencies installer excluded in .spacemacs.
EXAMPLE: (pandoc :variables pandoc-spacemacs-docker-disable-deps-install t)"
  (get-config (intern
               (format "%s-spacemacs-docker-disable-deps-install"
                       (if (symbolp layer)
                           (symbol-name layer)
                         layer)))))

;; Don't do this if we are evaluating buffer.
(when load-file-name
  ;; Add common env variables to /etc/environment.
  (dolist (genv '("UNAME"
                  "GNAME"
                  "UHOME"
                  "UID"
                  "GID"
                  "WORKSPACE"
                  "SHELL"))
    (let ((silent t))(unless (get-glob-env genv)
                       (set-glob-env genv (getenv genv)))))

  ;; Generate loadable file with Spacemacs configs.
  (unless (file-readable-p spacemacs-docker-dump-layer-data-fp)
    (message "Generating %s file..."
             spacemacs-docker-dump-layer-data-fp)
    (let ((silent t)
          (uname (get-glob-env "UNAME")))
      ;; Make sure that the env user is configured.
      ($  "asEnvUser true"
          (l "chown $UNAME:$GNAME %s" spacemacs-docker-temp-deps-dir)
          (v (l "su-exec %s emacs -batch -u %s" uname uname)
             "-f spacemacs-docker//dump-layers-data"))))

  (load spacemacs-docker-dump-layer-data-fp nil t)

  (expose-all-glob-envs))
