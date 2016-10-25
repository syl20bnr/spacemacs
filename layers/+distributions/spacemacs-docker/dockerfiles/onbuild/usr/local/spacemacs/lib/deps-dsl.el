;;; deps-dsl.el --- Spacemacs layers dependencies installation DSL File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(load (expand-file-name "dockerfile-dsl.el"
                        (file-name-directory (or load-file-name
                                                 (buffer-file-name))))
      nil
      t)

(setq silent (let ((silent t)) (not (get-glob-env "VERBOSE_INSTALL"))))

(let ((silent t))
  (defvar allow-rerun (get-glob-env "ALLOW_RERUN")
    "If non-nil allow repeated execution of a installer script.")
  (defvar allow-unused (get-glob-env "ALLOW_UNUSED")
    "If non-nil allow execution of dependency installation scripts
for unused layers."))

(defun add-glob-executed-installers (&rest installers)
  "Add INSTALLERS to the value of EXECUTED_INSTALLERS
variable in the `env-fp' file end export it with `setenv'"
  (apply 'append-glob-var-val "EXECUTED_INSTALLERS" ":" installers))

(defun dotfile-append-sexp-to-defun (regexp comment sexp)
  "Append COMMENT and SEXP to the end of the first
defun that matched REGEXP in the `dotfile-fp'."
  (with-temp-file dotfile-fp
    (insert-file-contents dotfile-fp)
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
  "Return t if SYMBOL present in the `dotfile-fp'
ignoring comments and docstrings."
  (let ((name (symbol-name symbol)))
    (with-temp-buffer
      (block nil
        (insert-file-contents dotfile-fp)
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

(defun executedp (installer)
  "Return t if INSTALLER was executed.
NOTE: Failed installations also count."
  (let ((ex-installers  (get-glob-env "EXECUTED_INSTALLERS" )))
    (when ex-installers
      (string-match-p (regexp-quote installer) ex-installers))))

(defun layer-used-p (layer)
  "Return t if LAYER is used."
  (member-ignore-case (symbol-name layer)
                      (mapcar 'symbol-name used-layers)))

(defun layer-installer-not-excluded-p (layer)
  "Return t if LAYER dependencies installer isn't
excluded in .spacemacs.
EXAMPLE: (pandoc :variables pandoc-disable-dependencies-installer t)"
  (not (get-config (intern
                    (format "%s-docker-spacemacs-disable-dependencies-installer"
                            (symbol-name layer))))))

(unless (file-readable-p dump-layer-data-fp)
  (message "Generating %s file..." dump-layer-data-fp)
  (let ((uname (get-glob-env "UNAME")))
    ($ (v (l "sudo su %s -c \"emacs -batch -u %s" uname uname)
          "-f spacemacs-docker//dump-layers-data\""))))

(with-eval-after-load load-file-name
  (load dump-layer-data-fp nil t)
  (let ((layer-name (-> load-file-name
                        (f-dirname)
                        (f-filename))))
    (unless (layer-installer-not-excluded-p
             (intern layer-name))
      (message "Dependencies installer for \"%s\" layer is excluded."
               layer-name)
      (kill-emacs 0))
    (when (and (let ((silent t))(executedp layer-name))
               (not allow-rerun))
      (message (concat "Dependencies installation script for \"%s\" "
                       "layer has already been executed. Aborting.")
               layer-name)
      (kill-emacs 0))
    (unless (or (member-ignore-case layer-name
                                    (mapcar 'symbol-name used-layers))
                allow-unused)
      (unless silent
        (message "Layer \"%s\" not used. Aborting."
                 layer-name))
      (kill-emacs 0))
    (message "Executing dependencies installation script for \"%s\" layer..."
             layer-name)
    (let ((silent t))(add-glob-executed-installers layer-name)))
  (cd (file-name-directory load-file-name))
  (advice-add 'command-error-default-function
              :before (lambda (data context signal)
                        (message "Layer dependencies installainton failed!"))))
