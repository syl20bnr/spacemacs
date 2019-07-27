;;; core-dumper.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs-dump-mode 'not-dumped
  "Spacemacs dump mode, can be `not-dumped', `dumped' or `dumping'.")

(defvar spacemacs-dump-process nil
  "The process object to dump Emacs.")

(defvar spacemacs-dump-delayed-functions '()
  "List of function to execute once the dump file has been loaded.")

(defvar spacemacs-dump-load-path nil
  "Variable to backup the `load-path' variable built during the dump creation.")

(defconst spacemacs-dump-directory
  (concat spacemacs-cache-directory "dumps/"))

(defconst spacemacs-dump-buffer-name "*spacemacs-dumper*")

(defun spacemacs/dump-save-load-path ()
  "Save `load-path' variable."
  (setq spacemacs-dump-load-path load-path))

(defun spacemacs/dump-restore-load-path ()
  "Restore the `load-path' variable from the dump. "
  (spacemacs|unless-dumping
    (when (not (null spacemacs-dump-load-path))
      (setq load-path spacemacs-dump-load-path))))

(defun spacemacs/defer (&optional idle-time)
  "Return t or IDLE-TIME when Spacemacs is not running from a dump."
  (when (eq 'not-dumped spacemacs-dump-mode)
    (or idle-time t)))

(defmacro spacemacs|require (&rest args)
  "Require feature if dumping."
  (spacemacs|when-dumping-strict `(require ,@args)))

(defun spacemacs-is-dumping-p ()
  "Return non-nil if Spacemacs is dumping."
  (eq 'dumping spacemacs-dump-mode))

(defun spacemacs-run-from-dump-p ()
  "Return non-nil if Spacemacs is running from a dump."
  (eq 'dumped spacemacs-dump-mode))

(defmacro spacemacs|when-dumping (&rest body)
  "Execute body if dumping.
This function considers that we are always dumping if dumping is not supported.
You should always use this function."
  (declare (indent defun))
  `(when (not (eq 'dumped spacemacs-dump-mode))
     ,@body))

(defmacro spacemacs|when-dumping-strict (&rest body)
  "Execute body if we are really dumping.
You should not used this function, it is reserved for some specific process."
  (declare (indent defun))
  `(when (eq 'dumping spacemacs-dump-mode)
     ,@body))

(defmacro spacemacs|unless-dumping (&rest body)
  "Execute body if not dumping"
  (declare (indent defun))
  `(unless (eq 'dumping spacemacs-dump-mode)
     ,@body))

(defmacro spacemacs|unless-dumping-and-eval-after-loaded-dump (funcname &rest body)
  "Execute BODY if not dumping, delay BODY evaluation after the dump is loaded.
FUNCNAME is the name of the function that will be created and evaluated at
the end of the loading of the dump file."
  (declare (indent defun))
  (if (eq 'dumping spacemacs-dump-mode)
      (let ((funcname2 (intern (format "spacemacs//after-dump-%S" funcname))))
            `(progn
               (defun ,funcname2 nil ,@body)
               (add-to-list 'spacemacs-dump-delayed-functions ',funcname2)))
    `(progn ,@body)))

(defun spacemacs/emacs-with-pdumper-set-p ()
  "Return non-nil if a portable dumper capable emacs executable is set."
  (and dotspacemacs-enable-emacs-pdumper
       (file-exists-p
        (locate-file (or dotspacemacs-emacs-pdumper-executable-file "emacs")
                     exec-path exec-suffixes 'file-executable-p))))

(defun spacemacs/dump-modes (modes)
  "Load given MODES in order to be dumped."
  (dolist (mode modes)
    (with-temp-buffer
      (when (fboundp mode)
        (message "Loading mode %S..." mode)
        (funcall-interactively mode)))))

(defun spacemacs/dump-emacs ()
  "Dump emacs in a subprocess."
  (interactive)
  (when spacemacs-dump-process
    (message "Cancel running dumping process to start a new one.")
    (delete-process spacemacs-dump-process)
    (with-current-buffer spacemacs-dump-buffer-name
      (erase-buffer)))
  (make-directory spacemacs-dump-directory t)
  (setq spacemacs-dump-process
        (make-process
         :name "spacemacs-dumper"
         :buffer spacemacs-dump-buffer-name
         :command
         (list dotspacemacs-emacs-pdumper-executable-file
               "--batch"
               "-l" "~/.emacs.d/dump-init.el"
               "-eval" (concat "(dump-emacs-portable \""
                               (concat spacemacs-dump-directory
                                       dotspacemacs-emacs-dumper-dump-file)
                               "\")")))))

(defun spacemacs/dump-eval-delayed-functions ()
  "Evaluate delayed functions."
  (spacemacs|unless-dumping
    (dolist (func spacemacs-dump-delayed-functions)
      (funcall func))))

;; ;; Brute-force load all .el files in ELPA packages
;; (dolist (d (directory-files package-user-dir t nil 'nosort))
;;   (unless (or (string-equal ".." (substring d -2))
;;               (string-equal "." (substring d -1))
;;               (not (file-directory-p d)))
;;     (message "%s" d)
;;     (dolist (f (directory-files d t "\\.el$" 'nosort))
;;       (unless (string-match-p ".*pkg\\.el$" f)
;;         (message "%s" f)
;;         (ignore-errors (load f t))))))

(provide 'core-dumper)
