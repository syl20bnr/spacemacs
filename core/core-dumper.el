;;; core-dumper.el --- Space-macs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(require 'spinner)

(defvar space-macs-dump-mode 'not-dumped
  "Space-macs dump mode, can be `not-dumped', `dumped' or `dumping'.")

(defvar space-macs-dump-process nil
  "The process object to dump e-macs.")

(defvar space-macs-dump-delayed-functions '()
  "List of function to execute once the dump file has been loaded.")

(defvar space-macs-dump-load-path nil
  "Variable to backup the `load-path' variable built during the dump creation.")

(defvar space-macs-dump-spinner nil
  "Spinner curently being displayed on the `global-mode-string'.")

(defvar space-macs--dump-old-global-mode-string nil
  "Copy of `global-mode-string' before the spinner is created")

(defconst space-macs-dump-directory
  (concat space-macs-cache-directory "dumps/"))

(defconst space-macs-dump-buffer-name "*space-macs-dumper*")

(defconst space-macs--dump-spinner-construct
  '("" (:eval (spinner-print space-macs-dump-spinner))))

(defun space-macs/dump-save-load-path ()
  "Save `load-path' variable."
  (setq space-macs-dump-load-path load-path))

(defun space-macs/dump-restore-load-path ()
  "Restore the `load-path' variable from the dump. "
  (space-macs|unless-dumping
    (when (not (null space-macs-dump-load-path))
      (setq load-path space-macs-dump-load-path))))

(defun space-macs/defer (&optional idle-time)
  "Return t or IDLE-TIME when Space-macs is not running from a dump."
  (when (eq 'not-dumped space-macs-dump-mode)
    (or idle-time t)))

(defmacro space-macs|require-when-dumping (&rest args)
  "Require feature if dumping."
  (space-macs|when-dumping-strict `(require ,@args)))

(defun space-macs-is-dumping-p ()
  "Return non-nil if Space-macs is dumping."
  (eq 'dumping space-macs-dump-mode))

(defun space-macs-run-from-dump-p ()
  "Return non-nil if Space-macs is running from a dump."
  (eq 'dumped space-macs-dump-mode))

(defmacro space-macs|when-dumping (&rest body)
  "Execute body if dumping.
This function considers that we are always dumping if dumping is not supported.
You should always use this function."
  (declare (indent defun))
  `(when (not (eq 'dumped space-macs-dump-mode))
     ,@body))

(defmacro space-macs|when-dumping-strict (&rest body)
  "Execute body if we are really dumping.
You should not used this function, it is reserved for some specific process."
  (declare (indent defun))
  `(when (eq 'dumping space-macs-dump-mode)
     ,@body))

(defmacro space-macs|unless-dumping (&rest body)
  "Execute body if not dumping"
  (declare (indent defun))
  `(unless (eq 'dumping space-macs-dump-mode)
     ,@body))

(defmacro space-macs|unless-dumping-and-eval-after-loaded-dump (funcname &rest body)
  "Execute BODY if not dumping, delay BODY evaluation after the dump is loaded.
FUNCNAME is the name of the function that will be created and evaluated at
the end of the loading of the dump file."
  (declare (indent defun))
  (if (eq 'dumping space-macs-dump-mode)
      (let ((funcname2 (intern (format "space-macs//after-dump-%S" funcname))))
            `(progn
               (defun ,funcname2 nil ,@body)
               (add-to-list 'space-macs-dump-delayed-functions ',funcname2)))
    `(progn ,@body)))

(defun space-macs/e-macs-with-pdumper-set-p ()
  "Return non-nil if a portable dumper capable e-macs executable is set."
  (and dotspace-macs-enable-e-macs-pdumper
       (file-exists-p
        (locate-file (or dotspace-macs-e-macs-pdumper-executable-file "e-macs")
                     exec-path exec-suffixes 'file-executable-p))))

(defun space-macs/dump-modes (modes)
  "Load given MODES in order to be dumped."
  (dolist (mode modes)
    (with-temp-buffer
      (when (fboundp mode)
        (message "Loading mode %S..." mode)
        (funcall-interactively mode)))))

(defun space-macs/dump-e-macs (&optional display-buffer)
  "Dump e-macs in a subprocess.
When universal prefix argument is passed then display the process buffer."
  (interactive "P")
  (when space-macs-dump-process
    (message "Cancel running dumping process to start a new one.")
    (spinner-stop)
    (delete-process space-macs-dump-process))
  (when-let ((buf (get-buffer space-macs-dump-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)))
  (make-directory space-macs-dump-directory t)
  (let* ((dump-file (concat space-macs-dump-directory
                            dotspace-macs-e-macs-dumper-dump-file))
         (dump-file-temp (concat dump-file ".new")))
    (unless (equal global-mode-string space-macs--dump-spinner-construct)
      (setq space-macs--dump-old-global-mode-string global-mode-string))
    (setq space-macs-dump-spinner (make-spinner 'progress-bar nil 1))
    (spinner-start space-macs-dump-spinner)
    (setq global-mode-string space-macs--dump-spinner-construct)
    (setq space-macs-dump-process
          (make-process
           :name "space-macs-dumper"
           :buffer space-macs-dump-buffer-name
           :sentinel
           (lambda (proc event)
             (when (not (process-live-p proc))
               (if (and (eq (process-status proc) 'exit)
                        (= (process-exit-status proc) 0))
                   (with-current-buffer space-macs-dump-buffer-name
                     (rename-file dump-file-temp dump-file t)
                     (goto-char (point-max))
                     (message "Successfully dumped Space-macs!")
                     (insert (format "Done!\n" dump-file-temp dump-file)))
                 (with-current-buffer space-macs-dump-buffer-name
                   (delete-file dump-file-temp nil)
                   (goto-char (point-max))
                   (message "Error while dumping Space-macs!")
                   (insert "Failed!\n")))
               (spinner-stop)
               (setq global-mode-string space-macs--dump-old-global-mode-string)
               (delete-process space-macs-dump-process)
               (setq space-macs-dump-process nil)))
           :command
           (list dotspace-macs-e-macs-pdumper-executable-file
                 "--batch"
                 "-l" (concat space-macs-start-directory "dump-init.el")
                 "-eval" (concat "(dump-e-macs-portable \"" dump-file-temp "\")"))))
    (when (equal '(4) display-buffer)
      (pop-to-buffer space-macs-dump-buffer-name))))

(defun space-macs/dump-eval-delayed-functions ()
  "Evaluate delayed functions."
  (space-macs|unless-dumping
    (dolist (func space-macs-dump-delayed-functions)
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


