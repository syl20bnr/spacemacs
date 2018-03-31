;;; core-dump.el --- Spacemacs Core File
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

(defun spacemacs/defer ()
  "Return non-nil if dump is not supported."
  (eq 'not-dumped spacemacs-dump-mode))

(defmacro spacemacs|require (&rest args)
  "Require feature if dumping."
  (spacemacs|when-dumping-strict `(require ,@args)))

(defun spacemacs-is-dumping-p ()
  "Return non-nil if Spacemacs is dumping."
  (eq 'dumping spacemacs-dump-mode))

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

(provide 'core-dump)
