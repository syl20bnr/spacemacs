;;; paradox-execute.el --- executing package transactions -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Prefix: paradox
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:
;; 
;; Functions related to executing package-menu transactions.
;; Everything that happens when you hit `x' is in here.


;;; Code:
(require 'cl-lib)
(require 'seq)

(require 'package)
(require 'paradox-core)
(require 'paradox-github)

(defgroup paradox-execute nil
  "Paradox Packages Menu configurations."
  :prefix "paradox-"
  :package-version '(paradox . "2.0")
  :group 'paradox)

(defvar paradox--current-filter)

;;; Customization Variables
(defcustom paradox-execute-asynchronously 'ask
  "Whether the install/delete/upgrade should be asynchronous.
Possible values are:
  t, which means always;
  nil, which means never;
  ask, which means ask each time."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "Ask each time" ask))
  :package-version '(paradox . "2.0")
  :group 'paradox-execute)

(defcustom paradox-async-display-buffer-function #'display-buffer
  "Function used to display *Paradox Report* buffer after asynchronous upgrade.
Set this to nil to avoid displaying the buffer. Or set this to a
function like `display-buffer' or `pop-to-buffer'.

This is only used if `paradox-menu-execute' was given a non-nil
NOQUERY argument. Otherwise, only a message is displayed."
  :type '(choice (const :tag "Don't display the buffer" nil)
                 function)
  :package-version '(paradox . "2.0")
  :group 'paradox-execute)


;;; Execution Hook
(defvar paradox-after-execute-functions nil
  "List of functions run after performing package transactions.
These are run after a set of installation, deletion, or upgrades
has been performed. Each function in this hook must take a single
argument. An associative list of the form

    ((SYMBOL . DATA) (SYMBOL . DATA) ...)

This list contains the following entries, describing what
occurred during the execution:

  SYMBOL      DATA
  `installed' List of installed packages.
  `deleted'   List of deleted packages.
  `activated' List of activated packages.
  `error'     List of errors.
  `async'     Non-nil if transaction was performed asynchronously.
  `noquery'   The NOQUERY argument given to `paradox-menu-execute'.")
(put 'risky-local-variable-p 'paradox-after-execute-functions t)
(mapc (lambda (x) (add-hook 'paradox-after-execute-functions x t))
      '(paradox--activate-if-asynchronous
        paradox--refresh-package-buffer
        paradox--report-buffer-print
        paradox--report-buffer-display-if-noquery
        paradox--report-message
        ))

(defun paradox--refresh-package-buffer (_)
  "Refresh the *Packages* buffer, if it exists."
  (let ((buf (get-buffer "*Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (revert-buffer)))))

(defun paradox--activate-if-asynchronous (alist)
  "Activate packages after an asynchronous operation.
Argument ALIST describes the operation."
  (let-alist alist
    (when .async
      (dolist (pkg .activated)
        (if (fboundp 'package--list-loaded-files)
            (package-activate-1 pkg 'reload)
          (package-activate-1 pkg))))))

(defun paradox--print-package-list (list)
  "Print LIST at point."
  (let* ((width (apply #'max
                  (mapcar (lambda (x) (string-width (symbol-name (package-desc-name x))))
                          list)))
         (tabulated-list-format
          `[("Package" ,(1+ width) nil)
            ("Version" 0 nil)])
         (tabulated-list-padding 2))
    (mapc
     (lambda (p) (tabulated-list-print-entry
             p
             `[,(symbol-name (package-desc-name p))
               ,(package-version-join (package-desc-version p))]))
     list)))

(defun paradox--report-buffer-print (alist)
  "Print a transaction report in *Package Report* buffer.
Possibly display the buffer or message the user depending on the
situation.
Argument ALIST describes the operation."
  (let-alist alist
    (let ((buf (get-buffer-create "*Paradox Report*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (goto-char (point-max))
        ;; TODO: Write our own mode for this.
        (special-mode)
        (insert "\n\n")
        (save-excursion
          (insert (format-time-string "Package transaction finished. %c\n"))
          (when .error
            (insert "Errors:\n  ")
            (dolist (it .error)
              (princ it (current-buffer))
              (insert "\n"))
            (insert "\n\n"))
          (when .installed
            (insert  "Installed:\n")
            (paradox--print-package-list .installed)
            (insert "\n"))
          (when .deleted
            (insert  "Deleted:\n")
            (paradox--print-package-list .deleted)
            (insert "\n")))))))

(defun paradox--report-buffer-display-if-noquery (alist)
  "Display report buffer if `paradox-execute' was called with a NOQUERY prefix.
ALIST describes the transaction.
`paradox-async-display-buffer-function' is used if transaction
was asynchronous. Otherwise, `pop-to-buffer' is used."
  (let-alist alist
    ;; The user has never seen the packages in this transaction. So
    ;; we display them in a buffer.
    (when (or .noquery .error)
      (let ((buf (get-buffer "*Paradox Report*")))
        (when (buffer-live-p buf)
          (cond
           ;; If we're async, the user might be doing something else, so
           ;; we don't steal focus.
           ((and .async paradox-async-display-buffer-function)
            (funcall paradox-async-display-buffer-function buf))
           ;; If we're not async, just go ahead and pop.
           ((or (not .async)
                ;; If there's an error, display the buffer even if
                ;; `paradox-async-display-buffer-function' is nil.
                .error)
            (pop-to-buffer buf))))))))

(defun paradox--report-message (alist)
  "Message the user about the executed transaction.
ALIST describes the transaction."
  (let-alist alist
    (message "%s%s"
      (paradox--format-message nil .installed .deleted)
      (if (memq 'paradox--report-buffer-print paradox-after-execute-functions)
          " See the buffer *Paradox Report* for more details." ""))
    (when .errors
      (message "Errors encountered during the operation: %S\n%s"
        .errors
        (if (memq 'paradox--report-buffer-print paradox-after-execute-functions)
            " See the buffer *Paradox Report* for more details." "")))))


;;; Execution
(defun paradox-menu-execute (&optional noquery)
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed.

Afterwards, if `paradox-automatically-star' is t, automatically
star new packages, and unstar removed packages.  Upgraded packages
aren't changed.

Synchronicity of the actions depends on
`paradox-execute-asynchronously'.  Optional argument NOQUERY
non-nil means do not ask the user to confirm.  If asynchronous,
never ask anyway."
  (interactive "P")
  (unless (derived-mode-p 'paradox-menu-mode)
    (error "The current buffer is not in Paradox Menu mode"))
  (when (and (stringp paradox-github-token)
             (eq paradox-automatically-star 'unconfigured))
    (customize-save-variable
     'paradox-automatically-star
     (y-or-n-p "When you install new packages would you like them to be automatically starred?
\(They will be unstarred when you delete them) ")))
  (when (and (stringp paradox--current-filter)
             (string-match "Upgradable" paradox--current-filter))
    (setq tabulated-list-sort-key '("Status" . nil))
    (setq paradox--current-filter nil))
  (paradox--menu-execute-1 noquery))

(defmacro paradox--perform-package-transaction (install delete)
  "Install all packages from INSTALL and delete those from DELETE.
Return an alist with properties listing installed,
deleted, and activated packages, and errors."
  `(let (activated installed deleted errored)
     (advice-add #'package-activate-1 :after
                 (lambda (pkg &rest _)
                   (ignore-errors (push pkg activated)))
                 '((name . paradox--track-activated)))
     (condition-case err
         (progn
           (dolist (pkg ,install)
             ;; 2nd arg introduced in 25.
             (if (version<= "25" emacs-version)
                 (package-install pkg 'dont-select)
               (package-install pkg))
             (push pkg installed))
           (let ((delete-list ,delete))
             (dolist (pkg (if (fboundp 'package--sort-by-dependence)
                              (package--sort-by-dependence delete-list)
                            delete-list))
               (condition-case err
                   (progn (package-delete pkg)
                          (push pkg deleted))
                 (error (push err errored))))))
       (error (push err errored)))
     (advice-remove #'package-activate-1 'paradox--track-activated)
     (list (cons 'installed (nreverse installed))
           (cons 'deleted   (nreverse deleted))
           (cons 'activated (nreverse activated))
           (cons 'error     (nreverse errored)))))

(defvar paradox--current-filter)

(declare-function async-inject-variables "async")
(defun paradox--menu-execute-1 (&optional noquery)
  "Implementation used by `paradox-menu-execute'.
NOQUERY, if non-nil, means to execute without prompting the
user."
  (let ((before-alist (paradox--repo-alist))
        install-list delete-list)
    (save-excursion
      (goto-char (point-min))
      (let ((p (point))
            (inhibit-read-only t))
        (while (not (eobp))
          (let ((c (char-after)))
            (if (eq c ?\s)
                (forward-line 1)
              (push (tabulated-list-get-id)
                    (pcase c
                      (`?D delete-list)
                      (`?I install-list)))
              (delete-region p (point))
              (forward-line 1)
              (setq p (point)))))
        (when (or delete-list install-list)
          (delete-region p (point))
          (ignore-errors
            (set-window-start (selected-window) (point-min))))))
    (if (not (or delete-list install-list))
        (message "No operations specified.")
      ;; Confirm with the user.
      (when (or noquery
                (y-or-n-p (paradox--format-message 'question install-list delete-list)))
        ;; On Emacs 25, update the selected packages list.
        (when (fboundp 'package--update-selected-packages)
          (let-alist (package-menu--partition-transaction install-list delete-list)
            (package--update-selected-packages .install .delete)))
        ;; Background or foreground?
        (if (or (not install-list)
                (not (pcase paradox-execute-asynchronously
                       (`nil nil)
                       (`ask
                        (if noquery nil
                          (y-or-n-p "Execute in the background (see `paradox-execute-asynchronously')? ")))
                       (_ t))))
            ;; Synchronous execution
            (progn
              (let ((alist (paradox--perform-package-transaction install-list delete-list)))
                (run-hook-with-args 'paradox-after-execute-functions
                                    `((noquery . ,noquery) (async . nil) ,@alist)))
              (when (and (stringp paradox-github-token) paradox-automatically-star)
                (paradox--post-execute-star-unstar before-alist (paradox--repo-alist))))
          ;; Start spinning
          (paradox--start-spinner)
          
          ;; Async execution
          (unless (require 'async nil t)
            (error "For asynchronous execution please install the `async' package"))
          ;; We have to do this with eval, because `async-start' is a
          ;; macro and it might not have been defined at compile-time.
          (eval
           `(async-start
             (lambda ()
               (require 'package)
               ,(async-inject-variables "\\`package-")
               (setq package-menu-async nil)
               (dolist (elt package-alist)
                 (package-activate (car elt) 'force))
               (let ((alist ,(macroexpand
                              `(paradox--perform-package-transaction ',install-list ',delete-list))))
                 (list package-alist
                       (when (boundp 'package-selected-packages)
                         package-selected-packages)
                       package-archive-contents
                       ;; This is the alist that will be passed to the hook.
                       (cons '(noquery . ,noquery) (cons '(async . t) alist)))))
             (lambda (x)
               (setq package-alist (pop x)
                     package-selected-packages (pop x)
                     package-archive-contents (pop x))
               (when (spinner-p paradox--spinner)
                 (spinner-stop paradox--spinner)
                 (setq paradox--spinner nil))
               (setq paradox--executing nil)
               (run-hook-with-args 'paradox-after-execute-functions (pop x))
               (paradox--post-execute-star-unstar ',before-alist (paradox--repo-alist))))))))))


;;; Aux functions
(defun paradox--repo-alist ()
  "List of known repos."
  (delete-dups
   (remove nil
           (mapcar
            (lambda (it) (gethash it paradox--package-repo-list))
            package-alist))))

(defun paradox--format-message (question-p install-list delete-list)
  "Format a message regarding a transaction.
If QUESTION-P is non-nil, format a question suitable for
`y-or-n-p', otherwise format a report in the past sense.
INSTALL-LIST and DELETE-LIST are a list of packages about to be
installed and deleted, respectively."
  (concat
   (when install-list
     (let ((len (length install-list)))
       (format "Install%s %d package%s"
         (if question-p "" "ed")
         len
         (if (> len 1) "s" ""))))
   (when (and install-list (not delete-list))
     (if question-p "? " "."))
   (when (and install-list delete-list)
     ", and ")
   (when delete-list
     (let ((len (length delete-list)))
       (format "Delete%s %d package%s%s"
         (if question-p "" "d")
         len
         (if (> len 1) "s" "")
         (if question-p "? " "."))))))

(defun paradox--post-execute-star-unstar (before after)
  "Star repos in AFTER absent from BEFORE, unstar vice-versa."
  (let ((repos (hash-table-keys paradox--user-starred-repos)))
    (mapc #'paradox--star-repo
          (seq-difference (seq-difference after before) repos))
    (mapc #'paradox--unstar-repo
          (seq-intersection (seq-difference before after) repos))))

(provide 'paradox-execute)
;;; paradox-execute.el ends here
