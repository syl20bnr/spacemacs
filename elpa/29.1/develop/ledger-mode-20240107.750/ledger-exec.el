;;; ledger-exec.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.


;;; Commentary:
;; Code for executing ledger synchronously.

;;; Code:

(declare-function ledger-master-file "ledger-report" ())

(defconst ledger-version-needed "3.0.0"
  "The version of ledger executable needed for interactive features.")

(defvar ledger-works nil
  "Non-nil if the ledger binary can support `ledger-mode' interactive features.")

(defgroup ledger-exec nil
  "Interface to the Ledger command-line accounting program."
  :group 'ledger)

(defcustom ledger-mode-should-check-version t
  "Should Ledger-mode verify that the executable is working?"
  :type 'boolean
  :group 'ledger-exec)

(defcustom ledger-binary-path "ledger"
  "Path to the ledger executable."
  :type 'file
  :risky t
  :group 'ledger-exec)

(defun ledger-exec-handle-error (ledger-errfile)
  "Deal with ledger errors contained in LEDGER-ERRFILE."
  (with-current-buffer (get-buffer-create "*Ledger Error*")
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (insert-file-contents ledger-errfile))
    (view-mode)
    (setq buffer-read-only t)
    (current-buffer)))

(defun ledger-exec-success-p (exit-code ledger-output-buffer)
  "Return non-nil if EXIT-CODE and LEDGER-OUTPUT-BUFFER indicate success."
  (and (zerop exit-code)
       (with-current-buffer ledger-output-buffer
         (goto-char (point-min))
         (not (and (> (buffer-size) 1) (looking-at (regexp-quote "While")))))))

(defun ledger-exec-ledger (input-buffer &optional output-buffer &rest args)
  "Run Ledger using INPUT-BUFFER.
Optionally capture output in OUTPUT-BUFFER, and pass ARGS on the
command line.  Returns OUTPUT-BUFFER if ledger succeeded,
otherwise the error output is displayed and an error is raised."
  (unless (and ledger-binary-path
               (or (and (file-exists-p ledger-binary-path)
                        (file-executable-p ledger-binary-path))
                   (executable-find ledger-binary-path)))
    (error "`ledger-binary-path' (value: %s) is not executable" ledger-binary-path))
  (let ((buf (or input-buffer (find-file-noselect (ledger-master-file))))
        (outbuf (or output-buffer
                    (generate-new-buffer " *ledger-tmp*")))
        (errfile (make-temp-file "ledger-errors")))
    (unwind-protect
        (with-current-buffer buf
          (let ((exit-code
                 (let ((coding-system-for-write 'utf-8)
                       (coding-system-for-read 'utf-8))
                   (apply #'call-process-region
                          (append (list (point-min) (point-max)
                                        ledger-binary-path nil (list outbuf errfile) nil "-f" "-")
                                  (list "--date-format" ledger-default-date-format)
                                  args)))))
            (if (ledger-exec-success-p exit-code outbuf)
                outbuf
              (display-buffer (ledger-exec-handle-error errfile))
              (error "Ledger execution failed"))))
      (delete-file errfile))))

(defun ledger-version-greater-p (needed)
  "Verify the ledger binary version is at least NEEDED."
  (let ((version-strings '()))
    (with-temp-buffer
      (when (ledger-exec-ledger (current-buffer) (current-buffer) "--version")
        (goto-char (point-min))
        (delete-horizontal-space)
        (setq version-strings (split-string
                               (buffer-substring-no-properties (point)
                                                               (point-max))))
        (if (and (string-match (regexp-quote "Ledger") (car version-strings))
                 (or (string= needed (cadr version-strings))
                     (string< needed (cadr version-strings))))
            t ;; success
          nil))))) ;;failure

(defun ledger-check-version ()
  "Verify that ledger works and is modern enough."
  (interactive)
  (if ledger-mode-should-check-version
      (if (setq ledger-works (ledger-version-greater-p ledger-version-needed))
          (message "Good Ledger Version")
        (message "Bad Ledger Version"))))

(provide 'ledger-exec)

;;; ledger-exec.el ends here
