;;; ledger-test.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t -*-

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

;;; Code:

(declare-function ledger-mode "ledger-mode") ; TODO: fix this cyclic dependency
(require 'org)
(require 'outline)

(defgroup ledger-test nil
  "Definitions for the Ledger testing framework"
  :group 'ledger)

(defcustom ledger-source-directory "~/ledger/"
  "Directory where the Ledger sources are located."
  :type 'directory
  :group 'ledger-test)

(defcustom ledger-test-binary "/Products/ledger/debug/ledger"
  "Directory where the Ledger debug binary is located."
  :type 'file
  :group 'ledger-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ledger-create-test ()
  "Create a regression test."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (save-excursion
      (let (text beg)
        (goto-char (point-min))
        (forward-line 1)
        (setq beg (point))
        (search-forward ":PROPERTIES:")
        (goto-char (line-beginning-position))
        (setq text (buffer-substring-no-properties beg (point)))
        (goto-char (point-min))
        (re-search-forward ":ID:\\s-+\\([^-]+\\)")
        (find-file-other-window
         (format "~/src/ledger/test/regress/%s.test" (match-string 1)))
        (sit-for 0)
        (insert text)
        (goto-char (point-min))
        (while (not (eobp))
          (goto-char (line-beginning-position))
          (delete-char 3)
          (forward-line 1))))))

(defun ledger-test-org-narrow-to-entry ()
  (outline-back-to-heading)
  (narrow-to-region (point) (progn (outline-next-heading) (point)))
  (goto-char (point-min)))

(defun ledger-test-create ()
  (interactive)
  (let ((uuid (org-entry-get (point) "ID")))
    (when (string-match "\\`\\([^-]+\\)-" uuid)
      (let ((prefix (match-string 1 uuid))
            input output)
        (save-restriction
          (ledger-test-org-narrow-to-entry)
          (goto-char (point-min))
          (while (re-search-forward "#\\+begin_src ledger" nil t)
            (goto-char (match-end 0))
            (forward-line 1)
            (let ((beg (point)))
              (re-search-forward "#\\+end_src")
              (setq input
                    (concat (or input "")
                            (buffer-substring beg (match-beginning 0))))))
          (goto-char (point-min))
          (while (re-search-forward ":OUTPUT:" nil t)
            (goto-char (match-end 0))
            (forward-line 1)
            (let ((beg (point)))
              (re-search-forward ":END:")
              (setq output
                    (concat (or output "")
                            (buffer-substring beg (match-beginning 0)))))))
        (find-file-other-window
         (expand-file-name (concat prefix ".test")
                           (expand-file-name "test/regress"
                                             ledger-source-directory)))
        (ledger-mode)
        (if input
            (insert input)
          (insert "2012-03-17 Payee\n")
          (insert "    Expenses:Food                $20\n")
          (insert "    Assets:Cash\n"))
        (insert "\ntest reg\n")
        (if output
            (insert output))
        (insert "end test\n")))))

(defun ledger-test-run ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^test \\(.+?\\)\\( ->.*\\)?$" nil t)
      (let ((command (expand-file-name ledger-test-binary))
            (args (format "--args-only --columns=80 --no-color -f \"%s\" %s"
                          buffer-file-name (match-string 1))))
        (setq args (replace-regexp-in-string "\\$sourcepath"
                                             ledger-source-directory args))
        (kill-new args)
        (message "Testing: ledger %s" args)
        (let ((prev-directory default-directory))
          (cd ledger-source-directory)
          (unwind-protect
              (async-shell-command (format "\"%s\" %s" command args))
            (cd prev-directory)))))))

(provide 'ledger-test)

;;; ledger-test.el ends here
