;;; magit-reflog.el --- Inspect ref history  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2023 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements support for looking at Git reflogs.

;;; Code:

(require 'magit-core)
(require 'magit-log)

;;; Options

(defcustom magit-reflog-limit 256
  "Maximal number of entries initially shown in reflog buffers.
The limit in the current buffer can be changed using \"+\"
and \"-\"."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type 'number)

(defcustom magit-reflog-margin
  (list (nth 0 magit-log-margin)
        (nth 1 magit-log-margin)
        'magit-log-margin-width nil
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-reflog-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.  Option `magit-log-margin-show-committer-date'
  controls which date is being displayed.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize #'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-reflog-mode))

;;; Faces

(defface magit-reflog-commit '((t :foreground "green"))
  "Face for commit commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-amend '((t :foreground "magenta"))
  "Face for amend commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-merge '((t :foreground "green"))
  "Face for merge, checkout and branch commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-checkout '((t :foreground "blue"))
  "Face for checkout commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-reset '((t :foreground "red"))
  "Face for reset commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-rebase '((t :foreground "magenta"))
  "Face for rebase commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-cherry-pick '((t :foreground "green"))
  "Face for cherry-pick commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-remote '((t :foreground "cyan"))
  "Face for pull and clone commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-other '((t :foreground "cyan"))
  "Face for other commands in reflogs."
  :group 'magit-faces)

;;; Commands

;;;###autoload
(defun magit-reflog-current ()
  "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead."
  (interactive)
  (magit-reflog-setup-buffer (or (magit-get-current-branch) "HEAD")))

;;;###autoload
(defun magit-reflog-other (ref)
  "Display the reflog of a branch or another ref."
  (interactive (list (magit-read-local-branch-or-ref "Show reflog for")))
  (magit-reflog-setup-buffer ref))

;;;###autoload
(defun magit-reflog-head ()
  "Display the `HEAD' reflog."
  (interactive)
  (magit-reflog-setup-buffer "HEAD"))

;;; Mode

(defvar-keymap magit-reflog-mode-map
  :doc "Keymap for `magit-reflog-mode'."
  :parent magit-log-mode-map
  "C-c C-n" #'undefined
  "L"       #'magit-margin-settings)

(define-derived-mode magit-reflog-mode magit-mode "Magit Reflog"
  "Mode for looking at Git reflog.

This mode is documented in info node `(magit)Reflog'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[magit-cherry-pick] to apply the commit at point.
Type \\[magit-reset] to reset `HEAD' to the commit at point.

\\{magit-reflog-mode-map}"
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer)
  (setq magit--imenu-item-types 'commit))

(defun magit-reflog-setup-buffer (ref)
  (require 'magit)
  (magit-setup-buffer #'magit-reflog-mode nil
    (magit-buffer-refname ref)
    (magit-buffer-log-args (list (format "-n%s" magit-reflog-limit)))))

(defun magit-reflog-refresh-buffer ()
  (magit-set-header-line-format (concat "Reflog for " magit-buffer-refname))
  (magit-insert-section (reflogbuf)
    (magit-git-wash (apply-partially #'magit-log-wash-log 'reflog)
      "reflog" "show" "--format=%h%x00%aN%x00%gd%x00%gs" "--date=raw"
      magit-buffer-log-args magit-buffer-refname "--")))

(cl-defmethod magit-buffer-value (&context (major-mode magit-reflog-mode))
  magit-buffer-refname)

(defvar magit-reflog-labels
  '(("commit"      . magit-reflog-commit)
    ("amend"       . magit-reflog-amend)
    ("merge"       . magit-reflog-merge)
    ("checkout"    . magit-reflog-checkout)
    ("branch"      . magit-reflog-checkout)
    ("reset"       . magit-reflog-reset)
    ("rebase"      . magit-reflog-rebase)
    ("rewritten"   . magit-reflog-rebase)
    ("cherry-pick" . magit-reflog-cherry-pick)
    ("initial"     . magit-reflog-commit)
    ("pull"        . magit-reflog-remote)
    ("clone"       . magit-reflog-remote)
    ("autosave"    . magit-reflog-commit)
    ("restart"     . magit-reflog-reset)))

(defun magit-reflog-format-subject (subject)
  (let* ((match (string-match magit-reflog-subject-re subject))
         (command (and match (match-string 1 subject)))
         (option  (and match (match-string 2 subject)))
         (type    (and match (match-string 3 subject)))
         (label (if (string= command "commit")
                    (or type command)
                  command))
         (text (if (string= command "commit")
                   label
                 (mapconcat #'identity
                            (delq nil (list command option type))
                            " "))))
    (format "%-16s "
            (magit--propertize-face
             text (or (cdr (assoc label magit-reflog-labels))
                      'magit-reflog-other)))))

;;; _
(provide 'magit-reflog)
;;; magit-reflog.el ends here
