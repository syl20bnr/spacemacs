;;; company-etags.el --- company-mode completion backend for etags  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2015, 2018-2019, 2023  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'etags)

(defgroup company-etags nil
  "Completion backend for etags."
  :group 'company)

(defcustom company-etags-use-main-table-list t
  "Always search `tags-table-list' if set.
If this is disabled, `company-etags' will try to find the one table for each
buffer automatically."
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t)))

(defcustom company-etags-ignore-case nil
  "Non-nil to ignore case in completion candidates."
  :type 'boolean
  :package-version '(company . "0.7.3"))

(defcustom company-etags-everywhere nil
  "Non-nil to offer completions in comments and strings.
Set it to t or to a list of major modes."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Any supported mode" t)
                 (repeat :tag "Some major modes"
                         (symbol :tag "Major mode")))
  :package-version '(company . "0.9.0"))

(defvar company-etags-modes '(prog-mode c-mode objc-mode c++-mode java-mode
                              jde-mode pascal-mode perl-mode python-mode))

(defvar-local company-etags-buffer-table 'unknown)

(defun company-etags-find-table ()
  (let ((file (expand-file-name
               "TAGS"
               (locate-dominating-file (or buffer-file-name
                                           default-directory)
                                       "TAGS"))))
    (when (and file (file-regular-p file))
      (list file))))

(defun company-etags-buffer-table ()
  (or (and company-etags-use-main-table-list tags-table-list)
      (if (eq company-etags-buffer-table 'unknown)
          (setq company-etags-buffer-table (company-etags-find-table))
        company-etags-buffer-table)))

(defun company-etags--candidates (prefix)
  (let ((tags-table-list (company-etags-buffer-table))
        (tags-file-name tags-file-name)
        (completion-ignore-case company-etags-ignore-case))
    (and (or tags-file-name tags-table-list)
         (fboundp 'tags-completion-table)
         (save-excursion
           (visit-tags-table-buffer)
           (all-completions prefix (tags-completion-table))))))

;;;###autoload
(defun company-etags (command &optional arg &rest _ignored)
  "`company-mode' completion backend for etags."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-etags))
    (prefix (and (cl-some #'derived-mode-p company-etags-modes)
                 (or (eq t company-etags-everywhere)
                     (cl-some #'derived-mode-p company-etags-everywhere)
                     (not (company-in-string-or-comment)))
                 (company-etags-buffer-table)
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-etags--candidates arg))
    (location (let ((tags-table-list (company-etags-buffer-table)))
                (when (fboundp 'find-tag-noselect)
                  (save-excursion
                    (let ((buffer (find-tag-noselect arg)))
                      (cons buffer (with-current-buffer buffer (point))))))))
    (ignore-case company-etags-ignore-case)))

(provide 'company-etags)
;;; company-etags.el ends here
