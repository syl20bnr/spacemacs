;;; company-oddmuse.el --- company-mode completion backend for oddmuse-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2016, 2022, 2023  Free Software Foundation, Inc.

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
(eval-when-compile (require 'yaoddmuse nil t))
(eval-when-compile (require 'oddmuse nil t))

(defvar company-oddmuse-link-regexp
  "\\(\\<[A-Z][[:alnum:]]*\\>\\)\\|\\[\\[\\([[:alnum:]]+\\>\\|\\)")

(defun company-oddmuse-get-page-table ()
  (cl-case major-mode
    (yaoddmuse-mode (with-no-warnings
                      (yaoddmuse-get-pagename-table yaoddmuse-wikiname)))
    (oddmuse-mode (with-no-warnings
                    (oddmuse-make-completion-table oddmuse-wiki)))))

;;;###autoload
(defun company-oddmuse (command &optional arg &rest _ignored)
  "`company-mode' completion backend for `oddmuse-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-oddmuse))
    (prefix (let ((case-fold-search nil))
              (and (memq major-mode '(oddmuse-mode yaoddmuse-mode))
                   (looking-back company-oddmuse-link-regexp (line-beginning-position))
                   (or (match-string 1)
                       (match-string 2)))))
    (candidates (all-completions arg (company-oddmuse-get-page-table)))))

(provide 'company-oddmuse)
;;; company-oddmuse.el ends here
