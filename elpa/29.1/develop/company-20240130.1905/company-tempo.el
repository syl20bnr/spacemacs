;;; company-tempo.el --- company-mode completion backend for tempo  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2016, 2023  Free Software Foundation, Inc.

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
(require 'tempo)

(defgroup company-tempo nil
  "Tempo completion backend."
  :group 'company)

(defcustom company-tempo-expand nil
  "Whether to expand a tempo tag after completion."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defsubst company-tempo-lookup (match)
  (cdr (assoc match (tempo-build-collection))))

(defun company-tempo-insert (match)
  "Replace MATCH with the expanded tempo template."
  (search-backward match)
  (goto-char (match-beginning 0))
  (replace-match "")
  (call-interactively (company-tempo-lookup match)))

(defsubst company-tempo-meta (match)
  (let ((templ (company-tempo-lookup match))
        doc)
    (and templ
         (setq doc (documentation templ t))
         (car (split-string doc "\n" t)))))

;;;###autoload
(defun company-tempo (command &optional arg &rest _ignored)
  "`company-mode' completion backend for tempo."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tempo))
    (prefix (or (car (tempo-find-match-string tempo-match-finder)) ""))
    (candidates (all-completions arg (tempo-build-collection)))
    (kind 'snippet)
    (meta (company-tempo-meta arg))
    (post-completion (when company-tempo-expand (company-tempo-insert arg)))
    (sorted t)))

(provide 'company-tempo)
;;; company-tempo.el ends here
