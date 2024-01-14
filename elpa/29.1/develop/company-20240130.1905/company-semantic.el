;;; company-semantic.el --- company-mode completion backend using Semantic  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2018, 2023  Free Software Foundation, Inc.

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
(require 'company-template)
(require 'cl-lib)

(defvar semantic-idle-summary-function)
(declare-function semantic-documentation-for-tag "semantic/doc" )
(declare-function semantic-analyze-current-context "semantic/analyze")
(declare-function semantic-analyze-possible-completions "semantic/complete")
(declare-function semantic-analyze-find-tags-by-prefix "semantic/analyze/fcn")
(declare-function semantic-tag-class "semantic/tag")
(declare-function semantic-tag-name "semantic/tag")
(declare-function semantic-tag-start "semantic/tag")
(declare-function semantic-tag-buffer "semantic/tag")
(declare-function semantic-active-p "semantic")
(declare-function semantic-format-tag-prototype "semantic/format")

(defgroup company-semantic nil
  "Completion backend using Semantic."
  :group 'company)

(defcustom company-semantic-metadata-function 'company-semantic-summary-and-doc
  "The function turning a semantic tag into doc information."
  :type 'function)

(defcustom company-semantic-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\"."
  :type 'boolean)

(defcustom company-semantic-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :package-version '(company . "0.9.0"))

(defvar company-semantic-modes '(c-mode c++-mode jde-mode java-mode))

(defvar-local company-semantic--current-tags nil
  "Tags for the current context.")

(defun company-semantic-documentation-for-tag (tag)
  (when (semantic-tag-buffer tag)
    ;; When TAG's buffer is unknown, the function below raises an error.
    (semantic-documentation-for-tag tag)))

(defun company-semantic-doc-or-summary (tag)
  (or (company-semantic-documentation-for-tag tag)
      (and (require 'semantic-idle nil t)
           (require 'semantic/idle nil t)
           (funcall semantic-idle-summary-function tag nil t))))

(defun company-semantic-summary-and-doc (tag)
  (let ((doc (company-semantic-documentation-for-tag tag))
        (summary (funcall semantic-idle-summary-function tag nil t)))
    (and (stringp doc)
         (string-match "\n*\\(.*\\)$" doc)
         (setq doc (match-string 1 doc)))
    (concat summary
            (when doc
                  (if (< (+ (length doc) (length summary) 4) (window-width))
                      " -- "
                    "\n"))
            doc)))

(defun company-semantic-doc-buffer (tag)
  (let ((doc (company-semantic-documentation-for-tag tag)))
    (when doc
      (company-doc-buffer
       (concat (funcall semantic-idle-summary-function tag nil t)
               "\n"
               doc)))))

(defsubst company-semantic-completions (prefix)
  (ignore-errors
    (let ((completion-ignore-case nil)
          (context (semantic-analyze-current-context)))
      (setq company-semantic--current-tags
            (semantic-analyze-possible-completions context 'no-unique))
      (all-completions prefix company-semantic--current-tags))))

(defun company-semantic-completions-raw (prefix)
  (setq company-semantic--current-tags nil)
  (dolist (tag (semantic-analyze-find-tags-by-prefix prefix))
    (unless (eq (semantic-tag-class tag) 'include)
      (push tag company-semantic--current-tags)))
  (delete "" (mapcar 'semantic-tag-name company-semantic--current-tags)))

(defun company-semantic-annotation (argument tags)
  (let* ((tag (assq argument tags))
         (kind (when tag (elt tag 1))))
    (cl-case kind
      (function (let* ((prototype (semantic-format-tag-prototype tag nil nil))
                       (par-pos (string-match "(" prototype)))
                  (when par-pos (substring prototype par-pos)))))))

(defun company-semantic--prefix ()
  (if company-semantic-begin-after-member-access
      (company-grab-symbol-cons "\\.\\|->\\|::" 2)
    (company-grab-symbol)))

;;;###autoload
(defun company-semantic (command &optional arg &rest _ignored)
  "`company-mode' completion backend using CEDET Semantic."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-semantic))
    (prefix (and (featurep 'semantic)
                 (semantic-active-p)
                 (memq major-mode company-semantic-modes)
                 (not (company-in-string-or-comment))
                 (or (company-semantic--prefix) 'stop)))
    (candidates (if (and (equal arg "")
                         (not (looking-back "->\\|\\.\\|::" (- (point) 2))))
                    (company-semantic-completions-raw arg)
                  (company-semantic-completions arg)))
    (meta (funcall company-semantic-metadata-function
                   (assoc arg company-semantic--current-tags)))
    (annotation (company-semantic-annotation arg
                                             company-semantic--current-tags))
    (doc-buffer (company-semantic-doc-buffer
                 (assoc arg company-semantic--current-tags)))
    ;; Because "" is an empty context and doesn't return local variables.
    (no-cache (equal arg ""))
    (duplicates t)
    (location (let ((tag (assoc arg company-semantic--current-tags)))
                (when (buffer-live-p (semantic-tag-buffer tag))
                  (cons (semantic-tag-buffer tag)
                        (semantic-tag-start tag)))))
    (post-completion (let ((anno (company-semantic-annotation
                                  arg company-semantic--current-tags)))
                       (when (and company-semantic-insert-arguments anno)
                         (insert anno)
                         (company-template-c-like-templatify (concat arg anno)))
                       ))))

(provide 'company-semantic)
;;; company-semantic.el ends here
