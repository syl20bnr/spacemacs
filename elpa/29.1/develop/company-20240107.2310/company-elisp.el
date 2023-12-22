;;; company-elisp.el --- company-mode completion backend for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2009-2015, 2017, 2020  Free Software Foundation, Inc.

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
;; In newer versions of Emacs, company-capf is used instead.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'help-mode)
(require 'find-func)

(defgroup company-elisp nil
  "Completion backend for Emacs Lisp."
  :group 'company)

(defcustom company-elisp-detect-function-context t
  "If enabled, offer Lisp functions only in appropriate contexts.
Functions are offered for completion only after \\=' and \(."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom company-elisp-show-locals-first t
  "If enabled, locally bound variables and functions are displayed
first in the candidates list."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defun company-elisp--prefix ()
  (let ((prefix (company-grab-symbol)))
    (if prefix
        (when (if (company-in-string-or-comment)
                  (= (char-before (- (point) (length prefix))) ?`)
                (company-elisp--should-complete))
          prefix)
      'stop)))

(defun company-elisp--predicate (symbol)
  (or (boundp symbol)
      (fboundp symbol)
      (facep symbol)
      (featurep symbol)))

(defun company-elisp--fns-regexp (&rest names)
  (concat "\\_<\\(?:cl-\\)?" (regexp-opt names) "\\*?\\_>"))

(defvar company-elisp-parse-limit 30)
(defvar company-elisp-parse-depth 100)

(defvar company-elisp-defun-names '("defun" "defmacro" "defsubst"))

(defvar company-elisp-var-binding-regexp
  (apply #'company-elisp--fns-regexp "let" "lambda" "lexical-let"
         company-elisp-defun-names)
  "Regular expression matching head of a multiple variable bindings form.")

(defvar company-elisp-var-binding-regexp-1
  (company-elisp--fns-regexp "dolist" "dotimes")
  "Regular expression matching head of a form with one variable binding.")

(defvar company-elisp-fun-binding-regexp
  (company-elisp--fns-regexp "flet" "labels")
  "Regular expression matching head of a function bindings form.")

(defvar company-elisp-defuns-regexp
  (concat "([ \t\n]*"
          (apply #'company-elisp--fns-regexp company-elisp-defun-names)))

(defun company-elisp--should-complete ()
  (let ((start (point))
        (depth (car (syntax-ppss))))
    (not
     (when (> depth 0)
       (save-excursion
         (up-list (- depth))
         (when (looking-at company-elisp-defuns-regexp)
           (forward-char)
           (forward-sexp 1)
           (unless (= (point) start)
             (condition-case nil
                 (let ((args-end (scan-sexps (point) 2)))
                   (or (null args-end)
                       (> args-end start)))
               (scan-error
                t)))))))))

(defun company-elisp--locals (prefix functions-p)
  (let ((regexp (concat "[ \t\n]*\\(\\_<" (regexp-quote prefix)
                        "\\(?:\\sw\\|\\s_\\)*\\_>\\)"))
        (pos (point))
        res)
    (condition-case nil
        (save-excursion
          (dotimes (_ company-elisp-parse-depth)
            (up-list -1)
            (save-excursion
              (when (eq (char-after) ?\()
                (forward-char 1)
                (when (ignore-errors
                        (save-excursion (forward-list)
                                        (<= (point) pos)))
                  (skip-chars-forward " \t\n")
                  (cond
                   ((looking-at (if functions-p
                                    company-elisp-fun-binding-regexp
                                  company-elisp-var-binding-regexp))
                    (down-list 1)
                    (condition-case nil
                        (dotimes (_ company-elisp-parse-limit)
                          (save-excursion
                            (when (looking-at "[ \t\n]*(")
                              (down-list 1))
                            (when (looking-at regexp)
                              (cl-pushnew (match-string-no-properties 1) res)))
                          (forward-sexp))
                      (scan-error nil)))
                   ((unless functions-p
                      (looking-at company-elisp-var-binding-regexp-1))
                    (down-list 1)
                    (when (looking-at regexp)
                      (cl-pushnew (match-string-no-properties 1) res)))))))))
      (scan-error nil))
    res))

(defun company-elisp-candidates (prefix)
  (let* ((predicate (company-elisp--candidates-predicate prefix))
         (locals (company-elisp--locals prefix (eq predicate 'fboundp)))
         (globals (company-elisp--globals prefix predicate))
         (locals (cl-loop for local in locals
                          when (not (member local globals))
                          collect local)))
    (if company-elisp-show-locals-first
        (append (sort locals 'string<)
                (sort globals 'string<))
      (append locals globals))))

(defun company-elisp--globals (prefix predicate)
  (all-completions prefix obarray predicate))

(defun company-elisp--candidates-predicate (prefix)
  (let* ((completion-ignore-case nil)
         (beg (- (point) (length prefix)))
         (before (char-before beg)))
    (if (and company-elisp-detect-function-context
             (not (memq before '(?' ?`))))
        (if (and (eq before ?\()
                 (not
                  (save-excursion
                    (ignore-errors
                      (goto-char (1- beg))
                      (or (company-elisp--before-binding-varlist-p)
                          (progn
                            (up-list -1)
                            (company-elisp--before-binding-varlist-p)))))))
            'fboundp
          'boundp)
      'company-elisp--predicate)))

(defun company-elisp--before-binding-varlist-p ()
  (save-excursion
    (and (prog1 (search-backward "(")
           (forward-char 1))
         (looking-at company-elisp-var-binding-regexp))))

(defun company-elisp--doc (symbol)
  (let* ((symbol (intern symbol))
         (doc (if (fboundp symbol)
                  (documentation symbol t)
                (documentation-property symbol 'variable-documentation t))))
    (and (stringp doc)
         (string-match ".*$" doc)
         (match-string 0 doc))))

;;;###autoload
(defun company-elisp (command &optional arg &rest _ignored)
  "`company-mode' completion backend for Emacs Lisp."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-elisp))
    (prefix (and (or (derived-mode-p 'emacs-lisp-mode)
                     (derived-mode-p 'inferior-emacs-lisp-mode))
                 (company-elisp--prefix)))
    (candidates (company-elisp-candidates arg))
    (sorted company-elisp-show-locals-first)
    (meta (company-elisp--doc arg))
    (doc-buffer (let ((symbol (intern arg)))
                  (save-window-excursion
                    (ignore-errors
                      (cond
                       ((fboundp symbol) (describe-function symbol))
                       ((boundp symbol) (describe-variable symbol))
                       ((featurep symbol) (describe-package symbol))
                       ((facep symbol) (describe-face symbol))
                       (t (signal 'user-error nil)))
                      (help-buffer)))))
    (location (let ((sym (intern arg)))
                (cond
                 ((fboundp sym) (find-definition-noselect sym nil))
                 ((boundp sym) (find-definition-noselect sym 'defvar))
                 ((featurep sym) (cons (find-file-noselect (find-library-name
                                                            (symbol-name sym)))
                                       0))
                 ((facep sym) (find-definition-noselect sym 'defface)))))))

(provide 'company-elisp)
;;; company-elisp.el ends here
