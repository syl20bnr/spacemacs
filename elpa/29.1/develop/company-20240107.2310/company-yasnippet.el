;;; company-yasnippet.el --- company-mode completion backend for Yasnippet  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015, 2020-2023  Free Software Foundation, Inc.

;; Author: Dmitry Gutov

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

(declare-function yas--table-hash "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas--template-content "yasnippet")
(declare-function yas--template-expand-env "yasnippet")
(declare-function yas--warning "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas--require-template-specific-condition-p "yasnippet")
(declare-function yas--template-can-expand-p "yasnippet")
(declare-function yas--template-condition "yasnippet")

(defvar company-yasnippet-annotation-fn
  (lambda (name)
    (concat
     (unless company-tooltip-align-annotations " -> ")
     name))
  "Function to format completion annotation.
It has to accept one argument: the snippet's name.")

(defun company-yasnippet--key-prefixes ()
  ;; Mostly copied from `yas--templates-for-key-at-point'.
  (defvar yas-key-syntaxes)
  (save-excursion
    (let ((original (point))
          (methods yas-key-syntaxes)
          prefixes
          method)
      (while methods
        (unless (eq method (car methods))
          (goto-char original))
        (setq method (car methods))
        (cond ((stringp method)
               (skip-syntax-backward method)
               (setq methods (cdr methods)))
              ((functionp method)
               (unless (eq (funcall method original)
                           'again)
                 (setq methods (cdr methods))))
              (t
               (setq methods (cdr methods))
               (yas--warning "Invalid element `%s' in `yas-key-syntaxes'" method)))
        (let ((prefix (buffer-substring-no-properties (point) original)))
          (unless (equal prefix (car prefixes))
            (push prefix prefixes))))
      (nreverse prefixes))))

(defun company-yasnippet--candidates (prefix)
  ;; Process the prefixes in reverse: unlike Yasnippet, we look for prefix
  ;; matches, so the longest prefix with any matches should be the most useful.
  (cl-loop with tables = (yas--get-snippet-tables)
           for key-prefix in (company-yasnippet--key-prefixes)
           ;; Only consider keys at least as long as the symbol at point.
           when (>= (length key-prefix) (length prefix))
           thereis (company-yasnippet--completions-for-prefix prefix
                                                              key-prefix
                                                              tables)))

(defun company-yasnippet--completions-for-prefix (prefix key-prefix tables)
  (cl-mapcan
   (lambda (table)
     (let ((keyhash (yas--table-hash table))
           (requirement (yas--require-template-specific-condition-p))
           res)
       (when keyhash
         (maphash
          (lambda (key value)
            (when (and (stringp key)
                       (string-prefix-p key-prefix key))
              (maphash
               (lambda (name template)
                 (when (yas--template-can-expand-p
                        (yas--template-condition template) requirement)
                   (push
                    (propertize key
                                'yas-annotation name
                                'yas-template template
                                'yas-prefix-offset (- (length key-prefix)
                                                      (length prefix)))
                    res)))
               value)))
          keyhash))
       res))
   tables))

(defun company-yasnippet--doc (arg)
  (let ((template (get-text-property 0 'yas-template arg))
        (mode major-mode)
        (file-name (buffer-file-name)))
    (defvar yas-prompt-functions)
    (with-current-buffer (company-doc-buffer)
      (let ((buffer-file-name file-name))
        (yas-minor-mode 1)
        (setq-local yas-prompt-functions '(yas-no-prompt))
        (condition-case error
            (yas-expand-snippet (yas--template-content template))
          (error
           (message "%s"  (error-message-string error))))
        (delay-mode-hooks
          (let ((inhibit-message t))
            (if (eq mode 'web-mode)
                (progn
                  (setq mode 'html-mode)
                  (funcall mode))
              (funcall mode)))
          (ignore-errors (font-lock-ensure))))
      (current-buffer))))

(defun company-yasnippet--prefix ()
  ;; We can avoid the prefix length manipulations after GH#426 is fixed.
  (let* ((prefix (company-grab-symbol))
         (tables (yas--get-snippet-tables))
         (key-prefixes (company-yasnippet--key-prefixes))
         key-prefix)
    (while (and key-prefixes
                (setq key-prefix (pop key-prefixes)))
      (when (company-yasnippet--completions-for-prefix
             prefix key-prefix tables)
        ;; Stop iteration.
        (setq key-prefixes nil)))
    (if (equal key-prefix prefix)
        prefix
      (cons prefix (length key-prefix)))))

;;;###autoload
(defun company-yasnippet (command &optional arg &rest _ignore)
  "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='js-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)
"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-yasnippet))
    (prefix
     (and (bound-and-true-p yas-minor-mode)
          (company-yasnippet--prefix)))
    (annotation
     (funcall company-yasnippet-annotation-fn
              (get-text-property 0 'yas-annotation arg)))
    (candidates (company-yasnippet--candidates arg))
    (doc-buffer (company-yasnippet--doc arg))
    (no-cache t)
    (kind 'snippet)
    (post-completion
     (let ((template (get-text-property 0 'yas-template arg))
           (prefix-offset (get-text-property 0 'yas-prefix-offset arg)))
       (yas-expand-snippet (yas--template-content template)
                           (- (point) (length arg) prefix-offset)
                           (point)
                           (yas--template-expand-env template))))))

(provide 'company-yasnippet)
;;; company-yasnippet.el ends here
