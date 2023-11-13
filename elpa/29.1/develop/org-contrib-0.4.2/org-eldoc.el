;;; org-eldoc.el --- display org header and src block info using eldoc -*- lexical-binding: t; -*-

;; Copyright (c) 2014-2021 Free Software Foundation, Inc.

;; Author: Łukasz Gruner <lukasz@gruner.lu>
;; Maintainer: Łukasz Gruner <lukasz@gruner.lu>
;; Version: 6
;; Package-Requires: ((org "8"))
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Created: 25/05/2014
;; Keywords: eldoc, outline, breadcrumb, org, babel, minibuffer

;; This file is not part of Emacs.

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

;;; Changelog:

;; As of 01/11/14 switching license to GPL3 to allow submission to org-mode.
;; 08/11/14 switch code to automatically define eldoc-documentation-function, but don't autostart eldoc-mode.

;;; Code:

(require 'org)
(require 'ob-core)
(require 'eldoc)

(declare-function org-element-at-point "org-element" ())
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))

(defgroup org-eldoc nil "" :group 'org)

(defcustom org-eldoc-breadcrumb-separator "/"
  "Breadcrumb separator."
  :group 'org-eldoc
  :type 'string)

(defcustom org-eldoc-test-buffer-name " *Org-eldoc test buffer*"
  "Name of the buffer used while testing for mode-local variable values."
  :group 'org-eldoc
  :type 'string)

(eldoc-add-command 'org-self-insert-command)

(defun org-eldoc-get-breadcrumb ()
  "Return breadcrumb if on a headline or nil."
  (let ((case-fold-search t) cur)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at org-complex-heading-regexp)
          (setq cur (match-string 4))
          (org-format-outline-path
           (append (org-get-outline-path) (list cur))
           (frame-width) "" org-eldoc-breadcrumb-separator))))))

(defun org-eldoc-get-src-header ()
  "Returns lang and list of header properties if on src definition line and nil otherwise."
  (let ((case-fold-search t) info lang hdr-args)
    (save-excursion
      (beginning-of-line)
      (save-match-data
        (when (looking-at "^[ \t]*#\\+\\(begin\\|end\\)_src")
          (setq info (org-babel-get-src-block-info 'light)
                lang (propertize (or (nth 0 info) "no lang") 'face 'font-lock-string-face)
                hdr-args (nth 2 info))
          (concat
           lang
           ": "
           (mapconcat
            (lambda (elem)
              (when-let* ((val (and (cdr elem)
                                    (format "%s" (cdr elem))))
                          (_ (not (string-empty-p val))))
                (concat
                 (propertize (symbol-name (car elem)) 'face 'org-list-dt)
                 " "
                 (propertize val 'face 'org-verbatim)
                 " ")))
            hdr-args " ")))))))

(defun org-eldoc-get-src-lang ()
  "Return value of lang for the current block if in block body and nil otherwise."
  (let ((element (save-match-data (org-element-at-point))))
    (and (eq (org-element-type element) 'src-block)
	 (>= (line-beginning-position)
	     (org-element-property :post-affiliated element))
	 (<=
	  (line-end-position)
	  (org-with-wide-buffer
	   (goto-char (org-element-property :end element))
	   (skip-chars-backward " \t\n")
	   (line-end-position)))
	 (org-element-property :language element))))

(defvar org-eldoc-local-functions-cache (make-hash-table :size 40 :test 'equal)
  "Cache of major-mode's eldoc-documentation-functions,
 used by \\[org-eldoc-get-mode-local-documentation-function].")

(defun org-eldoc-get-mode-local-documentation-function (lang)
  "Check if LANG-mode sets eldoc-documentation-function and return its value."
  (let ((cached-func (gethash lang org-eldoc-local-functions-cache 'empty))
        (mode-func (org-src-get-lang-mode lang))
        doc-func)
    (if (eq 'empty cached-func)
        (when (fboundp mode-func)
	  (with-temp-buffer
	    (funcall mode-func)
	    (setq doc-func (if (boundp 'eldoc-documentation-functions)
			       (let ((doc-funs eldoc-documentation-functions))
				 (lambda (callback)
				   (let ((eldoc-documentation-functions doc-funs))
				     (run-hook-with-args-until-success
				      'eldoc-documentation-functions
				      callback))))
			     (and eldoc-documentation-function
				  (symbol-value 'eldoc-documentation-function))))
	    (puthash lang doc-func org-eldoc-local-functions-cache))
          doc-func)
      cached-func)))

(declare-function c-eldoc-print-current-symbol-info "c-eldoc" ())
(declare-function css-eldoc-function "css-eldoc" ())
(declare-function php-eldoc-function "php-eldoc" ())
(declare-function go-eldoc--documentation-function "go-eldoc" ())

(defun org-eldoc-documentation-function (&rest args)
  "Return breadcrumbs when on a headline, args for src block header-line,
  calls other documentation functions depending on lang when inside src body."
  (or
   (org-eldoc-get-breadcrumb)
   (org-eldoc-get-src-header)
   (let ((lang (org-eldoc-get-src-lang)))
     (cond
      ((string= lang "org")       ;Prevent inf-loop for Org src blocks
       nil)
      ((or
        (string= lang "emacs-lisp")
        (string= lang "elisp"))
       (cond ((and (boundp 'eldoc-documentation-functions) ; Emacs>=28
                   (fboundp 'elisp-eldoc-var-docstring)
                   (fboundp 'elisp-eldoc-funcall))
              (let ((eldoc-documentation-functions
                     '(elisp-eldoc-var-docstring elisp-eldoc-funcall)))
                (eldoc-print-current-symbol-info)))
             ((fboundp 'elisp-eldoc-documentation-function)
              (elisp-eldoc-documentation-function))
             (t            ; Emacs<25
              (let (eldoc-documentation-function)
                (eldoc-print-current-symbol-info)))))
      ((or
        (string= lang "c") ;; https://github.com/nflath/c-eldoc
        (string= lang "C"))
       (when (require 'c-eldoc nil t)
         (c-eldoc-print-current-symbol-info)))
      ;; https://github.com/zenozeng/css-eldoc
      ((string= lang "css") (when (require 'css-eldoc nil t)
                              (css-eldoc-function)))
      ;; https://github.com/zenozeng/php-eldoc
      ((string= lang "php") (when (require 'php-eldoc nil t)
                              (php-eldoc-function)))
      ((or
        (string= lang "go")
        (string= lang "golang"))
       (when (require 'go-eldoc nil t)
         (go-eldoc--documentation-function)))
      (t
       (let ((doc-fun (org-eldoc-get-mode-local-documentation-function lang))
             (callback (car args)))
         (when (functionp doc-fun)
           (if (functionp callback)
               (funcall doc-fun callback)
             (funcall doc-fun)))))))))

;;;###autoload
(defun org-eldoc-load ()
  "Set up org-eldoc documentation function."
  (interactive)
  ;; This approach is taken from python.el.
  (with-no-warnings
    (cond
     ((null eldoc-documentation-function) ; Emacs<25
      (setq-local eldoc-documentation-function
		  #'org-eldoc-documentation-function))
     ((boundp 'eldoc-documentation-functions) ; Emacs>=28
      (add-hook 'eldoc-documentation-functions
		#'org-eldoc-documentation-function nil t))
     (t
      (add-function :before-until (local 'eldoc-documentation-function)
		    #'org-eldoc-documentation-function)))))

;;;###autoload
(add-hook 'org-mode-hook #'org-eldoc-load)

(provide 'org-eldoc)

;; -*- coding: utf-8-emacs; -*-

;;; org-eldoc.el ends here
