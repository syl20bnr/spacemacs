;;; poly-org.el --- Polymode for org-mode -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2020 Vitalie Spinu
;; Version: 0.2.2
;; Package-Requires: ((emacs "25") (polymode "0.2.2"))
;; URL: https://github.com/polymode/poly-org
;; Keywords: languages, multi-modes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode)
(require 'org)
(require 'org-src)

(define-obsolete-variable-alias 'pm-host/org 'poly-org-hostmode "v0.2")
(define-obsolete-variable-alias 'pm-inner/org 'poly-org-innermode "v0.2")

(defun poly-org-mode-matcher ()
  (let ((case-fold-search t))
    (when (re-search-forward "#\\+begin_\\(src\\|example\\|export\\) +\\([^ \t\n]+\\)" (point-at-eol) t)
      (let ((lang (match-string-no-properties 2)))
        (or (cdr (assoc lang org-src-lang-modes))
            lang)))))

(defvar ess-local-process-name)
(defun poly-org-convey-src-block-params-to-inner-modes (_ this-buf)
  "Move src block parameters to innermode specific locals.
Used in :switch-buffer-functions slot."
  (cond
   ((derived-mode-p 'ess-mode)
    (with-current-buffer (pm-base-buffer)
      (let* ((params (nth 2 (org-babel-get-src-block-info t)))
             (session (cdr (assq :session params))))
        (when (and session (org-babel-comint-buffer-livep session))
          (let ((proc (buffer-local-value 'ess-local-process-name
                                          (get-buffer session))))
            (with-current-buffer this-buf
              (setq-local ess-local-process-name proc)))))))))

(define-hostmode poly-org-hostmode
  :mode 'org-mode
  :protect-syntax nil
  :protect-font-lock nil)

(define-auto-innermode poly-org-innermode
  :fallback-mode 'host
  :head-mode 'host
  :tail-mode 'host
  :head-matcher "^[ \t]*#\\+begin_\\(src\\|example\\|export\\) .*\n"
  :tail-matcher "^[ \t]*#\\+end_\\(src\\|example\\|export\\)"
  :mode-matcher #'poly-org-mode-matcher
  :head-adjust-face nil
  :switch-buffer-functions '(poly-org-convey-src-block-params-to-inner-modes)
  :body-indent-offset 'org-edit-src-content-indentation
  :indent-offset 'org-edit-src-content-indentation)

(define-innermode poly-org-latex-innermode nil
  "Innermode for matching latex fragments in `org-mode'"
  :mode 'latex-mode
  :body-indent-offset 'LaTeX-indent-level
  ;; has to be bol-anchored to avoid false-positive as in #35
  :head-matcher "^[ \t]*\\\\begin{.+}.*$"
  :tail-matcher "^[ \t]*\\\\end{.+}.*$"
  :head-mode 'host
  :tail-mode 'host)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode
  :hostmode 'poly-org-hostmode
  :innermodes '(poly-org-innermode poly-org-latex-innermode)
  (setq-local org-src-fontify-natively t)
  (setq-local polymode-move-these-minor-modes-from-old-buffer
              (append '(org-indent-mode)
                      polymode-move-these-minor-modes-from-old-buffer))
  (setq-local polymode-run-these-before-change-functions-in-other-buffers
              (append '(org-before-change-function
                        org-element--cache-before-change
                        org-table-remove-rectangle-highlight)
                      polymode-run-these-before-change-functions-in-other-buffers))
  (setq-local polymode-run-these-after-change-functions-in-other-buffers
              (append '(org-element--cache-after-change)
                      polymode-run-these-after-change-functions-in-other-buffers)))

 ;;;###autoload
(add-to-list 'auto-mode-alist '("\\.org\\'" . poly-org-mode))

(provide 'poly-org)
;;; poly-org.el ends here
