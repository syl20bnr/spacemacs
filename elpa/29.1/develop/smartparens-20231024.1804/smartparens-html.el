;;; smartparens-html.el --- Additional configuration for HTML based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014, 2017-2018 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 14 Sep 2013
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for HTML based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-html)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; This file provides these interactive functions:

;; `sp-html-next-tag'     - Recommended binding: C-c C-f
;; `sp-html-previous-tag' - Recommended binding: C-c C-b
;;
;; (These two bindings are used for navigation by tags forward or
;;  backward, but `sp-forward-sexp' already does that.)

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defun sp-html-next-tag (arg)
  "Move point to the beginning of next SGML tag.

With ARG positive N > 1, move N tags forward.

With ARG raw prefix argument \\[universal-argument] move out of
the current tag and to the beginning of enclosing tag.

Note: this function is based on `sp-beginning-of-sexp' but
specialized to only work with SGML tags and to always move
forward."
  (interactive "P")
  (let ((sp-prefix-tag-object t))
    (if (sp--raw-argument-p arg)
        (sp-beginning-of-sexp arg)
      (sp-beginning-of-sexp (1+ (prefix-numeric-value arg))))))

(defun sp-html-previous-tag (arg)
  "Move point to the beginning of previous SGML tag.

With ARG positive N > 1, move N tags backward.

With ARG raw prefix argument \\[universal-argument] move out of
the current tag and to the beginning of enclosing tag.

Note: this function is based on `sp-beginning-of-sexp' but
specialized to only work with SGML tags and to always move
backward."
  (interactive "P")
  (let ((sp-prefix-tag-object t))
    (if (sp--raw-argument-p arg)
        (sp-beginning-of-sexp arg)
      (sp-beginning-of-sexp (1- (- (prefix-numeric-value arg)))))))

(defun sp-html-post-handler (&optional _id action _context)
  "Post-action hooks for `html-mode'.

ID is the tag being processed, ACTION is the action and CONTEXT
specifies if we are inside a string or code."
  (cl-case action
    (slurp-forward
     (save-excursion
       (let ((sp-prefix-pair-object t))
         (sp-backward-sexp))
       (-when-let (enc (sp-get-enclosing-sexp))
         (sp-get enc
           (goto-char :beg-in)
           (when (looking-at-p "[ \t]*$")
             (goto-char :end-in)
             (save-excursion
               (sp-backward-sexp)
               (forward-line -1)
               (when (sp-point-in-blank-line)
                 (delete-region (line-beginning-position) (1+ (line-end-position)))))
             (newline-and-indent))))))
    (slurp-backward
     (save-excursion
       (-when-let (enc (sp-get-enclosing-sexp))
         (sp-get enc
           (goto-char :end-in)
           (when (sp--looking-back-p "^[ \t]*")
             (save-excursion
               (goto-char :beg-in)
               (newline-and-indent)
               (sp-forward-sexp)
               (forward-line)
               (when (sp-point-in-blank-line)
                 (delete-region (line-beginning-position) (1+ (line-end-position))))))))))
    (barf-forward
     (save-excursion
       (let ((sp-prefix-pair-object t))
         (sp-backward-sexp))
       (-when-let (enc (sp-get-enclosing-sexp))
         (sp-get enc
           (goto-char :beg-in)
           (when (looking-at-p "[ \t]*$")
             (goto-char :end-in)
             (newline-and-indent)))))
     (save-excursion
       (sp-forward-sexp)
       (forward-line)
       (when (sp-point-in-blank-line)
         (delete-region (line-beginning-position) (1+ (line-end-position))))))
    (barf-backward
     (save-excursion
       (-when-let (enc (sp-get-enclosing-sexp))
         (sp-get enc
           (goto-char :end-in)
           (when (sp--looking-back-p "^[ \t]*")
             (goto-char :beg-in)
             (newline-and-indent)
             (sp-backward-up-sexp)
             (sp-backward-sexp)
             (forward-line -1)
             (when (sp-point-in-blank-line)
               (delete-region (line-beginning-position) (1+ (line-end-position)))))))))
    (beginning-of-sexp
     (when (looking-at-p "[ \t]*$")
       (sp-next-sexp)))
    (end-of-sexp
     (when (sp--looking-back-p "^[ \t]*" nil t)
       (sp-previous-sexp)))))

(sp-with-modes sp--html-modes
  (sp-local-pair "<" ">")
  (sp-local-tag  "<" "<_>" "</_>" :transform 'sp-match-sgml-tags :post-handlers '(sp-html-post-handler)))

(--each sp--html-modes
  (add-to-list 'sp-navigate-consider-sgml-tags it))

(--each '(web-mode)
  (add-to-list 'sp-sexp-suffix (list it 'regexp "")))

(provide 'smartparens-html)

;;; smartparens-html.el ends here
