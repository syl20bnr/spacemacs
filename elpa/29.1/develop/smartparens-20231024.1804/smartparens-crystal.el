;;; smartparens-crystal.el --- Additional configuration for Crystal based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Brantou

;; Author: Brantou <brantou89@gmail.com>
;; Maintainer: Brantou <brantou89@gmail.com>
;; Created: 5 March 2018
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; Based on smartparens-ruby.el

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

;; This file provides some additional configuration for Crystal based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-crystal)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)
(require 'smartparens-ruby)

(declare-function crystal-forward-sexp "crystal")
(declare-function crystal-backward-sexp "crystal")

(defun sp-crystal-forward-sexp ()
  "Wrapper for `crystal-forward-sexp'."
  (interactive)
  (crystal-forward-sexp))

(defun sp-crystal-backward-sexp ()
  "Wrapper for `crystal-backward-sexp'."
  (interactive)
  (crystal-backward-sexp))

(defun sp-crystal-inline-p (id)
  "Test if ID is inline."
  (save-excursion
    (when (looking-back id nil)
      (backward-word))
    (when (not (or (looking-back "^[[:blank:]]*" nil)
                   (looking-back "= *" nil)))
      (or (save-excursion
            (forward-symbol -1)
            (forward-symbol 1)
            (looking-at-p (concat " *" id)))
          (save-excursion
            ;; This does not seem to make emacs snapshot happy
            (ignore-errors
              (sp-crystal-backward-sexp)
              (sp-crystal-forward-sexp)
              (looking-at-p (concat "[^[:blank:]]* *" id))))))))

(defun sp-crystal-skip-inline-match-p (ms _mb _me)
  "If non-nil, skip inline match.
MS, MB, ME."
  (or (sp-ruby-method-p ms)
      (sp-crystal-inline-p ms)))

(defun sp-crystal-in-string-word-or-inline-p (id action context)
  "Test if point is inside string, word or inline.
ID, ACTION, CONTEXT."
  (or (sp-ruby-in-string-or-word-p id action context)
      (and (looking-back id nil)
           (sp-crystal-inline-p id))))

(add-to-list 'sp-navigate-skip-match
             '((crystal-mode) . sp--ruby-skip-match))

(dolist (mode '(crystal-mode))
  (add-to-list 'sp-sexp-suffix `(,mode syntax "")))

(sp-with-modes 'crystal-mode
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "{" "}"
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-post-handler)
                 :suffix "")

  (sp-local-pair "begin" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "def" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "class" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "struct" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "lib" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "fun" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "enum" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "union" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "module" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "macro" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-method-p
                 :suffix "")

  (sp-local-pair "if" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "unless" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "while" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "until" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-crystal-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-crystal-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "|" "|"
                 :when '(sp-ruby-should-insert-pipe-close)
                 :pre-handlers '(sp-ruby-pre-pipe-handler)
                 :suffix ""))

(provide 'smartparens-crystal)

;;; smartparens-crystal.el ends here
