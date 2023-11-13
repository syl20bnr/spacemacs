;;; smartparens-elixir.el --- Configuration for Elixir.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018, 2020 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 15th January 2017
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'smartparens)

(--each '(elixir-mode elixir-ts-mode)
  (add-to-list 'sp-sexp-suffix (list it 'regexp "")))

(defvar sp-elixir-builtins
  (regexp-opt '("defmodule" "defmacro" "defmacrop" "def" "defp" "defimpl"
                "if" "unless" "case" "cond"
                "with" "for" "receive" "try" "quote")
              'words)
  "Regexp that matches opening delimiters for definitions.

Definitions require either comma followed by \"do:\" keyword
list, or \"do\" \"end\" block delimiters.")

(defun sp-elixir-skip-do-keyword-p-fun (bodyless-ms keywords)
  "Define a function that will test if any of keywords is part of definition.

BODYLESS-MS is an keyword that supports bodyless form, like
\"def\" or \"defp\".  KEYWORDS is additional regexp for keywords
to check in order to skip definition.

This line-based search terminates early if any of
`sp-elixir-builtins' were found."
  (lambda (ms _mb _me)
    (unless (or (equal "end" ms) (equal "do" ms))
      (or (string-match-p keywords (thing-at-point 'line t))
          (catch 'definition
            (save-excursion
              (while t
                (forward-line 1)
                (let ((line (string-trim-left (thing-at-point 'line t))))
                  (unless (string-match-p "\\s-*#" line) ;; skip full line comments
                    (cond ((eq (string-match-p "\\bend\\b" line) 0)
                           (throw 'definition nil))
                          ;; if BODYLESS-MS was supplied, means we're trying
                          ;; to match bodyless form
                          ((and bodyless-ms (eq (string-match-p bodyless-ms line) 0))
                           (throw 'definition t))
                          ;; Terminate the search if we find any of
                          ;; `sp-elixir-builtins' as we're usually
                          ;; searching for "end"
                          ((eq (string-match-p sp-elixir-builtins line) 0)
                           (throw 'definition nil))
                          ;; "do:" keyword means that there will be no
                          ;; "end" so we skip this definition
                          ((eq (string-match-p keywords line) 0)
                           (throw 'definition t))
                          ((eobp) (throw 'definition nil))))))))))))

;; Special functions for SKIP-MATCH parameter of `sp-pair'

(fset 'sp-elixir-skip-keyword-list-def-p (sp-elixir-skip-do-keyword-p-fun nil "\\bdo:"))
(fset 'sp-elixir-skip-bodyless-def-p (sp-elixir-skip-do-keyword-p-fun "\\bdef\\b" "\\bdo:"))
(fset 'sp-elixir-skip-bodyless-defp-p (sp-elixir-skip-do-keyword-p-fun "\\bdefp\\b" "\\bdo:"))
(fset 'sp-elixir-skip-for-in-defimpl-p (sp-elixir-skip-do-keyword-p-fun nil "\\b\\(defimpl\\b\\|do:\\)"))

(defun sp-elixir-skip-def-p (ms _mb _me)
  "Test if \"do\" is part of definition.

MS must be \"do\" keyword.

Definitions in Elixir can contain any of `sp-elixir-builtins'
followed with \"do\" keyword and closed with \"end\" keyword,
which may not be on the same line."
  (when (equal "do" ms)
    (save-excursion
      (catch 'definition
        (while t
          (let ((line (string-trim-left (thing-at-point 'line t))))
            (unless (string-match-p "\\s-*#" line)
              (cond ((eq (string-match-p sp-elixir-builtins line) 0)
                     (throw 'definition t))
                    ((eq (string-match-p "\\bend\\b" line) 0)
                     (throw 'definition nil))
                    ((bobp) (throw 'definition nil)))))
          (forward-line -1))))))

(defun sp-elixir-do-block-post-handler (_id action _context)
  "Insert \"do\" keyword and indent the new block.
ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (let ((m (make-marker)))
      (save-excursion
        (forward-word) ;; over the "end"
        (move-marker m (point)))
      (save-excursion (newline))
      (save-excursion (insert " do"))
      (indent-region (line-beginning-position) m)
      (move-marker m nil nil))))

(defun sp-elixir-empty-do-block-post-handler (_id action _context)
  "Insert empty \"do\" keyword and indent the new block.

This is used for receive-do-end expression.
ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (let ((m (make-marker)))
      (save-excursion
        (forward-word) ;; over the "end"
        (move-marker m (point)))
      (save-excursion
        (forward-line -1)
        (end-of-line)
        (insert " do"))
      (save-excursion (newline))
      (indent-region (line-beginning-position) m)
      (indent-according-to-mode)
      (move-marker m nil nil))))

(sp-with-modes '(elixir-mode elixir-ts-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :skip-match 'sp-elixir-skip-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "def" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-bodyless-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "defp" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-bodyless-defp-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "defmodule" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "defimpl" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '("| "))
  (sp-local-pair "if" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "for" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-for-in-defimpl-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "cond" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "with" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "unless" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "try" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :post-handlers '(sp-elixir-do-block-post-handler)
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :unless '(sp-in-comment-p sp-in-string-p))
  (sp-local-pair "receive" "end"
                 :when '(("RET" "<evil-ret>"))
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :post-handlers '(sp-elixir-empty-do-block-post-handler))
  (sp-local-pair "quote" "end"
                 :when '(("RET" "<evil-ret>"))
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :post-handlers '(sp-elixir-empty-do-block-post-handler))
  (sp-local-pair "defmacro" "end"
                 :when '(("RET" "<evil-ret>"))
                 :skip-match 'sp-elixir-skip-keyword-list-def-p
                 :post-handlers '(sp-elixir-empty-do-block-post-handler))
  )

(provide 'smartparens-elixir)
;;; smartparens-elixir.el ends here
