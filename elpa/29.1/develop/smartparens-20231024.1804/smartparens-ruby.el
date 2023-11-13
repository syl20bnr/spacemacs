;;; smartparens-ruby.el --- Additional configuration for Ruby based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014, 2016-2018, 2020, 2023 Jean-Louis Giordano, Matus Goljer

;; Author: Jean-Louis Giordano <jean-louis@jawaninja.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 16 June 2013
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

;; This file provides some additional configuration for Ruby based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-ruby)
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

(declare-function enh-ruby-forward-sexp "ruby")
(declare-function ruby-forward-sexp "ruby")
(declare-function enh-ruby-backward-sexp "ruby")
(declare-function ruby-backward-sexp "ruby")

(defun sp-ruby-forward-sexp ()
  "Wrapper for `ruby-forward-sexp' based on `enh-ruby-mode'."
  (interactive)
  (if (boundp 'enh-ruby-forward-sexp)
      (enh-ruby-forward-sexp)
    (ruby-forward-sexp)))

(defun sp-ruby-backward-sexp ()
  "Wrapper for `ruby-backward-sexp' based on `enh-ruby-mode'."
  (interactive)
  (if (boundp 'enh-ruby-backward-sexp)
      (enh-ruby-backward-sexp)
    (ruby-backward-sexp)))

(defun sp-ruby-maybe-one-space ()
  "Turn whitespace around point to just one space."
  (while (looking-back " " nil) (backward-char))
  (when (or (looking-at-p " ")
            (looking-at-p "}")
            (looking-back "{" nil)
            (and (looking-at-p "\\sw")
                 (looking-back ":" nil)))
    (save-excursion (just-one-space)))
  (when (and (not (looking-back "^.?" nil))
             (save-excursion
               (backward-char 2)
               (or (looking-at-p ".[^:] [.([,;]")
                   (looking-at-p ".. ::")
                   (looking-at-p ".[.@$] ")
                   (looking-at-p ":: "))))
    (delete-char 1)))

(defun sp-ruby-delete-indentation (&optional arg)
  "Better way of joining ruby lines.

ARG is how many indentation to delete."
  (delete-indentation arg)
  (sp-ruby-maybe-one-space))

(defun sp-ruby-block-post-handler (id action context)
  "Handler for ruby block-like insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (sp-ruby-post-handler id action context))

(defun sp-ruby-def-post-handler (id action context)
  "Handler for ruby def-like insertions.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (insert "x")
      (newline)
      (indent-according-to-mode))
    (delete-char 1))
  (sp-ruby-post-handler id action context))

(defun sp-ruby-post-handler (_id action _context)
  "Ruby post handler.
ID, ACTION, CONTEXT."
  (-let (((&plist :arg arg :enc enc) sp-handler-context))
    (when (equal action 'barf-backward)
      (goto-char (sp-get enc :beg))
      (sp-ruby-delete-indentation 1)
      (indent-according-to-mode)
      (save-excursion
        (sp-backward-sexp) ; move to begining of current sexp
        (sp-backward-sexp arg)
        (sp-ruby-maybe-one-space)))

    (when (equal action 'barf-forward)
      (sp-get enc
        (let ((beg-line (line-number-at-pos :beg-in)))
          (sp-forward-sexp arg)
          (sp-ruby-maybe-one-space)
          (when (not (= (line-number-at-pos) beg-line))
            (sp-ruby-delete-indentation -1))
          (indent-according-to-mode))))))

(defun sp-ruby-pre-handler (_id action _context)
  "Handler for ruby slurp and barf.
ID, ACTION, CONTEXT."
  (let ((enc (plist-get sp-handler-context :enc)))
    (sp-get enc
      (let ((beg-line (line-number-at-pos :beg-in))
            (end-line (line-number-at-pos :end-in)))

        (when (equal action 'slurp-backward)
          (save-excursion
            (sp-forward-sexp)
            (when (looking-at-p ";") (forward-char))
            (sp-ruby-maybe-one-space)
            (when (not (= (line-number-at-pos) end-line))
              (sp-ruby-delete-indentation -1)))
          (when (looking-at-p "::")
            (while (and (looking-back "\\sw" nil)
                        (--when-let (sp-get-symbol t)
                          (sp-get it (goto-char :beg-prf))))))
          (while (thing-at-point-looking-at "\\.[[:blank:]\n]*")
            (sp-backward-sexp))
          (when (looking-back "[@$:&?!]" nil)
            (backward-char)
            (when (looking-back "[@&:]" nil)
              (backward-char)))
          (just-one-space)
          (save-excursion
            (if (= (line-number-at-pos) end-line)
                (insert " ")
              (newline))))

        (when (equal action 'barf-backward)
          ;; Barf whole method chains
          (while (thing-at-point-looking-at "[(.:[][\n[:blank:]]*")
            (sp-forward-sexp))
          (if (looking-at-p " *$")
              (newline)
            (save-excursion (newline))))

        (when (equal action 'slurp-forward)
          (save-excursion
            (sp-backward-sexp)
            (when (looking-back "\." nil) (backward-char))
            (sp-ruby-maybe-one-space)
            (when (not (= (line-number-at-pos) beg-line))
              (if (thing-at-point-looking-at "\\.[[:blank:]\n]*")
                  (progn
                    (forward-symbol -1)
                    (sp-ruby-delete-indentation -1))
                (sp-ruby-delete-indentation))))
          (while (looking-at-p "::") (sp-forward-symbol))
          (when (looking-at-p "[?!;]") (forward-char))
          (if (= (line-number-at-pos) beg-line)
              (insert " ")
            (newline)))

        (when (equal action 'barf-forward)
          (when (looking-back "\\." nil) (backward-char))
          (when (looking-at-p "::")
            (while (and (looking-back "\\sw" nil)
                        (--when-let (sp-get-symbol t)
                          (sp-get it (goto-char :beg-prf))))))
          (if (= (line-number-at-pos) end-line)
              (insert " ")
            (if (looking-back "^[[:blank:]]*" nil)
                (save-excursion (newline))
              (newline))))))))

(defun sp-ruby-inline-p (id)
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
              (sp-ruby-backward-sexp)
              (sp-ruby-forward-sexp)
              (looking-at-p (concat "[^[:blank:]]* *" id))))))))

(defun sp-ruby-method-p (id)
  "Test if ID is a method."
  (save-excursion
    (when (looking-back id nil)
      (backward-word))
    (and (looking-at-p id)
         (or
          ;; fix for def_foo
          (looking-at-p (concat id "[_?!:]"))
          ;; fix for foo_def
          (looking-back "[_:@$.]" nil)
          ;; fix for def for; end
          (looking-back "def \\|class \\|module " nil)
          ;; Check if multiline method call
          ;; But beware of comments!
          (and (looking-back "\\.[[:blank:]\n]*" nil)
               (not (save-excursion
                      (search-backward ".")
                      (sp-point-in-comment))))))))

(defun sp-ruby-skip-inline-match-p (ms _mb _me)
  "If non-nil, skip inline match.
MS, MB, ME."
  (or (sp-ruby-method-p ms)
      (sp-ruby-inline-p ms)))

(defun sp-ruby-skip-method-p (ms _mb _me)
  "If non-nil, skip method.
MS, MB, ME."
  (sp-ruby-method-p ms))

(defun sp-ruby-in-string-or-word-p (id action context)
  "Test if point is inside string or word.
ID, ACTION, CONTEXT."
  (or (sp-in-string-p id action context)
      (and (looking-back id nil)
           (not (looking-back (sp--strict-regexp-quote id) nil)))
      (sp-ruby-method-p id)))

(defun sp-ruby-in-string-word-or-inline-p (id action context)
  "Test if point is inside string, word or inline.
ID, ACTION, CONTEXT."
  (or (sp-ruby-in-string-or-word-p id action context)
      (and (looking-back id nil)
           (sp-ruby-inline-p id))))

(defun sp-ruby-pre-pipe-handler (id action _context)
  "Ruby pipe handler.
ID, ACTION, CONTEXT."
  (when (equal action 'insert)
    (save-excursion
      (just-one-space))
    (save-excursion
      (search-backward id)
      (just-one-space))))

(defun sp-ruby-should-insert-pipe-close (id action _context)
  "Test whether to insert the closing pipe for a lambda-binding pipe pair.
ID, ACTION, CONTEXT"
  (if (eq action 'insert)
      (thing-at-point-looking-at
       (rx-to-string `(and (or "do" "{") (* space) ,id)))
    t))

(defun sp--ruby-skip-match (ms me mb)
  "Ruby skip match.
MS, ME, MB."
  (when (string= ms "end")
    (or (sp-in-string-p ms me mb)
        (sp-ruby-method-p "end"))))

(add-to-list 'sp-navigate-skip-match
             '((ruby-mode ruby-ts-mode enh-ruby-mode motion-mode) . sp--ruby-skip-match))

(dolist (mode '(ruby-mode ruby-ts-mode motion-mode))
  (add-to-list 'sp-sexp-suffix `(,mode syntax "")))

(sp-with-modes '(ruby-base-mode enh-ruby-mode motion-mode)
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

  (sp-local-pair "module" "end"
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

  (sp-local-pair "for" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p)

  (sp-local-pair "if" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "unless" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "while" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "until" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-word-or-inline-p sp-in-comment-p)
                 :actions '(insert navigate)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-def-post-handler)
                 :skip-match 'sp-ruby-skip-inline-match-p
                 :suffix "")

  (sp-local-pair "|" "|"
                 :when '(sp-ruby-should-insert-pipe-close)
                 :pre-handlers '(sp-ruby-pre-pipe-handler)
                 :suffix ""))

(provide 'smartparens-ruby)

;;; smartparens-ruby.el ends here
