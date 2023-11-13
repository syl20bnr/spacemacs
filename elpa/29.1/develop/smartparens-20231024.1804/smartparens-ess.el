;;; smartparens-ess.el --- Smartparens Extension for Emacs Speaks Statistics  -*- lexical-binding: t; -*-

;; Copyright (c) 2016-2018, 2020-2021 Bernhard Pröll, Matus Goljer

;; Author: Bernhard Pröll
;; Maintainer: Bernhard Pröll
;; URL: https://github.com/Fuco1/smartparens
;; Created: 2015-02-26
;; Version: 0.2
;; Keywords: abbrev convenience editing

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides some additional configuration for ESS.  To use
;; it, simply add:
;;
;; (require 'smartparens-ess)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;;; Code:

(require 'smartparens)
(require 'rx)

(defvar ess-roxy-str)

(declare-function ess-roxy-indent-on-newline "ess-roxy")


(dolist (mode '(ess-mode ess-r-mode inferior-ess-mode inferior-ess-r-mode))
  ;; avoid traveling commas when slurping
  ;; (|a, b), c ---> (|a, b, c)
  (add-to-list 'sp-sexp-suffix (list mode 'regexp ""))
  ;; `sp-sexp-prefix' for ESS
  (add-to-list 'sp-sexp-prefix
               (list mode 'regexp
                     (rx (zero-or-more (or word (syntax symbol)))))))

(defadvice sp-backward-kill-symbol (around sp-ess-backward-kill-symbol activate)
  "#821 For the purpose of killing words and symbols, we remove
the prefix resolution because it is not necessary.  We want to
treat function prefix as word or symbol to be deleted."
  (let ((sp-sexp-prefix nil))
    ad-do-it))

;; slurping follows Google's R style guide
;; see https://google.github.io/styleguide/Rguide.xml
(defun sp-ess-pre-handler (_id action _context)
  "Remove spaces before opening parenthesis in a function call.
Remove redundant space around commas.
ID, ACTION, CONTEXT."
  (when (equal action 'slurp-forward)
    (let ((sxp (sp-get-thing 'back)))
      (save-excursion
        (goto-char (sp-get sxp :beg-prf))
        ;; (|)   x ---> (x)
        (when (looking-back (rx (syntax open-parenthesis)
                                (one-or-more space)) nil)
          (cycle-spacing 0 nil 'single-shot))
        (cond
         ;; (|)if(cond) ---> (|if (cond))
         ((member (sp-get sxp :prefix) '("if" "for" "while"))
          (goto-char (sp-get sxp :beg))
          (cycle-spacing 1 nil 'single-shot))
         ;; (|)v [,2] <- if(x > 1) ---> (v[,2] <- if (x > 1))
         ((and
           (member (sp-get sxp :op) '("[" "("))
           (equal (sp-get sxp :prefix) "")
           (looking-back
            (rx (and (not-char "%" ",")
                     (not (syntax close-parenthesis)))
                (one-or-more space)) nil)
           (not (member
                 (save-excursion
                   (sp-backward-sexp)
                   (thing-at-point 'word 'noprop))
                 '("if" "for" "while"))))
          (cycle-spacing 0 nil 'single-shot))
         ;; (|[...])%in% ---> ([...] %in%|)
         ((or (looking-at "%") (looking-back "%" nil))
          (just-one-space))
         ;; (|)a , b,    c ---> (|a, b, c)
         ((looking-back
           (rx (zero-or-more space) "," (zero-or-more space))
           (line-beginning-position) 'greedy)
          (replace-match ", "))))))
  (when (equal action 'slurp-backward)
    (let ((sxp (sp-get-thing)))
      (save-excursion
        (goto-char (sp-get sxp :end))
        ;; x  (|) ---> (x)
        (when (looking-at (rx (one-or-more space)
                              (syntax close-parenthesis)))
          (cycle-spacing 0 nil 'single-shot))
        ;; if(cond){} (|) ---> (if (cond) {}|)
        (cond ((member (sp-get sxp :prefix) '("if" "for" "while"))
               (goto-char (sp-get sxp :beg))
               (cycle-spacing 1 nil 'single-shot))
              ;; for style reasons there should be a space before curly
              ;; brackets and binary operators
              ((and (member (sp-get sxp :op) '("{" "%"))
                    (not (looking-at (rx (syntax close-parenthesis)))))
               (cycle-spacing 1 nil 'single-shot))
              ;; v[2](|) ---> (v[2]|)
              ((and
                (not (member (thing-at-point 'word 'noprop)
                             '("if" "for" "while")))
                (looking-at
                 (rx (and (zero-or-more space)
                          (not-char "{")
                          (or (syntax close-parenthesis)
                              (char "(")
                              (char "["))))))
               (cycle-spacing 0 nil 'single-shot))
              ;; 1 , 2 (|) ---> (1, 2)
              ((looking-at
                (rx (zero-or-more space) "," (zero-or-more space)))
               (replace-match ", ")))))))

;; function(x) {|} ---> function(x) {\n|\n}
;; ##' \tabular{rrr}{|} --->
;; ##' \tabular{rrr}{
;; ##'   |
;; ##' }
(defun sp-ess-open-sexp-indent (&rest _args)
  "Open new brace or bracket with indentation.
ARGS."
  (if (and (fboundp 'ess-roxy-entry-p) (ess-roxy-entry-p))
      (progn
        (save-excursion (ess-roxy-indent-on-newline))
        (when (looking-back ess-roxy-str nil)
          (cycle-spacing 3 nil t)))
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(defun sp-ess-roxy-str-p (_id action _context)
  "Test if looking back at `ess-roxy-re'.
ID, ACTION, CONTEXT."
  (when (and (bound-and-true-p ess-roxy-re) (eq action 'insert))
    (sp--looking-back-p ess-roxy-re)))

(sp-with-modes 'ess-mode
  (sp-local-pair "{" nil
                 :pre-handlers '(sp-ess-pre-handler)
    ;; the more reasonable C-j interferes with default binding for
    ;; `ess-eval-line'
                 :post-handlers '((sp-ess-open-sexp-indent "M-j")))
  (sp-local-pair "(" nil
                 :pre-handlers '(sp-ess-pre-handler)
                 :post-handlers '((sp-ess-open-sexp-indent "M-j")))
  (sp-local-pair "[" nil
                 :pre-handlers '(sp-ess-pre-handler)
                 :post-handlers '((sp-ess-open-sexp-indent "M-j")))
  (sp-local-pair "'" nil
                 :unless '(sp-ess-roxy-str-p sp-in-comment-p sp-in-string-quotes-p)))

;;; roxygen2 markup
;; see https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html
(sp-with-modes 'ess-mode
  (sp-local-pair "\\strong{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\strong")
  (sp-local-pair "\\emph{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\emph")
  (sp-local-pair "\\code{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\code")
  (sp-local-pair "\\url{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\url")
  (sp-local-pair "\\link{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\link")
  (sp-local-pair "\\href{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\href"
                 :suffix "{[^}]*}")
  (sp-local-pair "\\email{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\email")
  (sp-local-pair "\\pkg{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\pkg")
  (sp-local-pair "\\item{" "}"
                 :when '(sp-in-comment-p)
                 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\item{")
  (sp-local-pair "\\enumerate{" "}"
                 :when '(sp-in-comment-p)
                 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\enumerate")
  (sp-local-pair "\\itemize{" "}"
                 :when '(sp-in-comment-p)
                 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\itemize")
  (sp-local-pair "\\describe{" "}"
                 :when '(sp-in-comment-p)
                 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :trigger "\\describe")
  (sp-local-pair "\\eqn{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\eqn")
  (sp-local-pair "\\deqn{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\deqn")
  (sp-local-pair "\\tabular{" "}"
                 :when '(sp-in-comment-p)
                 :trigger "\\tabular"
                 :post-handlers '((sp-ess-open-sexp-indent "M-j"))
                 :suffix "{[^}]*}"))

(sp-with-modes '(ess-r-mode inferior-ess-r-mode)
  (sp-local-pair "\\(" nil
                 :unless '(:add sp-in-code-p)))

(provide 'smartparens-ess)
;;; smartparens-ess ends here
