;;; evil-cleverparens-util.el --- Utility functions for evil-cleverparens
;;
;; Copyright (C) 2015 Olli Piepponen
;;
;; Author: Olli Piepponen <opieppo@gmail.com>
;; URL: https://github.com/emacs-evil/evil-cleverparens
;; Keywords: convenience, emulations
;; Version: 0.1.0
;; Package-Requires: ((evil "1.0") (paredit "1") (smartparens "1.6.1") (emacs "24.4") (dash "2.12.0"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;;; Commentary:

;; Helpers and utility functions for evil-cleverparens.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'evil)
(require 'paredit)
(require 'smartparens)
(require 'subr-x)


(defvar evil-cp-pair-list
  '(("(" . ")") ("[" . "]") ("{" . "}") ("\"" . "\""))
  "List of parentheses pairs recognized by evil-cleverparens.")

(defvar evil-cp-sp-pair-list
  (-filter (lambda (pair)
             (not (string-equal (car pair) "`")))
           sp-pair-list))

(defvar evil-cp--ws-regexp "[ \n\r\t]")

(defun evil-cp--looking-at-string-delimiter-p ()
  "Predicate for checking if the point is on a string delimiter."
  (and (looking-at (sp--get-stringlike-regexp))
       (not (paredit-in-string-escape-p))))

(defun evil-cp--looking-at-whitespace-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (looking-at-p evil-cp--ws-regexp)))

(defun evil-cp--looking-at-escape-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (looking-at-p (regexp-quote (or sp-escape-char "\\")))))

(defun evil-cp--looking-at-escaped-p (&optional pos)
  (or (evil-cp--looking-at-escape-p pos)
      (evil-cp--looking-at-escape-p (1- (or pos (point))))))

(defun evil-cp--pair-for (pair pairs)
  (cond
   ((not pairs)
    (message "Pair for %s not found." pair))
   ((string= pair (caar pairs))
    (car pairs))
   (t
    (evil-cp--pair-for pair (cdr pairs)))))

(defun evil-cp-pair-for (pair)
  (evil-cp--pair-for pair evil-cp-pair-list))

(defun evil-cp--get-opening-regexp ()
  (sp--strict-regexp-opt (--map (car it) evil-cp-pair-list)))

(defun evil-cp--get-closing-regexp ()
  (sp--strict-regexp-opt (--map (cdr it) evil-cp-pair-list)))

(defun evil-cp--looking-at-opening-p (&optional pos)
  "Predicate that returns true if point is looking at an opening
parentheses as defined by smartparens for the major mode in
question. Ignores parentheses inside strings."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (evil-cp--get-opening-regexp))
         (not (evil-cp--looking-at-escaped-p))
         (not (evil-cp--inside-string-p)))))

(defun evil-cp--looking-at-opening-anywhere-p (&optional pos)
  "Predicate that returns true if point is looking at an opening
parentheses as defined by smartparens for the major mode in
question. Includes parentheses inside strings."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (evil-cp--get-opening-regexp))
         (not (evil-cp--looking-at-escaped-p))
         (not (evil-cp--looking-at-string-closing-p)))))

(defun evil-cp--looking-at-closing-p (&optional pos)
  "Predicate that returns true if point is looking at an closing
paren as defined by smartparens for the major mode in
question. Ignores parentheses inside strings."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (evil-cp--get-closing-regexp))
         (not (evil-cp--looking-at-escaped-p))
         (not (evil-cp--inside-string-p)))))

(defun evil-cp--looking-at-paren-p (&optional pos)
  "Predicate that returns true if point is looking at a
parentheses as defined by smartparens for the major mode in
question. Ignores parentheses inside strings."
  (save-excursion
    (when pos (goto-char pos))
    (and (sp--looking-at-p (sp--get-allowed-regexp))
         (not (evil-cp--inside-string-p)))))

(defun evil-cp--looking-at-any-delimiter (&optional pos)
  "Predicate that returns true if point is on top of a
  parentheses or a string delimiter as defined by smartparens for
  the major mode in question."
  (save-excursion
    (when pos (goto-char pos))
    (or (sp--looking-at-p (sp--get-stringlike-regexp))
        (evil-cp--looking-at-paren-p))))

(defun evil-cp--looking-at-string-opening-p (&optional pos)
  "Predicate for checking if point is on a opening string delimiter."
  (save-excursion
    (when pos (goto-char pos))
    (and (evil-cp--looking-at-string-delimiter-p)
         (progn
           (forward-char)
           (nth 3 (syntax-ppss))))))

(defun evil-cp--looking-at-string-closing-p (&optional pos)
  "Predicate for checking if point is on a closing delimiter."
  (save-excursion
    (when pos (goto-char pos))
    (and (evil-cp--looking-at-string-delimiter-p)
         (not (paredit-in-string-escape-p))
         (nth 3 (syntax-ppss)))))

(defun evil-cp--looking-at-any-opening-p (&optional pos)
  "Predicate to check if point (or POS) is on an opening
parentheses or a string delimiter."
  (or (evil-cp--looking-at-opening-p pos)
      (evil-cp--looking-at-string-opening-p pos)))

(defun evil-cp--looking-at-any-closing-p (&optional pos)
  "Predicate to check if point (or POS) is on an opening
parentheses or a string delimiter."
  (or (evil-cp--looking-at-closing-p pos)
      (evil-cp--looking-at-string-closing-p pos)))

(defun evil-cp-region-ok-p (beg end)
  "The same as `sp-region-ok-p' where `' are not treated as a
valid pair."
  (let ((sp-pair-list evil-cp-sp-pair-list))
    (sp-region-ok-p beg end)))

(defmacro evil-cp--guard-point (&rest body)
  "Evil/Vim and Emacs have different opinions on where the point
is with respect to the visible cursor. This macro is used to make
sure that commands that are used to the Emacs view still work
when the cursor in evil is on top of an opening parentheses or a
string delimiter."
  `(if (evil-cp--looking-at-any-opening-p)
       (save-excursion
         (forward-char 1)
         ,@body)
     ,@body))

(defmacro evil-cp--guard-point-inc-string (&rest body)
  "Adjust cursor if on any opening and execute BODY.
Just like `evil-cp--guard-point' but works inside strings."
  `(if (evil-cp--looking-at-opening-anywhere-p)
       (save-excursion
         (forward-char 1)
         ,@body)
     ,@body))

(defmacro evil-cp--point-after (&rest body)
  "Return location of point after performing body."
  `(save-excursion
     ,@body
     (point)))

(defmacro evil-cp--movement-bounds (movement)
  "Returns a cons-pair containing the range of motion that
executing MOVEMENT performs or nil if point wasn't moved."
  (let ((beg (cl-gensym))
        (end (cl-gensym)))
    `(let ((,beg (point))
           (,end (evil-cp--point-after ,movement)))
       (when (not (= ,beg ,end))
         (cons ,beg ,end)))))

(defmacro evil-cp--safe-line-bounds (&rest body)
  "Performs the actions in BODY and checks if the line where
point lands is a safe region, in which case its bounds are
returned."
  `(save-excursion
     ,@body
     (let ((pbol (line-beginning-position))
           (peol (line-end-position)))
       (when (and (sp-region-ok-p pbol peol))
         (cons pbol peol)))))

(defmacro evil-cp--next-thing-bounds (&optional movement by-line-p)
  "Fetches the bounds for the next hing which may be a symbol, a
form, a line or a comment block."
  `(save-excursion
     ,movement
     (evil-cp--next-thing-bounds* ,by-line-p)))

(defmacro evil-cp--previous-thing-bounds (&optional movement by-line-p)
  "Fetches the bounds for the previous thing which may be a
symbol, a form, a line or a comment block."
  `(save-excursion
     ,movement
     (evil-cp--previous-thing-bounds* ,by-line-p)))

(defun evil-cp--looking-at-empty-form ()
  "A predicate for checking if the point is currently looking at
an empty form."
  (evil-cp--guard-point
   (or (sp-point-in-empty-sexp)
       (sp-point-in-empty-string))))

(defun evil-cp--inside-form-p (&optional pos)
  "Predicate for checking if point is inside a sexp."
  (save-excursion
    (when pos (goto-char pos))
    (when (evil-cp--looking-at-opening-p) (forward-char))
    (not (zerop (nth 0 (syntax-ppss))))))

(defun evil-cp--inside-string-p (&optional pos)
  "Predicate that returns true if point is inside a string."
  (save-excursion
    (when pos (goto-char pos))
    (when (not (or (eobp) (bobp)))
      (let ((string-ppss (nth 3 (syntax-ppss))))
        (or string-ppss
            (progn
              (forward-char)
              (nth 3 (syntax-ppss))))))))

(defun evil-cp--inside-any-form-p (&optional pos)
  "Predicate that returns true if point is either inside a sexp
or a string."
  (or (evil-cp--inside-form-p pos)
      (evil-cp--inside-string-p pos)))

(defun evil-cp--top-level-form-p (&optional pos)
  "Predicate that returns true if point is inside a top-level sexp."
  (save-excursion
    (when pos (goto-char pos))
    (evil-cp--guard-point
     (let* ((ppss (syntax-ppss))
            (n0   (nth 0 ppss))
            (n3   (nth 3 ppss)))
       (or (and (eq n0 1) (not n3)) ; top-level sexp
           (and (eq n0 0) n3))))))    ; top-level string

(defun evil-cp--outside-form-p (&optional pos)
  "Prediacate for checking if the point is outside any form or
string."
  (let ((sppss (syntax-ppss pos)))
    (and (zerop (car sppss))
         (not (nth 3 sppss)))))

(defun evil-cp--string-bounds (&optional pos)
  "Returns the location of the beginning and ending positions for
a string form. Accepts an optional argument POS for moving the
point to that location."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (or (sp-get-quoted-string-bounds)
          (progn
            (forward-char)
            (sp-get-quoted-string-bounds))))))

(defun evil-cp--point-in-comment (&optional pos)
  "Slightly cheaper to check version of `sp-point-in-comment'. Does not
support comments with open/closing delimiters."
  (setq pos (or pos (point)))
  (or (looking-at "\\s<")
      (save-excursion
        (nth 4 (syntax-ppss pos)))))

(defun evil-cp--point-in-string-or-comment (&optional pos)
  "Slightly cheaper to check version of
`sp-point-in-string-or-comment'. Does not support comments with
open/closing delimiters."
  (or (evil-cp--point-in-comment pos)
      (sp-point-in-string pos)))

(defun evil-cp--skip-whitespace-and-comments (&optional reversep)
  "Skips whitespace and comments forward."
  (catch 'stop
    (if reversep
        (while (or (looking-back evil-cp--ws-regexp (1- (point)))
                   (evil-cp--point-in-comment (1- (point))))
          (backward-char)
          (when (bobp) (throw 'stop :bobp)))
      (while (or (looking-at evil-cp--ws-regexp)
                 (evil-cp--point-in-comment))
        (forward-char)
        (when (and sp-comment-char (looking-at sp-comment-char))
          (forward-line))
        (when (eobp) (throw 'stop :eobp))))))

(defun evil-cp--backward-up-list (&optional ignore-strings-p)
  "Workaround for `backward-up-list' not working inside strings.
If IGNORE-STRINGS-P is t then strings are ignored when moving up.
Otherwise they are treated as lists. Returns the location of
point when the operation was successful."
  (interactive)
  (when (let ((sppss (syntax-ppss)))
          (or (not (zerop (car sppss)))
              (nth 3 sppss)))
    (if (not (nth 3 (syntax-ppss)))
        (progn
          (backward-up-list)
          (point))
      (when (not (and ignore-strings-p (evil-cp--top-level-form-p)))
        (while (nth 3 (syntax-ppss))
          (backward-char))
        (when ignore-strings-p
          (backward-up-list))
        (point)))))

(defun evil-cp--up-list (&optional ignore-strings-p)
  "Workaround for `up-list' not working inside strings. If
IGNORE-STRINGS-P is t then strings are ignored when moving up.
Otherwise they are treated as lists. Returns the location of
point when the operation was successful."
  (interactive)
  (when (let ((sppss (syntax-ppss)))
          (or (not (zerop (car sppss)))
              (nth 3 sppss)))
    (if (not (nth 3 (syntax-ppss)))
        (progn
          (up-list)
          (point))
      (when (not (and ignore-strings-p (evil-cp--top-level-form-p)))
        (while (nth 3 (syntax-ppss))
          (forward-char))
        (when ignore-strings-p
          (up-list))
        (point)))))

(defun evil-cp--top-level-bounds (&optional pos)
  "Returns the bounds for the top-level form point is in, or POS
if given. Note that this is different from defun-bounds, as defun
would ignore top-level forms that are on the same line."
  (save-excursion
    (when pos (goto-char pos))
    (when (evil-cp--inside-form-p)
      (catch 'done
        (while t
          (evil-cp--backward-up-list)
          (when (evil-cp--top-level-form-p)
            (throw 'done (cons (point) (forward-list)))))))))

(defun evil-cp--beginning-of-top-level ()
  (when (evil-cp--inside-form-p)
    (goto-char (car (evil-cp--top-level-bounds)))))

(defun evil-cp--end-of-top-level ()
  (when (evil-cp--inside-form-p)
    (goto-char (cdr (evil-cp--top-level-bounds)))))

(defun evil-cp--matching-paren-pos (&optional pos)
  "Finds the location of the matching paren for where point is
located. Also works for strings. Takes an optional POS argument
for temporarily moving the point to before proceeding.
  Assumes that the parentheses in the buffer are balanced."
  (save-excursion
    (cond ((evil-cp--looking-at-opening-p)
           (progn
             (sp-forward-sexp 1)
             (backward-char)
             (point)))

          ((evil-cp--looking-at-closing-p)
           (progn
             (forward-char 1)
             (sp-backward-sexp 1)
             (point)))

          ((looking-at "\"")
           (let* ((bounds (evil-cp--string-bounds)))
             (if (= (car bounds) (point))
                 (1- (cdr bounds))
               (car bounds)))))))

(defun evil-cp--text-balanced-p (text)
  "Checks if the string TEXT is balanced or not."
  (with-temp-buffer
    (insert text)
    (sp-region-ok-p (point-min) (point-max))))

(defun evil-cp--balanced-block-p (beg end)
  "Checks whether the block defined by BEG and END contains
balanced parentheses."
  (let ((region (evil-yank-rectangle beg end)))
    (with-temp-buffer
      (insert (or region ""))
      (sp-region-ok-p (point-min) (point-max)))))

(defun evil-cp--sp-obj-bounds (thing)
  "Helper for turning a smartparens text object into a cons-pair."
  (sp-get thing
    (when :beg (cons :beg :end))))

(defun evil-cp--get-enclosing-bounds (&optional prefixp)
  "Returns a tuple of start/end positions for the surrounding
sexp. If PREFIXP is true, then the beginning bound starts from
the beginning of the prefix as defined by `sp-sexp-prefix'."
  (when (evil-cp--inside-any-form-p)
    (save-excursion
      (evil-cp--guard-point
       (sp-get (if (sp-point-in-string (point))
                   (sp-get-string t)
                 (sp-get-enclosing-sexp))
         (cons (if (and prefixp (not (string-empty-p :prefix)))
                   (- :beg (length :prefix))
                 :beg)
               :end))))))

(defun evil-cp--next-sexp-bounds (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (when (not (evil-cp--looking-at-whitespace-p))
      (evil-cp--movement-bounds (forward-sexp)))))

(provide 'evil-cleverparens-util)
;;; evil-cleverparens-util.el ends here
