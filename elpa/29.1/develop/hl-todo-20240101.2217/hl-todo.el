;;; hl-todo.el --- Highlight TODO and similar keywords  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/hl-todo
;; Keywords: convenience

;; Package-Requires: ((emacs "25.1") (compat "29.1.4.2"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Highlight TODO and similar keywords in comments and strings.

;; You can either explicitly turn on `hl-todo-mode' in certain buffers
;; or use the global variant `global-hl-todo-mode', which enables
;; the local mode based on each buffer's major-mode and the options
;; `hl-todo-include-modes' and `hl-todo-exclude-modes'.  By default
;; `hl-todo-mode' is enabled for all buffers whose major-mode derive
;; from either `prog-mode' or `text-mode', except `org-mode'.

;; This package also provides commands for moving to the next or
;; previous keyword, to invoke `occur' with a regexp that matches all
;; known keywords, and to insert a keyword.  If you want to use these
;; commands, then you should bind them in `hl-todo-mode-map', e.g.:
;;
;;   (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
;;   (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
;;   (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
;;   (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert)

;;; Code:

(require 'compat)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

(defvar grep-find-template)
(declare-function grep-read-files "grep" (regexp))
(declare-function flymake-make-diagnostic "flymake")

(defgroup hl-todo nil
  "Highlight TODO and similar keywords in comments and strings."
  :group 'font-lock-extra-types)

(defface hl-todo
  '((t (:bold t :foreground "#cc9393")))
  "Base face used to highlight TODO and similar keywords.
The faces used to highlight certain keywords are, by default,
created by inheriting this face and using the appropriate
color specified using the option `hl-todo-keyword-faces' as
foreground color."
  :group 'hl-todo)

(define-obsolete-variable-alias 'hl-todo-activate-in-modes
  'hl-todo-include-modes "hl-todo 3.1.0")

(defcustom hl-todo-include-modes '(prog-mode text-mode conf-mode)
  "Major-modes in which `hl-todo-mode' is activated.

This is used by `global-hl-todo-mode', which activates the local
`hl-todo-mode' in all buffers whose major-mode derive from one
of the modes listed here, but not from one of the modes listed
in `hl-todo-exclude-modes'."
  :package-version '(hl-todo . "3.7.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-exclude-modes '(org-mode)
  "Major-modes in which `hl-todo-mode' is not activated.

This is used by `global-hl-todo-mode', which activates the local
`hl-todo-mode' in all buffers whose major-mode derived from one
of the modes listed in `hl-todo-include-modes', but not from one
of the modes listed here."
  :package-version '(hl-todo . "3.1.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-text-modes '(text-mode)
  "Major-modes that are considered text-modes.

In buffers whose major-mode derives from one of the modes listed
here TODO keywords are always highlighted even if they are not
located inside a string."
  :package-version '(hl-todo . "2.1.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-keyword-faces
  '(("HOLD"   . "#d0bf8f")
    ("TODO"   . "#cc9393")
    ("NEXT"   . "#dca3a3")
    ("THEM"   . "#dc8cc3")
    ("PROG"   . "#7cb8bb")
    ("OKAY"   . "#7cb8bb")
    ("DONT"   . "#5f7f5f")
    ("FAIL"   . "#8c5353")
    ("DONE"   . "#afd8af")
    ("NOTE"   . "#d0bf8f")
    ("MAYBE"  . "#d0bf8f")
    ("KLUDGE" . "#d0bf8f")
    ("HACK"   . "#d0bf8f")
    ("TEMP"   . "#d0bf8f")
    ("FIXME"  . "#cc9393")
    ("XXXX*"  . "#cc9393"))
  "An alist mapping keywords to colors/faces used to display them.

Each entry has the form (KEYWORD . COLOR).  KEYWORD is used as
part of a regular expression.  If (regexp-quote KEYWORD) is not
equal to KEYWORD, then it is ignored by `hl-todo-insert-keyword'.
Instead of a color (a string), each COLOR may alternatively be a
face.

The syntax class of the characters at either end has to be `w'
\(which means word) in `hl-todo--syntax-table' (which derives
from `text-mode-syntax-table').

This package, like most of Emacs, does not use POSIX regexp
backtracking.  See info node `(elisp)POSIX Regexp' for why that
matters.  If you have two keywords \"TODO-NOW\" and \"TODO\", then
they must be specified in that order.  Alternatively you could
use \"TODO\\(-NOW\\)?\".

If you use the command `hl-todo-rgrep', rewrite KEYWORDs to
use \"*\" instead of \"+\" and generally make sure they are valid
as Emacs regexps and as basic regular expressions as understood
by Grep.  If you customize variables in the `grep' group, or use
a Grep implementation other than GNU's, then that may break
`hl-todo-rgrep'."
  :package-version '(hl-todo . "3.5.0")
  :group 'hl-todo
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face"))))
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (dolist (buf (buffer-list))
	   (with-current-buffer buf
             (when (and (bound-and-true-p hl-todo-mode)
                        (boundp 'hl-todo--regexp))
               (setq hl-todo--regexp nil)
               (hl-todo-mode -1)
               (hl-todo-mode 1))))))

(defcustom hl-todo-color-background nil
  "Whether to emphasize keywords using the background color.

If an entry in `hl-todo-keyword-faces' specifies a face, then the
respective keyword is displayed using exactly that face.  In that
case this option is irrelevant.

Otherwise, if an entry specifies only a color, then the `hl-todo'
face controls the appearance of the respective keyword, except
for either the foreground or the background color.  This option
controls which of the two it is."
  :package-version '(hl-todo . "3.1.0")
  :group 'hl-todo
  :type 'boolean)

(defcustom hl-todo-wrap-movement nil
  "Whether movement commands wrap around when there are no more matches."
  :package-version '(hl-todo . "3.4.0")
  :group 'hl-todo
  :type 'boolean)

(defcustom hl-todo-highlight-punctuation ""
  "String of characters to highlight after keywords.

Each of the characters appearing in this string is highlighted
using the same face as the preceding keyword when it directly
follows the keyword.

Characters whose syntax class is `w' (which means word),
including alphanumeric characters, cannot be used here."
  :package-version '(hl-todo . "2.0.0")
  :group 'hl-todo
  :type 'string)

(defcustom hl-todo-require-punctuation nil
  "Whether to require punctuation after keywords."
  :package-version '(hl-todo . "3.3.0")
  :group 'hl-todo
  :type 'boolean)

(defvar hl-todo--keywords
  `((,(lambda (bound) (hl-todo--search nil bound))
     (1 (hl-todo--get-face) prepend t))))

(defvar-local hl-todo--regexp nil)

(defsubst hl-todo--regexp ()
  "Return regular expression matching TODO or similar keyword."
  (or hl-todo--regexp (hl-todo--setup-regexp)))

(defun hl-todo--setup-regexp ()
  "Setup keyword regular expression.
See the function `hl-todo--regexp'."
  (when-let ((bomb (assoc "???" hl-todo-keyword-faces)))
    ;; If the user customized this variable before we started to
    ;; treat the strings as regexps, then the string "???" might
    ;; still be present.  We have to remove it because it results
    ;; in the regexp search taking forever.
    (setq hl-todo-keyword-faces (delete bomb hl-todo-keyword-faces)))
  (setq hl-todo--regexp
        (concat "\\(\\<"
                "\\(" (mapconcat #'car hl-todo-keyword-faces "\\|") "\\)"
                "\\>"
                (and (not (equal hl-todo-highlight-punctuation ""))
                     (concat "[" hl-todo-highlight-punctuation "]"
                             (if hl-todo-require-punctuation "+" "*")))
                "\\)")))

(defvar hl-todo--syntax-table (copy-syntax-table text-mode-syntax-table))

(defvar syntax-ppss-table) ; Silence Emacs 25's byte-compiler.

(defun hl-todo--search (&optional regexp bound backward)
  "Search for keyword REGEXP, optionally up to BOUND and BACKWARD.
If REGEXP is not given, it defaults to the return value of the
function `hl-todo--regexp'."
  (unless regexp
    (setq regexp (hl-todo--regexp)))
  (cl-block nil
    (while (let ((case-fold-search nil)
                 (syntax-ppss-table (syntax-table)))
             (with-syntax-table hl-todo--syntax-table
               (funcall (if backward #'re-search-backward #'re-search-forward)
                        regexp bound t)))
      (cond ((or (apply #'derived-mode-p hl-todo-text-modes)
                 (hl-todo--inside-comment-or-string-p))
             (cl-return t))
            ((and bound (funcall (if backward #'<= #'>=) (point) bound))
             (cl-return nil))))))

(defun hl-todo--inside-comment-or-string-p ()
  "Check syntax state if point is located inside comment or string literal."
  (nth 8 (syntax-ppss)))

(defun hl-todo--get-face ()
  "Return face for current keyword during font locking."
  (let ((keyword (match-string 2)))
    (hl-todo--combine-face
     (cdr (or
           ;; Fast allocation free lookup for literal keywords
           (assoc keyword hl-todo-keyword-faces)
           ;; Slower regexp lookup
           (compat-call assoc keyword hl-todo-keyword-faces
                        (lambda (a b)
                          (string-match-p (format "\\`%s\\'" a) b))))))))

(defun hl-todo--combine-face (color)
  "Combine COLOR string with `hl-todo' default face.
If COLOR is a face symbol, do not combine, return COLOR instead."
  (if (stringp color)
      `((,(if hl-todo-color-background :background :foreground)
         ,color)
        hl-todo)
    color))

(defvar-keymap hl-todo-mode-map
  :doc "Keymap for `hl-todo-mode'.")

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter ""
  :keymap hl-todo-mode-map
  :group 'hl-todo
  (if hl-todo-mode
      (font-lock-add-keywords nil hl-todo--keywords t)
    (font-lock-remove-keywords nil hl-todo--keywords))
  (when font-lock-mode
    (jit-lock-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode hl-todo--turn-on-mode-if-desired)

(defun hl-todo--turn-on-mode-if-desired ()
  "Enable local minor mode `hl-todo-mode' if test succeeds.
Depends on `hl-todo-include-modes' and `hl-todo-exclude-modes'."
  (when (and (apply #'derived-mode-p hl-todo-include-modes)
             (not (apply #'derived-mode-p hl-todo-exclude-modes))
             (not (bound-and-true-p enriched-mode)))
    (hl-todo-mode 1)))

;;;###autoload
(defun hl-todo-next (arg)
  "Jump to the next TODO or similar keyword.
The prefix argument ARG specifies how many keywords to move.
A negative argument means move backward that many keywords."
  (interactive "p")
  (if (< arg 0)
      (hl-todo-previous (- arg))
    (while (and (> arg 0)
                (not (eobp))
                (progn
                  (when (let ((case-fold-search nil))
                          (looking-at (hl-todo--regexp)))
                    (goto-char (match-end 0)))
                  (or (hl-todo--search)
                      (if hl-todo-wrap-movement
                          nil
                        (user-error "No more matches")))))
      (cl-decf arg))
    (when (> arg 0)
      (let ((pos (save-excursion
                   (goto-char (point-min))
                   (let ((hl-todo-wrap-movement nil))
                     (hl-todo-next arg))
                   (point))))
        (goto-char pos)))))

;;;###autoload
(defun hl-todo-previous (arg)
  "Jump to the previous TODO or similar keyword.
The prefix argument ARG specifies how many keywords to move.
A negative argument means move forward that many keywords."
  (interactive "p")
  (if (< arg 0)
      (hl-todo-next (- arg))
    (while (and (> arg 0)
                (not (bobp))
                (let ((start (point)))
                  (hl-todo--search (concat (hl-todo--regexp) "\\=") nil t)
                  (or (hl-todo--search nil nil t)
                      (progn (goto-char start)
                             (if hl-todo-wrap-movement
                                 nil
                               (user-error "No more matches"))))))
      (goto-char (match-end 0))
      (cl-decf arg))
    (when (> arg 0)
      (let ((pos (save-excursion
                   (goto-char (point-max))
                   (let ((hl-todo-wrap-movement nil))
                     (hl-todo-previous arg))
                   (point))))
        (goto-char pos)))))

;;;###autoload
(defun hl-todo-occur ()
  "Use `occur' to find all TODO or similar keywords.
This actually finds a superset of the highlighted keywords,
because it uses a regexp instead of a more sophisticated
matcher.  It also finds occurrences that are not within a
string or comment."
  (interactive)
  (with-syntax-table hl-todo--syntax-table
    (occur (hl-todo--regexp))))

;;;###autoload
(defun hl-todo-rgrep (regexp &optional files dir confirm)
  "Use `rgrep' to find all TODO or similar keywords.
This actually finds a superset of the highlighted keywords,
because it uses a regexp instead of a more sophisticated
matcher.  It also finds occurrences that are not within a
string or comment.  See `rgrep' for the meaning of REGEXP,
FILES, DIR and CONFIRM, except that the type of prefix
argument does not matter; with any prefix you can edit the
constructed shell command line before it is executed.
Also see option `hl-todo-keyword-faces'."
  (interactive
   (progn
     (require 'grep)
     (grep-compute-defaults)
     (unless grep-find-template
       (error "grep.el: No `grep-find-template' available"))
     (let ((regexp (with-temp-buffer (hl-todo--regexp))))
       (list regexp
             (grep-read-files regexp)
             (read-directory-name "Base directory: " nil default-directory t)
             current-prefix-arg))))
  (rgrep regexp files dir confirm))

;;;###autoload
(defun hl-todo-flymake (report-fn &rest _plist)
  "Flymake backend for `hl-todo-mode'.
Diagnostics are reported to REPORT-FN.  Use `add-hook' to
register this function in `flymake-diagnostic-functions' before
enabling `flymake-mode'."
  (let ((diags nil)
        (buf (current-buffer))
        (comment (concat (regexp-quote comment-start) "\\s-+")))
    (when hl-todo-mode
      (save-excursion
        (save-restriction
          (save-match-data
            (goto-char (point-min))
            (while (hl-todo--search)
              (let ((beg (match-beginning 0))
                    (end (pos-eol))
                    (bol (pos-bol)))
                ;; Take whole line when keyword is not at the start of comment
                (save-excursion
                  (goto-char beg)
                  (unless (looking-back comment bol)
                    (goto-char bol)
                    ;; Skip whitespace at the beginning of line
                    (when (and (not (looking-at-p "\\S-"))
                               (re-search-forward "\\S-" beg t))
                      (forward-char -1))
                    ;; Skip comment
                    (re-search-forward comment beg t)
                    (setq beg (point))))
                (push (flymake-make-diagnostic
                       buf beg end :note
                       (buffer-substring-no-properties beg end))
                      diags)))))))
    (funcall report-fn (nreverse diags))))

;;;###autoload
(defun hl-todo-insert (keyword)
  "Insert TODO or similar keyword.
If point is not inside a string or comment, then insert a new
comment.  If point is at the end of the line, then insert the
comment there, otherwise insert it as a new line before the
current line.  When called interactively the KEYWORD is read via
`completing-read'."
  (interactive
   (list (completing-read
          "Insert keyword: "
          (mapcan (pcase-lambda (`(,keyword . ,face))
                    (and (equal (regexp-quote keyword) keyword)
                         (list (propertize keyword 'face
                                           (hl-todo--combine-face face)))))
                  hl-todo-keyword-faces))))
  (cond
   ((hl-todo--inside-comment-or-string-p)
    (insert (concat (and (not (memq (char-before) '(?\s ?\t))) " ")
                    keyword
                    (and (not (memq (char-after) '(?\s ?\t ?\n))) " "))))
   ((and (eolp)
         (not (looking-back "^[\s\t]*" (line-beginning-position) t)))
    (insert (concat (and (not (memq (char-before) '(?\s ?\t))) " ")
                    (format "%s %s " comment-start keyword))))
   (t
    (goto-char (line-beginning-position))
    (insert (cond ((derived-mode-p 'lisp-mode 'emacs-lisp-mode)
                   (format "%s%s %s" comment-start comment-start keyword))
                  ((string-suffix-p " " comment-start)
                   (format "%s%s" comment-start keyword))
                  (t
                   (format "%s %s" comment-start keyword))))
    (unless (looking-at "[\s\t]*$")
      (save-excursion (insert "\n")))
    (indent-region (line-beginning-position) (line-end-position)))))

(define-obsolete-function-alias 'hl-todo-insert-keyword
  #'hl-todo-insert "hl-todo 3.0.0")

;;; _
(provide 'hl-todo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-todo.el ends here
