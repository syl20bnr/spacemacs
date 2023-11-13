;;; org-eval-light.el --- Display result of evaluating code in various languages (light)

;; Copyright (C) 2008-2021 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>,
;;         Eric Schulte <schulte dot eric at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp, literate programming,
;;           reproducible research
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 0.04

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is based off of org-eval, with the following changes.
;;
;; 1) forms are only executed manually, (allowing for the execution of
;;    an entire subtree of forms)
;; 2) use the org-mode style src blocks, rather than the muse style
;;    <code></code> blocks
;; 3) forms are not replaced by their outputs, but rather the output
;;    is placed in the buffer immediately following the src block
;;    commented by `org-eval-light-make-region-example' (when
;;    evaluated with a prefix argument no output is placed in the
;;    buffer)
;; 4) add defadvice to org-ctrl-c-ctrl-c so that when called inside of
;;    a source block it will call `org-eval-light-current-snippet'

;;; Code:
(require 'org)

(defgroup org-eval-light nil
  "Options concerning including output from commands into the Org-mode buffer."
  :tag "Org Eval"
  :group 'org)

(defvar org-eval-light-example-size-cutoff 10
  "The number of lines under which an example is considered
'small', and is exported with the '^:' syntax instead of in a
large example block")

(defvar org-eval-light-regexp nil)

(defun org-eval-light-set-interpreters (var value)
  (set-default var value)
  (setq org-eval-light-regexp
	(concat "#\\+begin_src \\("
		(mapconcat 'regexp-quote value "\\|")
		"\\)\\([^\000]+?\\)#\\+end_src")))

(defcustom org-eval-light-interpreters '("lisp" "emacs-lisp" "ruby" "shell")
  "Interpreters allows for evaluation tags.
This is a list of program names (as strings) that can evaluate code and
insert the output into an Org-mode buffer.  Valid choices are

lisp    Interpret Emacs Lisp code and display the result
shell   Pass command to the shell and display the result
perl    The perl interpreter
python  Thy python interpreter
ruby    The ruby interpreter"
  :group 'org-eval-light
  :set 'org-eval-light-set-interpreters
  :type '(set :greedy t
	      (const "lisp")
	      (const "emacs-lisp")
	      (const "perl")
	      (const "python")
	      (const "ruby")
	      (const "shell")))

;;; functions
(defun org-eval-light-inside-snippet ()
  (interactive)
  (save-excursion
    (let ((case-fold-search t)
	  (start-re "^#\\+begin_src\\( \\([^ \t\n]+\\)\\)?.*\n")
	  (end-re "\n#\\+end_src")
	  (pos (point))
	  beg end)
      (if (and (setq beg (re-search-backward start-re nil t))
	       (setq end (re-search-forward end-re nil t))
	       (<= beg pos) (>= end pos))
	  t))))

(defun org-eval-light-make-region-example (beg end)
  "Comment out region using either the '^:' or the BEGIN_EXAMPLE
syntax based on the size of the region as compared to
`org-eval-light-example-size-cutoff'."
  (interactive "*r")
  (let ((size (abs (- (line-number-at-pos end)
		      (line-number-at-pos beg)))))
    (if (= size 0)
	(let ((result (buffer-substring beg end)))
	  (delete-region beg end)
	  (insert (concat ": " result)))
      (if (<= size org-eval-light-example-size-cutoff)
	  (save-excursion
	    (goto-char beg)
	    (dotimes (n size)
	      (move-beginning-of-line 1) (insert ": ") (forward-line 1)))
	(let ((result (buffer-substring beg end)))
	  (delete-region beg end)
	  (insert (concat "#+BEGIN_EXAMPLE\n" result "#+END_EXAMPLE\n")))))))

(defun org-eval-light-current-snippet (&optional arg)
  "Execute the current #+begin_src #+end_src block, and dump the
results into the buffer immediately following the src block,
commented by `org-eval-light-make-region-example'."
  (interactive "P")
  (let ((line (org-current-line))
	(case-fold-search t)
	(info (org-edit-src-find-region-and-lang))
	beg end lang result)
    (setq beg (nth 0 info)
	    end (nth 1 info)
	    lang (nth 2 info))
    (unless (member lang org-eval-light-interpreters)
      (error "Language is not in `org-eval-light-interpreters': %s" lang))
    (goto-line line)
    (setq result (org-eval-light-code lang (buffer-substring beg end)))
    (unless arg
      (save-excursion
      (re-search-forward "^#\\+end_src" nil t) (open-line 1) (forward-char 2)
      (let ((beg (point))
	    (end (progn (insert result)
			(point))))
	(message (format "from %S %S" beg end))
	(org-eval-light-make-region-example beg end))))))

(defun org-eval-light-eval-subtree (&optional arg)
  "Replace EVAL snippets in the entire subtree."
  (interactive "P")
  (save-excursion
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (while (re-search-forward org-eval-light-regexp nil t)
      (org-eval-light-current-snippet arg))
    (widen)))

(defun org-eval-light-code (interpreter code)
  (cond
   ((member interpreter '("lisp" "emacs-lisp"))
    (org-eval-light-lisp (concat "(progn\n" code "\n)")))
   ((equal interpreter "shell")
    (shell-command-to-string code))
   ((member interpreter '("perl" "python" "ruby"))
    (org-eval-light-run (executable-find interpreter) code))
   (t (error "Cannot evaluate code type %s" interpreter))))

(defun org-eval-light-lisp (form)
  "Evaluate the given form and return the result as a string."
  (require 'pp)
  (save-match-data
    (condition-case err
        (let ((object (eval (read form))))
          (cond
           ((stringp object) object)
           ((and (listp object)
                 (not (eq object nil)))
            (let ((string (pp-to-string object)))
              (substring string 0 (1- (length string)))))
           ((numberp object)
            (number-to-string object))
           ((eq object nil) "")
           (t
            (pp-to-string object))))
      (error
       (org-display-warning (format "%s: Error evaluating %s: %s"
                                     "???" form err))
       "; INVALID LISP CODE"))))

(defun org-eval-light-run (cmd code)
  (with-temp-buffer
    (insert code)
    (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
    (buffer-string)))

(defadvice org-ctrl-c-ctrl-c (around org-cc-eval-source activate)
  (if (org-eval-light-inside-snippet)
      (call-interactively 'org-eval-light-current-snippet)
    ad-do-it))

(provide 'org-eval-light)

;;; org-eval-light.el ends here
