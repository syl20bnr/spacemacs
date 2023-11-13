;;; ob-coq.el --- Babel Functions for Coq            -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Luc Pellissier <luc.pellissier@crans.org>
;; Keywords: literate programming, reproducible research
;; Homepage: https://git.sr.ht/~bzg/org-contrib

;; This file is not part of GNU Emacs.

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

;; Rudimentary support for evaluating Coq code blocks.  Currently only
;; session evaluation is supported.  Requires both coq.el and
;; coq-inferior.el, both of which are distributed with Coq.
;;
;; https://coq.inria.fr/

;;; Code:
(require 'ob)

(declare-function run-coq "ext:coq-inferior.el" (cmd))
(declare-function coq-proc "ext:coq-inferior.el" ())

(defvar coq-program-name "coqtop"
  "Name of the coq toplevel to run.")

(defvar org-babel-coq-buffer "*coq*"
  "Buffer in which to evaluate coq code blocks.")

(defun org-babel-coq-clean-prompt (string)
  (if (string-match "^[^[:space:]]+ < " string)
      (substring string 0 (match-beginning 0))
    string))

(defun org-babel-execute:coq (body params)
  (let ((full-body (org-babel-expand-body:generic body params))
	(session (org-babel-coq-initiate-session))
	(pt (lambda ()
	      (marker-position
	       (process-mark (get-buffer-process (current-buffer)))))))
    (org-babel-coq-clean-prompt
     (org-babel-comint-in-buffer session
       (let ((start (funcall pt)))
	 (with-temp-buffer
	   (insert full-body)
	   (comint-send-region (coq-proc) (point-min) (point-max))
	   (comint-send-string (coq-proc)
	    (if (string= (buffer-substring (- (point-max) 1) (point-max)) ".")
		"\n"
	      ".\n")))
	 (while (equal start (funcall pt)) (sleep-for 0.1))
	 (buffer-substring start (funcall pt)))))))

(defun org-babel-coq-initiate-session ()
  "Initiate a coq session.
If there is not a current inferior-process-buffer in SESSION then
create one.  Return the initialized session."
  (unless (fboundp 'run-coq)
    (error "`run-coq' not defined, load coq-inferior.el"))
  (save-window-excursion (run-coq coq-program-name))
  (sit-for 0.1)
  (get-buffer org-babel-coq-buffer))

(provide 'ob-coq)

;;; ob-coq.el ends here
