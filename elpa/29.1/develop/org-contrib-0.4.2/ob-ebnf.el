;;; ob-ebnf.el --- Babel Functions for EBNF          -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

;; Author: Michael Gauland
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

;; Org-Babel support for using ebnf2ps to generate encapsulated postscript
;; railroad diagrams.  It recognizes these arguments:
;;
;;     :file is required; it must include the extension '.eps.' All the rules
;;           in the block will be drawn in the same file.  This is done by
;;           inserting a '[<file>' comment at the start of the block (see the
;;           documentation for ebnf-eps-buffer for more information).
;;
;;     :style specifies a value in ebnf-style-database.  This provides the
;;            ability to customize the output.  The style can also specify the
;;            grammar syntax (by setting ebnf-syntax); note that only ebnf,
;;            iso-ebnf, and yacc are supported by this file.

;;; Requirements:

;;; Code:
(require 'ob)
(require 'ebnf2ps)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:ebnf '((:style . nil)))

;; Use ebnf-eps-buffer to produce an encapsulated postscript file.
;;
(defun org-babel-execute:ebnf (body params)
  "Execute a block of Ebnf code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (save-excursion
    (let* ((dest-file (cdr (assq :file params)))
	   (dest-dir (file-name-directory dest-file))
	   (dest-root (file-name-sans-extension
		       (file-name-nondirectory dest-file)))
	   (style (cdr (assq :style params)))
	   (result nil))
      (with-temp-buffer
	(when style (ebnf-push-style style))
	(let ((comment-format
	       (cond ((string= ebnf-syntax 'yacc) "/*%s*/")
		     ((string= ebnf-syntax 'ebnf) ";%s")
		     ((string= ebnf-syntax 'iso-ebnf) "(*%s*)")
		     (t (setq result
			      (format "EBNF error: format %s not supported."
				      ebnf-syntax))))))
	  (setq ebnf-eps-prefix dest-dir)
	  (insert (format comment-format (format "[%s" dest-root)))
	  (newline)
	  (insert body)
	  (newline)
	  (insert (format comment-format (format "]%s" dest-root)))
	  (ebnf-eps-buffer)
	  (when style (ebnf-pop-style))))
      result)))

(provide 'ob-ebnf)

;;; ob-ebnf.el ends here
