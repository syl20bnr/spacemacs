;;; org-static-mathjax.el --- Muse-like tags in Org-mode
;;
;; Author: Jan Böker <jan dot boecker at jboecker dot de>

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
;;
;; This elisp code integrates Static MathJax into the
;; HTML export process of Org-mode.
;;
;; The supporting files for this package are in contrib/scripts/staticmathjax
;; Please read the README.org file in that directory for more information.

;; To use it, evaluate it on startup, add the following to your .emacs:

;; (require 'org-static-mathjax)
;;
;; You will then have to customize the following two variables:
;; - org-static-mathjax-app-ini-path
;; - org-static-mathjax-local-mathjax-path
;;
;; If xulrunner is not in your $PATH, you will also need to customize
;; org-static-mathjax-xulrunner-path.
;;
;; If everything is setup correctly, you can trigger Static MathJax on
;; export to HTML by adding the following line to your Org file:
;; #+StaticMathJax: embed-fonts:nil output-file-name:"embedded-math.html"
;;
;; You can omit either argument.
;; embed-fonts defaults to nil. If you do not specify output-file-name,
;; the exported file is overwritten with the static version.
;;
;; If embed-fonts is non-nil, the fonts are embedded directly into the
;; output file using data: URIs.
;;
;; output-file-name specifies the file name of the static version. You
;; can use any arbitrary lisp form here, for example:
;; output-file-name:(concat (file-name-sans-extension buffer-file-name) "-static.html")
;;
;; The StaticMathJax XULRunner application expects a UTF-8 encoded
;; input file. If the static version displays random characters instead
;; of your math, add the following line at the top of your Org file:
;; -*- coding: utf-8; -*-
;;
;;; Code:

(defcustom org-static-mathjax-app-ini-path
  (or (expand-file-name
       "../scripts/staticmatchjax/application.ini"
       (file-name-directory (or load-file-name buffer-file-name)))
      "")
  "Path to \"application.ini\" of the Static MathJax XULRunner application.
If you have extracted StaticMathJax to e.g. ~/.local/staticmathjax, set
this to ~/.local/staticmathjax/application.ini"
  :type 'string)

(defcustom org-static-mathjax-xulrunner-path
  "xulrunner"
  "Path to your xulrunner binary"
  :type 'string)

(defcustom org-static-mathjax-local-mathjax-path
  ""
  "Extract the MathJax zip file somewhere on your local
hard drive and specify the path here.

The directory has to be writeable, as org-static-mathjax
creates a temporary file there during export."
  :type 'string)

(defvar org-static-mathjax-debug
  nil
  "If non-nil, org-static-mathjax will print some debug messages")

(defun org-static-mathjax-hook-installer ()
  "Installs org-static-mathjax-process in after-save-hook.

Sets the following buffer-local variables for org-static-mathjax-process to pick up:
org-static-mathjax-mathjax-path: The path to MathJax.js as used by Org HTML export
org-static-mathjax-options:      The string given with #+STATICMATHJAX: in the file"
  (let ((static-mathjax-option-string (plist-get opt-plist :static-mathjax)))
	(if static-mathjax-option-string
		(progn (set (make-local-variable 'org-static-mathjax-options) static-mathjax-option-string)
			   (set (make-local-variable 'org-static-mathjax-mathjax-path)
					(nth 1 (assq 'path org-export-html-mathjax-options)))
			   (let ((mathjax-options (plist-get opt-plist :mathjax)))
				 (if mathjax-options
					 (if (string-match "\\<path:" mathjax-options)
						 (set 'org-static-mathjax-mathjax-path
							  (car (read-from-string
									(substring mathjax-options (match-end 0))))))))
			   (add-hook 'after-save-hook
						 'org-static-mathjax-process
						 nil t)))))


(defun org-static-mathjax-process ()
  (save-excursion
	; some sanity checking
	(if (or (string= org-static-mathjax-app-ini-path "")
			(not (file-exists-p org-static-mathjax-app-ini-path)))
		(error "Static MathJax: You must customize org-static-mathjax-app-ini-path!"))
	(if (or (string= org-static-mathjax-local-mathjax-path "")
			(not (file-exists-p org-static-mathjax-local-mathjax-path)))
		(error "Static MathJax: You must customize org-static-mathjax-local-mathjax-path!"))

	; define variables
	(let* ((options org-static-mathjax-options)
		   (output-file-name buffer-file-name)
		   (input-file-name (let ((temporary-file-directory (file-name-directory org-static-mathjax-local-mathjax-path)))
							  (make-temp-file "org-static-mathjax-" nil ".html")))
		   (html-code (buffer-string))
		   (mathjax-oldpath (concat "src=\"" org-static-mathjax-mathjax-path))
		   (mathjax-newpath (concat "src=\"" org-static-mathjax-local-mathjax-path))
		   embed-fonts)
	  ; read file-local options
	  (mapc
	   (lambda (symbol)
		 (if (string-match (concat "\\<" (symbol-name symbol) ":") options)
			 (set symbol (eval (car (read-from-string
									 (substring options (match-end 0))))))))
	   '(embed-fonts output-file-name))

	  ; debug
	  (when org-static-mathjax-debug
		(message "output file name, embed-fonts")
		(print output-file-name)
		(print embed-fonts))

	  ; open (temporary) input file, copy contents there, replace MathJax path with local installation
	  (with-temp-buffer
		(insert html-code)
		(goto-char 1)
		(replace-regexp mathjax-oldpath mathjax-newpath)
		(write-file input-file-name))

	  ; prepare argument list for call-process
	  (let ((call-process-args (list org-static-mathjax-xulrunner-path
									 nil nil nil
									 org-static-mathjax-app-ini-path
									 input-file-name
									 output-file-name)))
		; if fonts are embedded, just append the --embed-fonts flag
		(if embed-fonts
			(add-to-list 'call-process-args "--embed-fonts" t))
		; if fonts are not embedded, the XULRunner app must replace all references
		; to the font files with the real location (Firefox inserts file:// URLs there,
		; because we are using a local MathJax installation here)
		(if (not embed-fonts)
			(progn
			  (add-to-list 'call-process-args "--final-mathjax-url" t)
			  (add-to-list 'call-process-args
						   (file-name-directory org-static-mathjax-mathjax-path)
						   t)))

		; debug
		(when org-static-mathjax-debug
		  (print call-process-args))
		; call it
		(apply 'call-process call-process-args)
		; delete our temporary input file
		(kill-buffer)
		(delete-file input-file-name)
		(let ((backup-file (concat input-file-name "~")))
		  (if (file-exists-p backup-file)
			  (delete-file backup-file)))))))

(add-to-list 'org-export-inbuffer-options-extra
'("STATICMATHJAX" :static-mathjax))

(add-hook 'org-export-html-final-hook 'org-static-mathjax-hook-installer)


(provide 'org-static-mathjax)
