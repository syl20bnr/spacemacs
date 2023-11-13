;;; ob-oz.el --- Org-babel functions for Oz evaluation

;; Copyright (C) 2009-2014, 2021 Torsten Anders and Eric Schulte

;; Author: Torsten Anders and Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 0.02

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating Oz source code.
;;
;; Oz code is always send to the Oz Programming Environment (OPI), the
;; Emacs mode and compiler interface for Oz programs. Therefore, only
;; session mode is supported. In practice, non-session code blocks are
;; handled equally well by the session mode. However, only a single
;; session is supported. Consequently, the :session header argument is
;; ignored.
;;
;; The Org-babel header argument :results is interpreted as
;; follows. :results output requires the respective code block to be
;; an Oz statement and :results value requires an Oz
;; expression. Currently, results are only supported for expressions
;; (i.e. the result of :results output is always nil).
;;
;; Expression evaluation happens synchronously. Therefore there is an
;; additional header argument :wait-time <number>, which specifies the
;; maximum time to wait for the result of a given expression. nil
;; means to wait as long as it takes to get a result (potentially wait
;; forever).
;;
;; NOTE: Currently the copyright of this file may not be in a state to
;;       permit inclusion as core software into Emacs or Org-mode.

;;; Requirements:

;; - Mozart Programming System, the implementation of the Oz
;;   programming language (http://www.mozart-oz.org/), which includes
;;   the major mode mozart for editing Oz programs.
;;
;; - StartOzServer.oz which is located in the contrib/scripts
;;   directory of the Org-mode repository

;;; TODO:

;; - Decide: set communication to \\switch -threadedqueries?
;;
;; - Only start Oz compiler when required, e.g., load Org-babel only when needed?
;;
;; - Avoid synchronous evaluation to avoid blocking Emacs (complex
;;   Strasheela programs can take long to find a result..). In order
;;   to cleanly map code blocks to their associated results (which can
;;   arrive then in any order) I could use IDs
;;   (e.g. integers). However, how do I do concurrency in Emacs Lisp,
;;   and how can I define org-babel-execute:oz concurrently.
;;
;; - Expressions are rarely used in Oz at the top-level, and using
;;   them in documentation and Literate Programs will cause
;;   confusion. Idea: hide expression from reader and instead show
;;   them statement (e.g., MIDI output statement) and then include
;;   result in Org file. Implementation: for expressions (:results
;;   value) support an additional header argument that takes arbitrary
;;   Oz code. This code is not seen by the reader, but will be used
;;   for the actual expression at the end.  Alternative: feed all
;;   relevant code as statement (:results output), then add expression
;;   as extra code block which outputs, e.g., file name (so the file
;;   name must be accessible by global var), but the code of this
;;   extra codeblock is not seen.  Hm, in that case it might be even
;;   more easy to manually add this link to the Org file.
;;


(require 'ob)
;;; major mode for editing Oz programs
(require 'mozart nil t)

;;
;; Interface to communicate with Oz.
;; (1) For statements without any results: oz-send-string
;; (2) For expressions with a single result: oz-send-string-expression
;;     (defined in org-babel-oz-ResultsValue.el)
;;

;; oz-send-string-expression implements an additional very direct
;; communication between Org-babel and the Oz compiler. Communication
;; with the Oz server works already without this code via the function
;; oz-send-string from mozart.el.in, but this function does not get
;; back any results from Oz to Emacs. The following code creates a
;; socket for sending code to the OPI compiler and results are
;; returned by the same socket. On the Oz side, a socket is opened and
;; connected to the compiler of the OPI (via oz-send-string). On the
;; Emacs side, a connection to this socket is created for feeding code
;; and receiving results. This additional communication channel to the
;; OPI compiler ensures that results are returned cleanly (e.g., only
;; the result of the sent code is returned, no parsing or any
;; processing of *Oz Emulator* is required).
;;
;; There is no buffer, nor sentinel involved. Oz code is send
;; directly, and results from Oz are send back, but Emacs Lisp
;; requires a filter function for processing results.

(defvar org-babel-oz-server-dir
  (file-name-as-directory
   (expand-file-name
    "contrib/scripts"
    (file-name-as-directory
     (expand-file-name
      "../../.."
      (file-name-directory (or load-file-name buffer-file-name))))))
  "Path to the contrib/scripts directory in which
StartOzServer.oz is located.")

(defvar org-babel-oz-port 6001
  "Port for communicating with Oz compiler.")
(defvar org-babel-oz-OPI-socket nil
  "Socket for communicating with OPI.")

(defvar org-babel-oz-collected-result nil
  "Aux var to hand result from org-babel-oz-filter to oz-send-string-expression.")
(defun org-babel-oz-filter (proc string)
  "Processes output from socket org-babel-oz-OPI-socket."
;;   (setq org-babel-oz-collected-results (cons string org-babel-oz-collected-results))
  (setq org-babel-oz-collected-result string)
  )


(defun org-babel-oz-create-socket ()
  (message "Create OPI socket for evaluating expressions")
  ;; Start Oz directly
  (run-oz)
  ;; Create socket on Oz side (after Oz was started).
  (oz-send-string (concat "\\insert '" org-babel-oz-server-dir "StartOzServer.oz'"))
  ;; Wait until socket is created before connecting to it.
  ;; Quick hack: wait 3 sec
  ;;
  ;; extending time to 30 secs does not help when starting Emacs for
  ;; the first time (and computer does nothing else)
  (sit-for 3)
  ;; connect to OPI socket
  (setq org-babel-oz-OPI-socket
	;; Creates a socket. I/O interface of Emacs sockets as for processes.
	(open-network-stream "*Org-babel-OPI-socket*" nil "localhost" org-babel-oz-port))
  ;; install filter
  (set-process-filter org-babel-oz-OPI-socket #'org-babel-oz-filter)
)

;; communication with org-babel-oz-OPI-socket is asynchronous, but
;; oz-send-string-expression turns is into synchronous...
(defun oz-send-string-expression (string &optional wait-time)
  "Similar to oz-send-string, oz-send-string-expression sends a string to the OPI compiler. However, string must be expression and this function returns the result of the expression (as string). oz-send-string-expression is synchronous, wait-time allows to specify a maximum wait time. After wait-time is over with no result, the function returns nil."
  (if (not org-babel-oz-OPI-socket)
      (org-babel-oz-create-socket))
  (let ((polling-delay 0.1)
	result)
    (process-send-string org-babel-oz-OPI-socket string)
    ;; wait for result
    (if wait-time
	(let ((waited 0))
	  (unwind-protect
	      (progn
		(while
		    ;; stop loop if org-babel-oz-collected-result \= nil or waiting time is over
		    (not (or (not (equal org-babel-oz-collected-result nil))
			     (> waited wait-time)))
		  (progn
		    (sit-for polling-delay)
;; 		    (message "org-babel-oz: next polling iteration")
		    (setq waited (+ waited polling-delay))))
;; 		(message "org-babel-oz: waiting over, got result or waiting timed out")
;; 		(message (format "wait-time: %s, waited: %s" wait-time waited))
		(setq result org-babel-oz-collected-result)
		(setq org-babel-oz-collected-result nil))))
      (unwind-protect
	  (progn
	    (while (equal org-babel-oz-collected-result nil)
	      (sit-for polling-delay))
	    (setq result org-babel-oz-collected-result)
	    (setq org-babel-oz-collected-result nil))))
    result))

(defun org-babel-expand-body:oz (body params)
  (let ((vars (org-babel--get-vars params)))
    (if vars
	;; prepend code to define all arguments passed to the code block
	(let ((var-string (mapcar (lambda (pair)
				    (format "%s=%s"
					    (car pair)
					    (org-babel-oz-var-to-oz (cdr pair))))
				  vars)))
	  ;; only add var declarations if any variables are there
	  (mapconcat #'identity
		     (append (list "local") var-string (list "in" body "end"))
		     "\n"))
      body)))

(defun org-babel-execute:oz (body params)
  "Execute a block of Oz code with org-babel.  This function is
called by `org-babel-execute-src-block' via multiple-value-bind."
  (let* ((result-params (cdr (assq :result-params params)))
	 (full-body (org-babel-expand-body:oz body params))
	 (wait-time (plist-get params :wait-time)))
    ;; actually execute the source-code block
    (org-babel-reassemble-table
     (cond
      ((member "output" result-params)
       (message "Org-babel: executing Oz statement")
       (oz-send-string full-body))
      ((member "value" result-params)
       (message "Org-babel: executing Oz expression")
       (oz-send-string-expression full-body (or wait-time 1)))
      (t (error "either 'output' or 'results' must be members of :results")))
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :roname-names params))
			  (cdr (assq :rownames params))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:oz (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "org-babel-prep-session:oz unimplemented"))
;; TODO: testing... (copied from org-babel-haskell.el)
;; (defun org-babel-prep-session:oz (session params)
;;   "Prepare SESSION according to the header arguments specified in PARAMS."
;;   (save-window-excursion
;;     (org-babel-oz-initiate-session session)
;;     (let* ((vars (org-babel-ref-variables params))
;;            (var-lines (mapconcat ;; define any variables
;;                        (lambda (pair)
;;                          (format "%s=%s"
;;                                  (car pair)
;;                                  (org-babel-ruby-var-to-ruby (cdr pair))))
;;                        vars "\n"))
;;            (vars-file (concat (make-temp-file "org-babel-oz-vars") ".oz")))
;;       (when vars
;;         (with-temp-buffer
;;           (insert var-lines) (write-file vars-file)
;;           (oz-mode)
;; ;; 	  (inferior-oz-load-file) ; ??
;; 	  ))
;;       (current-buffer))))
;;


;; TODO: testing... (simplified version of def in org-babel-prep-session:ocaml)
;;
;; BUG: does not work yet. Error: ad-Orig-error: buffer none doesn't exist or has no process
;; UNUSED DEF
(defun org-babel-oz-initiate-session (&optional session params)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    ;; TODO: make it possible to have multiple sessions
    (save-window-excursion
      ;; (run-oz)
      (get-buffer oz-compiler-buffer))))

(defun org-babel-oz-var-to-oz (var)
  "Convert an elisp var into a string of Oz source code
specifying a var of the same value."
  (if (listp var)
;;       (concat "[" (mapconcat #'org-babel-oz-var-to-oz var ", ") "]")
      (eval var)
    (format "%s" var) ; don't preserve string quotes.
;;     (format "%s" var)
    ))

;; TODO:
(defun org-babel-oz-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (error "org-babel-oz-table-or-string unimplemented"))


(provide 'ob-oz)
;;; org-babel-oz.el ends here
