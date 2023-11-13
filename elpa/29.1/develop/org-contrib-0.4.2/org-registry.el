;;; org-registry.el --- a registry for Org links
;;
;; Copyright 2007-2021 Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-registry.el
;; Version: 0.1a
;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: org, wp, registry
;; Description: Shows Org files where the current buffer is linked
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;;
;; This file is not part of GNU Emacs.
;;
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library add a registry to your Org setup.
;;
;; Org files are full of links inserted with `org-store-link'. This links
;; point to e-mail, webpages, files, dirs, info pages, man pages, etc.
;; Actually, they come from potentially *everywhere* since Org lets you
;; define your own storing/following functions.
;;
;; So, what if you are on a e-mail, webpage or whatever and want to know if
;; this buffer has already been linked to somewhere in your agenda files?
;;
;; This is were org-registry comes in handy.
;;
;;     M-x org-registry-show will tell you the name of the file
;; C-u M-x org-registry-show will directly jump to the file
;;
;; In case there are several files where the link lives in:
;;
;;     M-x org-registry-show will display them in a new window
;; C-u M-x org-registry-show will prompt for a file to visit
;;
;; Add this to your Org configuration:
;;
;; (require 'org-registry)
;; (org-registry-initialize)
;;
;; If you want to update the registry with newly inserted links in the
;; current buffer: M-x org-registry-update
;;
;; If you want this job to be done each time you save an Org buffer,
;; hook 'org-registry-update to the local 'after-save-hook in org-mode:
;;
;; (org-registry-insinuate)

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup org-registry nil
  "A registry for Org."
  :group 'org)

(defcustom org-registry-file
  (concat (getenv "HOME") "/.org-registry.el")
  "The Org registry file."
  :group 'org-registry
  :type 'file)

(defcustom org-registry-find-file 'find-file-other-window
  "How to find visit files."
  :type 'function
  :group 'org-registry)

(defvar org-registry-alist nil
  "An alist containing the Org registry.")

;;;###autoload
(defun org-registry-show (&optional visit)
  "Show Org files where there are links pointing to the current
buffer."
  (interactive "P")
  (org-registry-initialize)
  (let* ((blink (or (org-remember-annotation) ""))
	 (link (when (string-match org-bracket-link-regexp blink)
		 (match-string-no-properties 1 blink)))
	 (desc (or (and (string-match org-bracket-link-regexp blink)
			(match-string-no-properties 3 blink)) "No description"))
	 (files (org-registry-assoc-all link))
	 file point selection tmphist)
    (cond ((and files visit)
	   ;; result(s) to visit
	   (cond ((< 1 (length files))
		  ;; more than one result
		  (setq tmphist (mapcar (lambda(entry)
					  (format "%s (%d) [%s]"
						  (nth 3 entry) ; file
						  (nth 2 entry) ; point
						  (nth 1 entry))) files))
		  (setq selection (completing-read "File: " tmphist
						   nil t nil 'tmphist))
		  (string-match "\\(.+\\) (\\([0-9]+\\))" selection)
		  (setq file (match-string 1 selection))
		  (setq point (string-to-number (match-string 2 selection))))
		 ((eq 1 (length files))
		  ;; just one result
		  (setq file (nth 3 (car files)))
		  (setq point (nth 2 (car files)))))
	   ;; visit the (selected) file
	   (funcall org-registry-find-file file)
	   (goto-char point)
	   (unless (org-before-first-heading-p)
	     (org-show-context)))
	  ((and files (not visit))
	   ;; result(s) to display
	   (cond  ((eq 1 (length files))
		   ;; show one file
		   (message "Link in file %s (%d) [%s]"
			    (nth 3 (car files))
			    (nth 2 (car files))
			    (nth 1 (car files))))
		  (t (org-registry-display-files files link))))
	  (t (message "No link to this in org-agenda-files")))))

(defun org-registry-display-files (files link)
  "Display files in a separate window."
  (switch-to-buffer-other-window
   (get-buffer-create " *Org registry info*"))
  (erase-buffer)
  (insert (format "Files pointing to %s:\n\n" link))
  (let (file)
    (while (setq file (pop files))
      (insert (format "%s (%d) [%s]\n" (nth 3 file)
		      (nth 2 file) (nth 1 file)))))
  (shrink-window-if-larger-than-buffer)
  (other-window 1))

(defun org-registry-assoc-all (link &optional registry)
  "Return all associated entries of LINK in the registry."
  (org-registry-find-all
   (lambda (entry) (string= link (car entry)))
   registry))

(defun org-registry-find-all (test &optional registry)
  "Return all entries satisfying `test' in the registry."
  (delq nil
        (mapcar
         (lambda (x) (and (funcall test x) x))
         (or registry org-registry-alist))))

;;;###autoload
(defun org-registry-visit ()
  "If an Org file contains a link to the current location, visit
this file."
  (interactive)
  (org-registry-show t))

;;;###autoload
(defun org-registry-initialize (&optional from-scratch)
  "Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'."
  (interactive "P")
  (if (or from-scratch (not (file-exists-p org-registry-file)))
      ;; create a new registry
      (let ((files org-agenda-files) file)
	(while (setq file (pop files))
	  (setq file (expand-file-name file))
	  (mapc (lambda (entry)
		  (add-to-list 'org-registry-alist entry))
		(org-registry-get-entries file)))
	(when from-scratch
	  (org-registry-create org-registry-alist)))
    ;; eval the registry file
    (with-temp-buffer
      (insert-file-contents org-registry-file)
      (eval-buffer))))

;;;###autoload
(defun org-registry-insinuate ()
  "Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit."
  (interactive)
  (add-hook 'org-mode-hook
	    (lambda() (add-hook 'after-save-hook
				'org-registry-update t t))))

(defun org-registry-get-entries (file)
  "List Org links in FILE that will be put in the registry."
  (let (bufstr result)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward org-angle-link-re nil t)
	(let* ((point (match-beginning 0))
	       (link (match-string-no-properties 0))
	       (desc (match-string-no-properties 0)))
	    (add-to-list 'result (list link desc point file))))
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
	(let* ((point (match-beginning 0))
	       (link (match-string-no-properties 1))
	       (desc (or (match-string-no-properties 3) "No description")))
	    (add-to-list 'result (list link desc point file)))))
    ;; return the list of new entries
    result))

;;;###autoload
(defun org-registry-update ()
  "Update the registry for the current Org file."
  (interactive)
  (unless (eq major-mode 'org-mode) (error "Not in org-mode"))
  (let* ((from-file (expand-file-name (buffer-file-name)))
	 (new-entries (org-registry-get-entries from-file)))
    (with-temp-buffer
      (unless (file-exists-p org-registry-file)
	(org-registry-initialize t))
      (find-file org-registry-file)
      (goto-char (point-min))
      (while (re-search-forward (concat from-file "\")$") nil t)
	(let ((end (1+ (match-end 0)))
	      (beg (progn (re-search-backward "^(\"" nil t)
			  (match-beginning 0))))
	(delete-region beg end)))
      (goto-char (point-min))
      (re-search-forward "^(\"" nil t)
      (goto-char (match-beginning 0))
      (mapc (lambda (elem)
	      (insert (with-output-to-string (prin1 elem)) "\n"))
	    new-entries)
      (save-buffer)
      (kill-buffer (current-buffer)))
    (message (format "Org registry updated for %s"
		     (file-name-nondirectory from-file)))))

(defun org-registry-create (entries)
  "Create `org-registry-file' with ENTRIES."
  (let (entry)
    (with-temp-buffer
      (find-file org-registry-file)
      (erase-buffer)
      (insert
       (with-output-to-string
	 (princ ";; -*- emacs-lisp -*-\n")
	 (princ ";; Org registry\n")
	 (princ ";; You shouldn't try to modify this buffer manually\n\n")
	 (princ "(setq org-registry-alist\n'(\n")
	 (while entries
	   (when (setq entry (pop entries))
	     (prin1 entry)
	     (princ "\n")))
	 (princ "))\n")))
      (save-buffer)
      (kill-buffer (current-buffer))))
  (message "Org registry created"))

(provide 'org-registry)

;;;  User Options, Variables

;;; org-registry.el ends here
