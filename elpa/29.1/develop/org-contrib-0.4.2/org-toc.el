;;; org-toc.el --- Table of contents for Org-mode buffer

;; Copyright 2007-2021 Free Software Foundation, Inc.
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;; Keywords: org, toc
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 0.8

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a browsable table of contents for Org files.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-toc)

;;; Code:

(provide 'org-toc)
(eval-when-compile
  (require 'cl))

;;; Custom variables:
(defvar org-toc-base-buffer nil)
(defvar org-toc-columns-shown nil)
(defvar org-toc-odd-levels-only nil)
(defvar org-toc-config-alist nil)
(defvar org-toc-cycle-global-status nil)
(defalias 'org-show-table-of-contents 'org-toc-show)

(defgroup org-toc nil
  "Options concerning the browsable table of contents of Org-mode."
  :tag "Org TOC"
  :group 'org)

(defcustom org-toc-default-depth 1
  "Default depth when invoking `org-toc-show' without argument."
  :group 'org-toc
  :type '(choice
	  (const :tag "same as base buffer" nil)
	  (integer :tag "level")))

(defcustom org-toc-follow-mode nil
  "Non-nil means navigating through the table of contents will
move the point in the Org buffer accordingly."
  :group 'org-toc
  :type 'boolean)

(defcustom org-toc-info-mode nil
  "Non-nil means navigating through the table of contents will
show the properties for the current headline in the echo-area."
  :group 'org-toc
  :type 'boolean)

(defcustom org-toc-show-subtree-mode nil
  "Non-nil means show subtree when going to headline or following
it while browsing the table of contents."
  :group 'org-toc
  :type '(choice
	  (const :tag "show subtree" t)
	  (const :tag "show entry" nil)))

(defcustom org-toc-recenter-mode t
  "Non-nil means recenter the Org buffer when following the
headlines in the TOC buffer."
  :group 'org-toc
  :type 'boolean)

(defcustom org-toc-recenter 0
  "Where to recenter the Org buffer when unfolding a subtree.
This variable is only used when `org-toc-recenter-mode' is set to
'custom. A value >=1000 will call recenter with no arg."
  :group 'org-toc
  :type 'integer)

(defcustom org-toc-info-exclude '("ALLTAGS")
  "A list of excluded properties when displaying info in the
echo-area. The COLUMNS property is always excluded."
  :group 'org-toc
  :type 'lits)

;;; Org TOC mode:
(defvar org-toc-mode-map (make-sparse-keymap)
  "Keymap for `org-toc-mode'.")

(defun org-toc-mode ()
  "A major mode for browsing the table of contents of an Org buffer.

\\{org-toc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map org-toc-mode-map)
  (setq mode-name "Org TOC")
  (setq major-mode 'org-toc-mode))

;; toggle modes
(define-key org-toc-mode-map "F" 'org-toc-follow-mode)
(define-key org-toc-mode-map "S" 'org-toc-show-subtree-mode)
(define-key org-toc-mode-map "s" 'org-toc-store-config)
(define-key org-toc-mode-map "g" 'org-toc-restore-config)
(define-key org-toc-mode-map "i" 'org-toc-info-mode)
(define-key org-toc-mode-map "r" 'org-toc-recenter-mode)

;; navigation keys
(define-key org-toc-mode-map "p" 'org-toc-previous)
(define-key org-toc-mode-map "n" 'org-toc-next)
(define-key org-toc-mode-map "f" 'org-toc-forward)
(define-key org-toc-mode-map "b" 'org-toc-back)
(define-key org-toc-mode-map [(left)] 'org-toc-back)
(define-key org-toc-mode-map [(right)] 'org-toc-forward)
(define-key org-toc-mode-map [(up)] 'org-toc-previous)
(define-key org-toc-mode-map [(down)] 'org-toc-next)
(define-key org-toc-mode-map "1" (lambda() (interactive) (org-toc-show 1 (point))))
(define-key org-toc-mode-map "2" (lambda() (interactive) (org-toc-show 2 (point))))
(define-key org-toc-mode-map "3" (lambda() (interactive) (org-toc-show 3 (point))))
(define-key org-toc-mode-map "4" (lambda() (interactive) (org-toc-show 4 (point))))
(define-key org-toc-mode-map " " 'org-toc-goto)
(define-key org-toc-mode-map "q" 'org-toc-quit)
(define-key org-toc-mode-map "x" 'org-toc-quit)
;; go to the location and stay in the base buffer
(define-key org-toc-mode-map [(tab)] 'org-toc-jump)
(define-key org-toc-mode-map "v" 'org-toc-jump)
;; go to the location and delete other windows
(define-key org-toc-mode-map [(return)]
  (lambda() (interactive) (org-toc-jump t)))

;; special keys
(define-key org-toc-mode-map "c" 'org-toc-columns)
(define-key org-toc-mode-map "?" 'org-toc-help)
(define-key org-toc-mode-map ":" 'org-toc-cycle-subtree)
(define-key org-toc-mode-map "\C-c\C-o" 'org-open-at-point)
;; global cycling in the base buffer
(define-key org-toc-mode-map (kbd "C-S-<iso-lefttab>")
  'org-toc-cycle-base-buffer)
;; subtree cycling in the base buffer
(define-key org-toc-mode-map [(control tab)]
  (lambda() (interactive) (org-toc-goto nil t)))

;;; Toggle functions:
(defun org-toc-follow-mode ()
  "Toggle follow mode in a `org-toc-mode' buffer."
  (interactive)
  (setq org-toc-follow-mode (not org-toc-follow-mode))
  (message "Follow mode is %s"
	   (if org-toc-follow-mode "on" "off")))

(defun org-toc-info-mode ()
  "Toggle info mode in a `org-toc-mode' buffer."
  (interactive)
  (setq org-toc-info-mode (not org-toc-info-mode))
  (message "Info mode is %s"
	   (if org-toc-info-mode "on" "off")))

(defun org-toc-show-subtree-mode ()
  "Toggle show subtree mode in a `org-toc-mode' buffer."
  (interactive)
  (setq org-toc-show-subtree-mode (not org-toc-show-subtree-mode))
  (message "Show subtree mode is %s"
	   (if org-toc-show-subtree-mode "on" "off")))

(defun org-toc-recenter-mode (&optional line)
  "Toggle recenter mode in a `org-toc-mode' buffer. If LINE is
specified, then make `org-toc-recenter' use this value."
  (interactive "P")
  (setq org-toc-recenter-mode (not org-toc-recenter-mode))
  (when (numberp line)
    (setq org-toc-recenter-mode t)
    (setq org-toc-recenter line))
  (message "Recenter mode is %s"
	   (if org-toc-recenter-mode
	       (format "on, line %d" org-toc-recenter) "off")))

(defun org-toc-cycle-subtree ()
  "Locally cycle a headline through two states: 'children and
'folded"
  (interactive)
  (let ((beg (point))
	(end (save-excursion (end-of-line) (point)))
	(ov (car (overlays-at (point))))
	status)
    (if ov (setq status (overlay-get ov 'status))
      (setq ov (make-overlay beg end)))
    ;; change the folding status of this headline
    (cond ((or (null status) (eq status 'folded))
	   (org-show-children)
	   (message "CHILDREN")
	   (overlay-put ov 'status 'children))
	  ((eq status 'children)
	   (show-branches)
	   (message "BRANCHES")
	   (overlay-put ov 'status 'branches))
	  (t (hide-subtree)
	     (message "FOLDED")
	     (overlay-put ov 'status 'folded)))))

;;; Main show function:
;; FIXME name this org-before-first-heading-p?
(defun org-toc-before-first-heading-p ()
  "Before first heading?"
  (save-excursion
    (null (re-search-backward org-outline-regexp-bol nil t))))

;;;###autoload
(defun org-toc-show (&optional depth position)
  "Show the table of contents of the current Org-mode buffer."
  (interactive "P")
  (if (eq major-mode 'org-mode)
      (progn (setq org-toc-base-buffer (current-buffer))
	     (setq org-toc-odd-levels-only org-odd-levels-only))
    (if (eq major-mode 'org-toc-mode)
	(org-pop-to-buffer-same-window org-toc-base-buffer)
      (error "Not in an Org buffer")))
  ;; create the new window display
  (let ((pos (or position
		 (save-excursion
		   (if (org-toc-before-first-heading-p)
		       (progn (re-search-forward org-outline-regexp-bol nil t)
			      (match-beginning 0))
		     (point))))))
    (setq org-toc-cycle-global-status org-cycle-global-status)
    (delete-other-windows)
    (and (get-buffer "*org-toc*") (kill-buffer "*org-toc*"))
    (switch-to-buffer-other-window
     (make-indirect-buffer org-toc-base-buffer "*org-toc*"))
    ;; make content before 1st headline invisible
    (goto-char (point-min))
    (let* ((beg (point-min))
	   (end (and (re-search-forward "^\\*" nil t)
		     (1- (match-beginning 0))))
	   (ov (make-overlay beg end))
	   (help (format "Table of contents for %s (press ? for a quick help):\n"
			 (buffer-name org-toc-base-buffer))))
      (overlay-put ov 'invisible t)
      (overlay-put ov 'before-string help))
    ;; build the browsable TOC
    (cond (depth
	   (let* ((dpth (if org-toc-odd-levels-only
			    (1- (* depth 2)) depth)))
	     (org-content dpth)
	     (setq org-toc-cycle-global-status
		   `(org-content ,dpth))))
	   ((null org-toc-default-depth)
	    (if (eq org-toc-cycle-global-status 'overview)
		(progn (org-overview)
		       (setq org-cycle-global-status 'overview)
		       (run-hook-with-args 'org-cycle-hook 'overview))
	      (progn (org-overview)
		     ;; FIXME org-content to show only headlines?
		     (org-content)
		     (setq org-cycle-global-status 'contents)
		     (run-hook-with-args 'org-cycle-hook 'contents))))
	   (t (let* ((dpth0 org-toc-default-depth)
		     (dpth (if org-toc-odd-levels-only
			       (1- (* dpth0 2)) dpth0)))
		(org-content dpth)
		(setq org-toc-cycle-global-status
		      `(org-content ,dpth)))))
    (goto-char pos))
  (move-beginning-of-line nil)
  (org-toc-mode)
  (shrink-window-if-larger-than-buffer)
  (setq buffer-read-only t))

;;; Navigation functions:
(defun org-toc-goto (&optional jump cycle)
  "From Org TOC buffer, follow the targeted subtree in the Org window.
If JUMP is non-nil, go to the base buffer.
If JUMP is 'delete, go to the base buffer and delete other windows.
If CYCLE is non-nil, cycle the targeted subtree in the Org window."
  (interactive)
  (let ((pos (point))
	(toc-buf (current-buffer)))
    (switch-to-buffer-other-window org-toc-base-buffer)
    (goto-char pos)
    (if cycle (org-cycle)
      (progn (org-overview)
	     (if org-toc-show-subtree-mode
		 (org-show-subtree)
	       (org-show-entry))
	     (org-show-context)))
    (if org-toc-recenter-mode
	(if (>= org-toc-recenter 1000) (recenter)
	  (recenter org-toc-recenter)))
    (cond ((null jump)
	   (switch-to-buffer-other-window toc-buf))
	  ((eq jump 'delete)
	   (delete-other-windows)))))

(defun org-toc-cycle-base-buffer ()
  "Call `org-cycle' with a prefix argument in the base buffer."
  (interactive)
  (switch-to-buffer-other-window org-toc-base-buffer)
  (org-cycle t)
  (other-window 1))

(defun org-toc-jump (&optional delete)
  "From Org TOC buffer, jump to the targeted subtree in the Org window.
If DELETE is non-nil, delete other windows when in the Org buffer."
  (interactive "P")
  (if delete (org-toc-goto 'delete)
    (org-toc-goto t)))

(defun org-toc-previous ()
  "Go to the previous headline of the TOC."
  (interactive)
  (if (save-excursion
	  (beginning-of-line)
	  (re-search-backward "^\\*" nil t))
    (outline-previous-visible-heading 1)
    (message "No previous heading"))
  (if org-toc-info-mode (org-toc-info))
  (if org-toc-follow-mode (org-toc-goto)))

(defun org-toc-next ()
  "Go to the next headline of the TOC."
  (interactive)
  (outline-next-visible-heading 1)
  (if org-toc-info-mode (org-toc-info))
  (if org-toc-follow-mode (org-toc-goto)))

(defun org-toc-forward ()
  "Go to the next headline at the same level in the TOC."
  (interactive)
  (condition-case nil
      (outline-forward-same-level 1)
    (error (message "No next headline at this level")))
  (if org-toc-info-mode (org-toc-info))
  (if org-toc-follow-mode (org-toc-goto)))

(defun org-toc-back ()
  "Go to the previous headline at the same level in the TOC."
  (interactive)
  (condition-case nil
      (outline-backward-same-level 1)
    (error (message "No previous headline at this level")))
  (if org-toc-info-mode (org-toc-info))
  (if org-toc-follow-mode (org-toc-goto)))

(defun org-toc-quit ()
  "Quit the current Org TOC buffer."
  (interactive)
  (kill-buffer)
  (other-window 1)
  (delete-other-windows))

;;; Special functions:
(defun org-toc-columns ()
  "Toggle columns view in the Org buffer from Org TOC."
  (interactive)
  (let ((indirect-buffer (current-buffer)))
    (org-pop-to-buffer-same-window org-toc-base-buffer)
    (if (not org-toc-columns-shown)
	(progn (org-columns)
	       (setq org-toc-columns-shown t))
      (progn (org-columns-remove-overlays)
	     (setq org-toc-columns-shown nil)))
    (org-pop-to-buffer-same-window indirect-buffer)))

(defun org-toc-info ()
  "Show properties of current subtree in the echo-area."
  (interactive)
  (let ((pos (point))
	(indirect-buffer (current-buffer))
	props prop msg)
    (org-pop-to-buffer-same-window org-toc-base-buffer)
    (goto-char pos)
    (setq props (org-entry-properties))
    (while (setq prop (pop props))
      (unless (or (equal (car prop) "COLUMNS")
		  (member (car prop) org-toc-info-exclude))
	(let ((p (car prop))
	      (v (cdr prop)))
	  (if (equal p "TAGS")
	      (setq v (mapconcat 'identity (split-string v ":" t) " ")))
	  (setq p (concat p ":"))
	  (add-text-properties 0 (length p) '(face org-special-keyword) p)
	  (setq msg (concat msg p " " v "  ")))))
    (org-pop-to-buffer-same-window indirect-buffer)
    (message msg)))

;;; Store and restore TOC configuration:
(defun org-toc-store-config ()
  "Store the current status of the tables of contents in
`org-toc-config-alist'."
  (interactive)
  (let ((file (buffer-file-name org-toc-base-buffer))
	(pos (point))
	(hlcfg (org-toc-get-headlines-status)))
    (setq org-toc-config-alist
	  (delete (assoc file org-toc-config-alist)
		  org-toc-config-alist))
    (add-to-list 'org-toc-config-alist
		 `(,file ,pos ,org-toc-cycle-global-status ,hlcfg))
    (message "TOC configuration saved: (%s)"
	     (if (listp org-toc-cycle-global-status)
		 (concat "org-content "
			 (number-to-string
			  (cadr org-toc-cycle-global-status)))
	       (symbol-name org-toc-cycle-global-status)))))

(defun org-toc-restore-config ()
  "Get the stored status in `org-toc-config-alist' and set the
current table of contents to it."
  (interactive)
  (let* ((file (buffer-file-name org-toc-base-buffer))
	 (conf (cdr (assoc file org-toc-config-alist)))
	 (pos (car conf))
	 (status (cadr conf))
	 (hlcfg (caddr conf)) hlcfg0 ov)
    (cond ((listp status)
	   (org-toc-show (cadr status) (point)))
	  ((eq status 'overview)
	   (org-overview)
	   (setq org-cycle-global-status 'overview)
	   (run-hook-with-args 'org-cycle-hook 'overview))
	  (t
	   (org-overview)
	   (org-content)
	   (setq org-cycle-global-status 'contents)
	   (run-hook-with-args 'org-cycle-hook 'contents)))
    (while (setq hlcfg0 (pop hlcfg))
      (save-excursion
	(goto-char (point-min))
	(when (search-forward (car hlcfg0) nil t)
	  (unless (overlays-at (match-beginning 0))
	    (setq ov (make-overlay (match-beginning 0)
				   (match-end 0))))
	  (cond ((eq (cdr hlcfg0) 'children)
		 (org-show-children)
		 (message "CHILDREN")
		 (overlay-put ov 'status 'children))
		((eq (cdr hlcfg0) 'branches)
		 (show-branches)
		 (message "BRANCHES")
		 (overlay-put ov 'status 'branches))))))
    (goto-char pos)
    (if org-toc-follow-mode (org-toc-goto))
    (message "Last TOC configuration restored")
    (sit-for 1)
    (if org-toc-info-mode (org-toc-info))))

(defun org-toc-get-headlines-status ()
  "Return an alist of headlines and their associated folding
status."
  (let (output ovs)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
		  (goto-char (next-overlay-change (point))))
	(when (looking-at org-outline-regexp-bol)
	  (add-to-list
	   'output
	   (cons (buffer-substring-no-properties
		  (match-beginning 0)
		  (save-excursion
		    (end-of-line) (point)))
		 (overlay-get
		  (car (overlays-at (point))) 'status))))))
    ;; return an alist like (("* Headline" . 'status))
    output))

;; In Org TOC buffer, hide headlines below the first level.
(defun org-toc-help ()
  "Display a quick help message in the echo-area for `org-toc-mode'."
  (interactive)
  (let ((st-start 0)
	(help-message
	 "\[space\]   show heading                     \[1-4\] hide headlines below this level
\[TAB\]     jump to heading                  \[F\]   toggle follow mode (currently %s)
\[return\]  jump and delete others windows   \[i\]   toggle info mode (currently %s)
\[S-TAB\]   cycle subtree (in Org)           \[S\]   toggle show subtree mode (currently %s)
\[C-S-TAB\] global cycle (in Org)            \[r\]   toggle recenter mode (currently %s)
\[:\]       cycle subtree (in TOC)           \[c\]   toggle column view (currently %s)
\[n/p\]     next/previous heading            \[s\]   save TOC configuration
\[f/b\]     next/previous heading of same level
\[q\]       quit the TOC                     \[g\]   restore last TOC configuration"))
    (while (string-match "\\[[^]]+\\]" help-message st-start)
      (add-text-properties (match-beginning 0)
                           (match-end 0) '(face bold) help-message)
      (setq st-start (match-end 0)))
  (message help-message
    (if org-toc-follow-mode "on" "off")
    (if org-toc-info-mode "on" "off")
    (if org-toc-show-subtree-mode "on" "off")
    (if org-toc-recenter-mode (format "on, line %s" org-toc-recenter) "off")
    (if org-toc-columns-shown "on" "off"))))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

;;; org-toc.el ends here
