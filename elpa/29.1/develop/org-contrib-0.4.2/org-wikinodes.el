;;; org-wikinodes.el --- Wiki-like CamelCase links to outline nodes

;; Copyright (C) 2010-2021 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 7.01trans
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'org)
(eval-when-compile
  (require 'cl))

(defgroup org-wikinodes nil
  "Wiki-like CamelCase links words to outline nodes in Org mode."
  :tag "Org WikiNodes"
  :group 'org)

(defconst org-wikinodes-camel-regexp "\\<[A-Z]+[a-z]+[A-Z]+[a-z]+[a-zA-Z]*\\>"
  "Regular expression matching CamelCase words.")

(defcustom org-wikinodes-active t
  "Should CamelCase links be active in the current file?"
  :group 'org-wikinodes
  :type 'boolean)
(put 'org-wikinodes-active 'safe-local-variable 'booleanp)

(defcustom org-wikinodes-scope 'file
  "The scope of searches for wiki targets.
Allowed values are:

file       Search for targets in the current file only
directory  Search for targets in all org files in the current directory"
  :group 'org-wikinodes
  :type '(choice
	  (const :tag "Find targets in current file" file)
	  (const :tag "Find targets in current directory" directory)))

(defcustom org-wikinodes-create-targets 'query
  "Non-nil means create Wiki target when following a wiki link fails.
Allowed values are:

nil     never create node, just throw an error if the target does not exist
query   ask the user what to do
t       create the node in the current buffer
\"file.org\"  create the node in the file \"file.org\", in the same directory

If you are using wiki links across files, you need to set `org-wikinodes-scope'
to `directory'."
  :group 'org-wikinodes
  :type '(choice
	  (const :tag "Never automatically create node" nil)
	  (const :tag "In current file" t)
	  (file  :tag "In one special file\n")
	  (const :tag "Query the user" query)))

;;; Link activation

(defun org-wikinodes-activate-links (limit)
  "Activate CamelCase words as links to Wiki targets."
  (when org-wikinodes-active
    (let (case-fold-search)
      (if (re-search-forward org-wikinodes-camel-regexp limit t)
	  (if (equal (char-after (point-at-bol)) ?*)
	      (progn
		;; in  heading - deactivate flyspell
		(org-remove-flyspell-overlays-in (match-beginning 0)
						 (match-end 0))
		t)
	    ;; this is a wiki link
	    (org-remove-flyspell-overlays-in (match-beginning 0)
					     (match-end 0))
	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'mouse-face 'highlight
				       'face 'org-link
				       'keymap org-mouse-map
				       'help-echo "Wiki Link"))
	    t)))))

;;; Following links and creating non-existing target nodes

(defun org-wikinodes-open-at-point ()
  "Check if the cursor is on a Wiki link and follow the link.

This function goes into `org-open-at-point-functions'."
  (and org-wikinodes-active
       (not (org-at-heading-p))
       (let (case-fold-search) (org-in-regexp org-wikinodes-camel-regexp))
       (progn (org-wikinodes-follow-link (match-string 0)) t)))

(defun org-wikinodes-follow-link (target)
  "Follow a wiki link to TARGET.

This need to be found as an exact headline match, either in the current
buffer, or in any .org file in the current directory, depending on the
variable `org-wikinodes-scope'.

If a target headline is not found, it may be created according to the
setting of `org-wikinodes-create-targets'."
  (if current-prefix-arg (org-wikinodes-clear-directory-targets-cache))
  (let ((create org-wikinodes-create-targets)
	visiting buffer m pos file rpl)
    (setq pos
	  (or (org-find-exact-headline-in-buffer target (current-buffer))
	      (and (eq org-wikinodes-scope 'directory)
		   (setq file (org-wikinodes-which-file
			       target (file-name-directory (buffer-file-name))))
		   (org-find-exact-headline-in-buffer
		    target (or (get-file-buffer file)
			       (find-file-noselect file))))))
    (if pos
	(progn
	  (org-mark-ring-push (point))
	  (org-goto-marker-or-bmk pos)
	  (move-marker pos nil))
      (when (eq create 'query)
	(if (eq org-wikinodes-scope 'directory)
	    (progn
	      (message "Node \"%s\" does not exist.  Should it be created?
\[RET] in this buffer   [TAB] in another file  [q]uit" target)
	      (setq rpl (read-char-exclusive))
	      (cond
	       ((member rpl '(?\C-g ?q)) (error "Abort"))
	       ((equal rpl ?\C-m) (setq create t))
	       ((equal rpl ?\C-i)
		(setq create (file-name-nondirectory
			      (read-file-name "Create in file: "))))
	       (t (error "Invalid selection"))))
	  (if (y-or-n-p (format "Create new node \"%s\" in current buffer? "
				target))
	      (setq create t)
	    (error "Abort"))))

      (cond
       ((not create)
	;; We are not allowed to create the new node
	(error "No match for link to \"%s\"" target))
       ((stringp create)
	;; Make new node in another file
	(org-mark-ring-push (point))
	(org-pop-to-buffer-same-window (find-file-noselect create))
	(goto-char (point-max))
	(or (bolp) (newline))
	(insert "\n* " target "\n")
	(backward-char 1)
	(org-wikinodes-add-target-to-cache target)
	(message "New Wiki target `%s' created in file \"%s\""
		 target create))
       (t
	;; Make new node in current buffer
	(org-mark-ring-push (point))
	(goto-char (point-max))
	(or (bolp) (newline))
	(insert "* " target "\n")
	(backward-char 1)
	(org-wikinodes-add-target-to-cache target)
	(message "New Wiki target `%s' created in current buffer"
		 target))))))

;;; The target cache

(defvar org-wikinodes-directory-targets-cache nil)

(defun org-wikinodes-clear-cache-when-on-target ()
  "When on a headline that is a Wiki target, clear the cache."
  (when (and (org-at-heading-p)
	     (org-in-regexp (format org-complex-heading-regexp-format
				    org-wikinodes-camel-regexp))
	     (org-in-regexp org-wikinodes-camel-regexp))
    (org-wikinodes-clear-directory-targets-cache)
    t))

(defun org-wikinodes-clear-directory-targets-cache ()
  "Clear the cache where to find wiki targets."
  (interactive)
  (setq org-wikinodes-directory-targets-cache nil)
  (message "Wiki target cache cleared, so that it will update when used again"))

(defun org-wikinodes-get-targets ()
  "Return a list of all wiki targets in the current buffer."
  (let ((re (format org-complex-heading-regexp-format
		    org-wikinodes-camel-regexp))
	(case-fold-search nil)
	targets)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (push (match-string-no-properties 4) targets))))
    (nreverse targets)))

(defun org-wikinodes-get-links-for-directory (dir)
  "Return an alist that connects wiki links to files in directory DIR."
  (let ((files (directory-files dir nil "\\`[^.#].*\\.org\\'"))
	(org-inhibit-startup t)
	target-file-alist file visiting m buffer)
    (while (setq file (pop files))
      (setq visiting (org-find-base-buffer-visiting file))
      (setq buffer (or visiting (find-file-noselect file)))
      (with-current-buffer buffer
	(mapc
	 (lambda (target)
	   (setq target-file-alist (cons (cons target file) target-file-alist)))
	 (org-wikinodes-get-targets)))
      (or visiting (kill-buffer buffer)))
    target-file-alist))

(defun org-wikinodes-add-target-to-cache (target &optional file)
  (setq file (or file buffer-file-name (error "No file for new wiki target")))
  (set-text-properties 0 (length target) nil target)
  (let ((dir (file-name-directory (expand-file-name file)))
	a)
    (setq a (assoc dir org-wikinodes-directory-targets-cache))
    (if a
	;; Push the new target onto the existing list
	(push (cons target (expand-file-name file)) (cdr a))
      ;; Call org-wikinodes-which-file so that the cache will be filled
      (org-wikinodes-which-file target dir))))

(defun org-wikinodes-which-file (target &optional directory)
  "Return the file for wiki headline TARGET DIRECTORY.
If there is no such wiki target, return nil."
  (let* ((directory (expand-file-name (or directory default-directory)))
	 (founddir (assoc directory org-wikinodes-directory-targets-cache))
	 (foundfile (cdr (assoc target (cdr founddir)))))
    (or foundfile
	(and (push (cons directory (org-wikinodes-get-links-for-directory directory))
		   org-wikinodes-directory-targets-cache)
	     (cdr (assoc target (cdr (assoc directory
					    org-wikinodes-directory-targets-cache))))))))

;;; Exporting Wiki links

(defvar target)
(defvar target-alist)
(defvar last-section-target)
(defvar org-export-target-aliases)
(defun org-wikinodes-set-wiki-targets-during-export (_)
  (let ((line (buffer-substring (point-at-bol) (point-at-eol)))
	(case-fold-search nil)
	wtarget a)
    (when (string-match (format org-complex-heading-regexp-format
				org-wikinodes-camel-regexp)
			line)
      (setq wtarget (match-string 4 line))
      (push (cons wtarget target) target-alist)
      (setq a (or (assoc last-section-target org-export-target-aliases)
		  (progn
		    (push (list last-section-target)
			  org-export-target-aliases)
		    (car org-export-target-aliases))))
      (push (caar target-alist) (cdr a)))))

(defun org-wikinodes-process-links-for-export (_)
  "Process Wiki links in the export preprocess buffer.
Try to find target matches in the wiki scope and replace CamelCase words
with working links."
  (let ((re org-wikinodes-camel-regexp)
	(case-fold-search nil)
	link file)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (unless (save-match-data
		(or (org-at-heading-p)
		    (org-in-regexp org-bracket-link-regexp)
		    (org-in-regexp org-plain-link-re)
		    (org-in-regexp "<<[^<>]+>>")))
	(setq link (match-string 0))
	(delete-region (match-beginning 0) (match-end 0))
	(save-match-data
	  (cond
	   ((org-find-exact-headline-in-buffer link (current-buffer))
	    ;; Found in current buffer
	    (insert (format "[[*%s][%s]]" link link)))
	   ((eq org-wikinodes-scope 'file)
	    ;; No match in file, and other files are not allowed
	    (insert (format "%s" link)))
	   (t ;; No match for this link
	    (insert (format "%s" link)))))))))

;;; Hook the WikiNode mechanism into Org

;; `C-c C-o' should follow wiki links
(add-hook 'org-open-at-point-functions 'org-wikinodes-open-at-point)

;; `C-c C-c' should clear the cache
(add-hook 'org-ctrl-c-ctrl-c-hook 'org-wikinodes-clear-cache-when-on-target)

;; Make Wiki haeding create additional link names for headlines
(add-hook 'org-export-before-parsing-hook
	  'org-wikinodes-set-wiki-targets-during-export)

;; Turn Wiki links into links the exporter will treat correctly
(add-hook 'org-export-before-parsing-hook
	  'org-wikinodes-process-links-for-export)

;; Activate CamelCase words as part of Org mode font lock

(defun org-wikinodes-add-to-font-lock-keywords ()
  "Add wikinode CamelCase highlighting to `org-font-lock-extra-keywords'."
  (let ((m (member '(org-activate-links) org-font-lock-extra-keywords)))
    (if m (push '(org-wikinodes-activate-links) (cdr m))
      (message "Failed to add wikinodes to `org-font-lock-extra-keywords'."))))

(add-hook 'org-font-lock-set-keywords-hook
	  'org-wikinodes-add-to-font-lock-keywords)

(provide 'org-wikinodes)

;;; org-wikinodes.el ends here
