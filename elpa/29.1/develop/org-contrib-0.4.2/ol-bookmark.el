;;; ol-bookmark.el --- Links to bookmarks

;; Copyright (C) 2008-2021 Free Software Foundation, Inc.
;;
;; Author: Tokuya Kameshima <kames AT fa2.so-net.ne.jp>
;; Version: 1.0
;; Keywords: outlines, hypermedia, calendar, wp
;;
;; This file is not part of GNU Emacs.
;;
;; Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'bookmark)
(require 'ol)

(defgroup org-bookmark nil
  "Options concerning the bookmark link."
  :tag "Org Startup"
  :group 'org-link)

(defcustom org-bookmark-in-dired nil
  "Use org-bookmark in dired."
  :group 'org-bookmark
  :type 'boolean)

(defcustom org-bookmark-when-visiting-a-file nil
  "Use org-bookmark in any buffer visiting a file."
  :group 'org-bookmark
  :type 'boolean)

(defcustom org-bookmark-use-first-bookmark nil
  "If several bookmarks links to the buffer, take the first one.
Otherwise prompt the user for the right bookmark to use."
  :group 'org-bookmark
  :type 'boolean)

(org-link-set-parameters "bookmark"
			 :follow #'org-bookmark-open
			 :store #'org-bookmark-store-link)

(defun org-bookmark-open (bookmark _)
  "Visit the bookmark BOOKMARK."
  (bookmark-jump bookmark))

(defun org-bookmark-store-link ()
  "Store a link to the current line's bookmark in bookmark list."
  (let (file bookmark bmks)
    (cond ((and org-bookmark-in-dired
		(eq major-mode 'dired-mode))
	   (setq file (abbreviate-file-name (dired-get-filename))))
	  ((and org-bookmark-when-visiting-a-file
		(buffer-file-name (buffer-base-buffer)))
	   (setq file (abbreviate-file-name
		       (buffer-file-name (buffer-base-buffer))))))
    (if (not file)
	(when (eq major-mode 'bookmark-bmenu-mode)
	  (setq bookmark (bookmark-bmenu-bookmark)))
      (when (and (setq bmks
		       (mapcar (lambda (name)
				 (if (equal file
					    (abbreviate-file-name
					     (bookmark-location name)))
				     name))
			       (bookmark-all-names)))
		 (setq bmks (delete nil bmks)))
	(setq bookmark
	      (if (or (eq 1 (length bmks)) org-bookmark-use-first-bookmark)
		  (car bmks)
		(completing-read "Bookmark: " bmks nil t nil nil (car bmks))))))
    (if bookmark
	(org-store-link-props :link (concat "bookmark:" bookmark)
			      :description bookmark))))

(provide 'ol-bookmark)

;;; ol-bookmark.el ends here
