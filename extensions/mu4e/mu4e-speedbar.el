;;; mu4e-speedbar --- Speedbar support for mu4e

;; Copyright (C) 2012 Antono Vasiljev, Dirk-Jan C. Binnema
;;
;; Author: Antono Vasiljev <self@antono.info>
;; Version: 0.1
;; Keywords: file, tags, tools
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Speedbar provides a frame in which files, and locations in files
;; are displayed.  These functions provide mu4e specific support,
;; showing maildir list in the side-bar.
;;
;;   This file requires speedbar.

;;; Code:

(require 'speedbar)
(require 'mu4e-vars)
(require 'mu4e-headers)

(defvar mu4e-main-speedbar-key-map nil
  "Keymap used when in mu4e display mode.")
(defvar mu4e-headers-speedbar-key-map nil
  "Keymap used when in mu4e display mode.")
(defvar mu4e-view-speedbar-key-map nil
  "Keymap used when in mu4e display mode.")

(defvar mu4e-main-speedbar-menu-items nil
  "Additional menu-items to add to speedbar frame.")
(defvar mu4e-headers-speedbar-menu-items nil
  "Additional menu-items to add to speedbar frame.")
(defvar mu4e-view-speedbar-menu-items nil
  "Additional menu-items to add to speedbar frame.")


(defun mu4e-speedbar-install-variables ()
  "Install those variables used by speedbar to enhance mu4e."
  (dolist (keymap
	    '( mu4e-main-speedbar-key-map
	       mu4e-headers-speedbar-key-map
	       mu4e-view-speedbar-key-map))
    (unless keymap
      (setq keymap (speedbar-make-specialized-keymap))
      (define-key keymap "RET" 'speedbar-edit-line)
      (define-key keymap "e" 'speedbar-edit-line))))


;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
  (mu4e-speedbar-install-variables)
  (add-hook 'speedbar-load-hook 'mu4e-speedbar-install-variables))

(defun mu4e~speedbar-render-maildir-list ()
  "Insert the list of maildirs in the speedbar."
  (interactive)
  (mapcar (lambda (maildir-name)
            (speedbar-insert-button
	      (concat "  " maildir-name)
	      'mu4e-highlight-face
	      'highlight
	      'mu4e~speedbar-maildir
	      maildir-name))
    (mu4e-get-maildirs)))

(defun mu4e~speedbar-maildir (&optional text token ident)
  "Jump to maildir TOKEN. TEXT and INDENT are not used."
  (speedbar-with-attached-buffer
    (mu4e-headers-search (concat "\"maildir:" token "\"")
      current-prefix-arg)))

(defun mu4e~speedbar-render-bookmark-list ()
  "Insert the list of bookmarks in the speedbar"
  (interactive)
  (mapcar (lambda (bookmark)
            (speedbar-insert-button
	      (concat "  " (nth 1 bookmark))
	      'mu4e-highlight-face
	      'highlight
	      'mu4e~speedbar-bookmark
	      (nth 0 bookmark)))
    mu4e-bookmarks))

(defun mu4e~speedbar-bookmark (&optional text token ident)
  "Run bookmarked query TOKEN. TEXT and INDENT are not used."
  (speedbar-with-attached-buffer
    (mu4e-headers-search token current-prefix-arg)))

;;;###autoload
(defun mu4e-speedbar-buttons (buffer)
  "Create buttons for any mu4e BUFFER."
  (interactive)
  (erase-buffer)
  (insert (propertize "* mu4e\n\n" 'face 'mu4e-title-face))

  (insert (propertize " Bookmarks\n" 'face 'mu4e-title-face))
  (mu4e~speedbar-render-bookmark-list)
  (insert "\n")
  (insert (propertize " Maildirs\n" 'face 'mu4e-title-face))
  (mu4e~speedbar-render-maildir-list))

(defun mu4e-main-speedbar-buttons (buffer) (mu4e-speedbar-buttons buffer)) 
(defun mu4e-headers-speedbar-buttons (buffer) (mu4e-speedbar-buttons buffer)) 
(defun mu4e-view-speedbar-buttons (buffer) (mu4e-speedbar-buttons buffer)) 


(provide 'mu4e-speedbar)
;;; mu4e-speedbar.el ends here
