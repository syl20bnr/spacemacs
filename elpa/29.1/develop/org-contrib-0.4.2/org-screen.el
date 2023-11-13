;;; org-screen.el --- Integreate Org-mode with screen.

;; Copyright (c) 2008-2014, 2021 Andrew Hyatt
;;
;; Author: Andrew Hyatt <ahyatt at gmail dot com>
;; Maintainer: Carsten Dominik <carsten.dominik@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;; This file contains functionality to integrate screen and org-mode.
;; When using org-mode, it is often useful to take tasks that have
;; some command-line work associated with them, and associate them
;; with a screen session.  Screen is used rather than a direct
;; terminal to facilitate portability of the resulting session.
;;
;; To use screen in org, in your .emacs file, simply put this file in
;; a directory in your load-path and write:
;;
;; (require 'org-screen)
;;
;; When have a task and want to start some command-line activity
;; associated with that task, go to the end of your item and type:
;;
;; M-x org-screen
;;
;; This will prompt you for a name of a screen session.  Type in a
;; name and it will insert a link into your org file at your current
;; location.
;;
;; When you want to visit the link, go to the link and type C-c C-o to
;; open the link.
;;
;; You may want to get rid of the constant queries about whether you
;; really want to execute lisp code.  Do so by adding to your .emacs:
;;
;; (setq org-confirm-elisp-link-function nil)

(require 'term)
(require 'org)

(defcustom org-screen-program-name "/usr/bin/screen"
  "Full location of the screen executable."
  :group 'org-screen
  :type 'string)

(defun org-screen (name)
  "Start a screen session with name"
  (interactive "MScreen name: ")
  (save-excursion
    (org-screen-helper name "-S"))
  (insert (concat "[[screen:" name "]]")))

(defun org-screen-buffer-name (name)
  "Returns the buffer name corresponding to the screen name given."
  (concat "*screen " name "*"))

(defun org-screen-helper (name arg)
  "This method will create a screen session with a specified name
and taking the specified screen arguments.  Much of this function
is copied from ansi-term method."

  ;; Pick the name of the new buffer.
  (let ((term-ansi-buffer-name
        (generate-new-buffer-name
         (org-screen-buffer-name name))))
    (setq term-ansi-buffer-name
          (term-ansi-make-term
          term-ansi-buffer-name org-screen-program-name nil arg name))
    (set-buffer term-ansi-buffer-name)
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    term-ansi-buffer-name))

(defun org-screen-goto (name)
  "Open the screen with the specified name in the window"
  (interactive "MScreen name: ")
  (let ((screen-buffer-name (org-screen-buffer-name name)))
    (if (member screen-buffer-name
                (mapcar 'buffer-name (buffer-list)))
        (org-pop-to-buffer-same-window screen-buffer-name)
      (org-pop-to-buffer-same-window (org-screen-helper name "-dr")))))

(if org-link-abbrev-alist
    (add-to-list 'org-link-abbrev-alist
		 '("screen" . "elisp:(org-screen-goto \"%s\")"))
  (setq org-link-abbrev-alist
	'(("screen" . "elisp:(org-screen-goto \"%s\")"))))

(provide 'org-screen)
