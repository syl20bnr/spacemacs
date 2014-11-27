;;; org-repo-todo.el --- Simple repository todo management with org-mode

;; Copyright (C) 2014  justin talbott

;; Author: justin talbott <justin@waymondo.com>
;; Keywords: convenience
;; URL: https://github.com/waymondo/org-repo-todo
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple package for capturing and visiting todo items for
;; the repository you are currently within. Under the hood it uses
;; `org-capture' to provide a popup window for inputting `org-mode'
;; checkboxed todo items (http://orgmode.org/manual/Checkboxes.html)
;; or regular *TODO items that get saved to a TODO.org file in the
;; root of the repository.
;;
;; Install is as easy as dropping this file into your load path and setting
;; the relevent functions to keybindings of your choice, i.e.:
;;
;;   (global-set-key (kbd "C-;") 'ort/capture-todo)
;;   (global-set-key (kbd "C-`") 'ort/capture-todo-check)
;;   (global-set-key (kbd "C-'") 'ort/goto-todos)
;;
;; Using the `C-u' command prefix with either of these commands will
;; capture/visit the todos in your `user-emacs-directory' instead.

;;; Code:

(require 'cl)
(require 'org-capture)

(defvar ort/todo-root)

(autoload 'vc-git-root "vc-git")
(autoload 'vc-svn-root "vc-svn")
(autoload 'vc-hg-root "vc-hg")

(push '("ort" "Org Repo Todo"
        entry
        (file+headline (ort/todo-file) "Tasks")
        "* TODO  %?\t\t\t%T\n %i\n Link: %l\n")
      org-capture-templates)

(push '("ortcheck" "Org Repo Todo Checklist" 
        checkitem
        (file+headline (ort/todo-file) "Checklist"))
      org-capture-templates)

(defun ort/todo-file ()
  (concat ort/todo-root "TODO.org"))

(defun ort/find-root (&optional dotemacs)
  "Find the repo root of the current directory.
With the argument DOTEMACS, find your .emacs.d's root folder."
  (let ((ort/dir (if dotemacs user-emacs-directory default-directory)))
    (or (vc-git-root ort/dir)
        (vc-svn-root default-directory)
        (vc-hg-root default-directory)
        ort/dir)))

;;;###autoload
(defun ort/goto-todos (&optional dotemacs)
  "Visit the current repo's TODO.org file.
With the argument DOTEMACS, visit your .emacs.d's TODO.org file."
  (interactive "P")
  (let ((ort/todo-root (ort/find-root dotemacs)))
    (find-file (ort/todo-file))))

;;;###autoload
(defun ort/capture-todo (&optional dotemacs)
  "Capture a todo for the current repo in an `org-capture' popup window.
With the argument DOTEMACS, capture the todo for your .emacs.d's TODO.org file."
  (interactive "P")
  ;; make window split horizontally
  (let ((split-width-threshold nil)
        (split-height-threshold 0)
        (ort/todo-root (ort/find-root dotemacs)))
    (org-capture nil "ort")
    (fit-window-to-buffer nil nil 5)))

;;;###autoload
(defun ort/capture-todo-check (&optional dotemacs)
  "Capture a todo for the current repo in an `org-capture' popup window.
With the argument DOTEMACS, capture the todo for your .emacs.d's TODO.org file."
  (interactive "P")
  ;; make window split horizontally
  (let ((split-width-threshold nil)
        (split-height-threshold 0)
        (ort/todo-root (ort/find-root dotemacs)))
    (org-capture nil "ortcheck")
    (fit-window-to-buffer nil nil 5)))

(provide 'org-repo-todo)
;;; org-repo-todo.el ends here
