;;; funcs.el --- Org Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun ort/find-todo-file (&optional directory)
  (let* ((ort/todo-root (or directory (ort/find-root default-directory)))
         (file (ort/todo-file)))
    (when (and (not (file-remote-p file))
               (file-readable-p file))
      file)))

(defun ort/find-all-todo-files ()
  (require 'projectile)
  (delq nil (mapcar 'ort/find-todo-file projectile-known-projects)))

(defun ort/list-project-todos ()
  "List all the TODOs of the current project."
  (interactive)
  (let ((org-agenda-files (list (ort/find-todo-file))))
    (org-todo-list)))

(defun ort/list-all-project-todos ()
  "List all the TODOs of all known projects (excluding remote
projects)."
  (interactive)
  (let ((org-agenda-files (ort/find-all-todo-files)))
    (org-todo-list)))

(defun ort/list-all-todos ()
  "List all the TODOs of all known projects (excluding remote
projects) as well as those from `org-agenda-files'."
  (interactive)
  (let ((org-agenda-files (append (org-agenda-files) (ort/find-all-todo-files))))
    (org-todo-list)))



(defun spacemacs/ob-fix-inline-images ()
  "Fix redisplay of inline images after a code block evaluation."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))



(defun spacemacs//surround-drawer ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format ":%s:" (upcase (or dname ""))) ":END:")))

(defun spacemacs//surround-code ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))
