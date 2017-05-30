;;; funcs.el --- Org Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Autoload space-doc-mode
(autoload 'space-doc-mode "space-doc" nil 'interactive)

(defun org-projectile/capture (&optional arg)
  (interactive "P")
  (if arg
      (org-projectile:project-todo-completing-read nil :empty-lines 1)
    (org-projectile:capture-for-current-project nil :empty-lines 1)))

(defun org-projectile/goto-todos ()
  (interactive)
  (org-projectile:location-for-project (projectile-project-name)))



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



(defun spacemacs/org-smart-open-line-below ()
  "Insert new item if already in an item, otherwise, open line."
  (interactive)
  (end-of-visible-line)
  (if (not (org-in-item-p))
      (evil-open-below nil)
    (org-insert-item)
    (evil-append nil)))

(defun spacemacs/org-smart-open-line-above ()
  "Insert new item if already in an item, otherwise, open line."
  (interactive)
  (end-of-visible-line)
  (if (not (org-in-item-p))
      (evil-open-above nil)
    (org-insert-item)
    (org-move-item-up)
    (evil-append nil)))

(defun spacemacs/org-insert-heading-below ()
  "Insert a new heading below."
  (interactive)
  (org-insert-heading-respect-content)
  (evil-append nil))

(defun spacemacs/org-insert-heading-above ()
  "Insert a new heading above."
  (interactive)
  (org-insert-heading-respect-content)
  (org-metaup)
  (evil-append nil))

(defun spacemacs/org-insert-todo-heading-below ()
  "Insert a new heading below."
  (interactive)
  (org-insert-todo-heading-respect-content)
  (evil-append nil))

(defun spacemacs/org-insert-todo-heading-above ()
  "Insert a new heading above."
  (interactive)
  (org-insert-todo-heading-respect-content)
  (org-metaup)
  (evil-append nil))
