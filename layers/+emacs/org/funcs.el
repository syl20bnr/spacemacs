;;; funcs.el --- Org Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
      (org-projectile-project-todo-completing-read :empty-lines 1)
    (org-projectile-capture-for-current-project :empty-lines 1)))

(defun org-projectile/goto-todos ()
  (interactive)
  (org-projectile-goto-location-for-project (projectile-project-name)))



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



(defun spacemacs//evil-org-mode ()
  (evil-org-mode)
  (evil-normalize-keymaps))

(defun spacemacs/org-setup-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-pairs-alist '(?: . spacemacs//surround-drawer))
    (add-to-list 'evil-surround-pairs-alist '(?# . spacemacs//surround-code))))



(defun spacemacs/org-trello-pull-buffer ()
  (interactive)
  (org-trello-sync-buffer 1))

(defun spacemacs/org-trello-push-buffer ()
  (interactive)
  (org-trello-sync-buffer))

(defun spacemacs/org-trello-pull-card ()
  (interactive)
  (org-trello-sync-card 1))

(defun spacemacs/org-trello-push-card ()
  (interactive)
  (org-trello-sync-card))
