;;; funcs.el --- Org Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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



(defun space-macs/ob-fix-inline-images ()
  "Fix redisplay of inline images after a code block evaluation."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))



(defun space-macs//surround-drawer ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format ":%s:" (upcase (or dname ""))) ":END:")))

(defun space-macs//surround-code ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))



(defun space-macs//evil-org-mode ()
  (evil-org-mode)
  (evil-normalize-keymaps))

(defun space-macs/org-setup-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-pairs-alist '(?: . space-macs//surround-drawer))
    (add-to-list 'evil-surround-pairs-alist '(?# . space-macs//surround-code))))

(defun space-macs//org-maybe-activate-evil-insert (&rest _)
  "Switch to evil insert state if the current state is normal.
Useful as an :after advice for commands that insert something
into buffer, but are not Evil-aware (e.g. `org-insert-item')."
  (when (and (member dotspace-macs-editing-style '(vim hybrid))
             (evil-normal-state-p))
    (evil-insert-state)))



(defun space-macs/org-trello-pull-buffer ()
  (interactive)
  (org-trello-sync-buffer 1))

(defun space-macs/org-trello-push-buffer ()
  (interactive)
  (org-trello-sync-buffer))

(defun space-macs/org-trello-pull-card ()
  (interactive)
  (org-trello-sync-card 1))

(defun space-macs/org-trello-push-card ()
  (interactive)
  (org-trello-sync-card))

(defun space-macs/org-clock-jump-to-current-clock ()
  (interactive)
  (org-clock-jump-to-current-clock))



(defun space-macs/org-reveal-advice (&rest _args)
  "Unfold the org headings for a target line.
This can be used to advice functions that might open .org files.

For example: To unfold from a magit diff buffer, evaluate the following:
(advice-add 'magit-diff-visit-file :after #'space-macs/org-reveal-advice)"
  (when (derived-mode-p 'org-mode)
    (org-reveal)))


