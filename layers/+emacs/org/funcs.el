;;; funcs.el --- Org Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Autoload space-doc-mode
(autoload 'space-doc-mode "space-doc" nil 'interactive)

(defun org-clocks-prefix ()
  (if org-enable-org-contacts-support
      "clocks/contacts"
    "clocks"))

(defun org-contacts-find-file ()
  "Open first contact file"
  (interactive)
  (if (bound-and-true-p org-contacts-files)
      (find-file (car org-contacts-files))
    (message "No specific org-contacts-files defined. Org-contacts uses all org files.")))



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

(defun spacemacs//org-maybe-activate-evil-insert (&rest _)
  "Switch to evil insert state if the current state is normal.
Useful as an :after advice for commands that insert something
into buffer, but are not Evil-aware (e.g. `org-insert-item')."
  (when (and (member dotspacemacs-editing-style '(vim hybrid))
             (evil-normal-state-p))
    (evil-insert-state)))



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

(defun spacemacs/org-clock-jump-to-current-clock ()
  (interactive)
  (org-clock-jump-to-current-clock))



(defun spacemacs/org-reveal-advice (&rest _args)
  "Unfold the org headings for a target line.
This can be used to advice functions that might open .org files.

For example: To unfold from a magit diff buffer, evaluate the following:
(advice-add 'magit-diff-visit-file :after #'spacemacs/org-reveal-advice)"
  (when (derived-mode-p 'org-mode)
    (org-reveal)))


;; Based on the suggestion here:
;; https://orgmode.org/manual/Breaking-Down-Tasks.html
(defun spacemacs/org-summary-todo-naive-auto (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (org-todo (if (= n-not-done 0) "DONE" "TODO")))

(defun spacemacs/org-summary-todo-semiauto (n-done n-not-done)
  "Prompt to change entry state when the state of the subentries imply it."
  (and (org-get-todo-state) ;; Don't force a todo state if there is none yet
       ;; If either it is in a todo state but should be in a done state
       (if (or (and (org-entry-is-todo-p) (= n-not-done 0))
               ;; or it is in a done state and should be in a todo state
               (and (org-entry-is-done-p) (> n-not-done 0)))
           ;; then prompt to change the state
           (org-todo))))
