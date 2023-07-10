;;; funcs.el --- Vinegar Layer Functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defun vinegar/dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (setq-local dired-dotfiles-show-p nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (setq-local dired-dotfiles-show-p t)))))

(defun vinegar/back-to-top ()
  "Move to first file"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 1))

(defun vinegar/jump-to-bottom ()
  "Move to last file"
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun vinegar/move-up ()
  "Move to previous file"
  (interactive)
  (dired-previous-line 1)
  (if (bobp)
      (dired-next-line 1)))

(defun vinegar/move-down ()
  "Move to next file"
  (interactive)
  (dired-next-line 1)
  (if (eobp)
      (dired-next-line -1)))

(defun vinegar/up-directory (&optional other-window)
  "Run Dired on parent directory of current directory."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (orig (current-buffer))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (kill-buffer orig)
          (dired up)
          (dired-goto-file dir)))))

(defun vinegar/dired-diff ()
  "Ediff marked files in dired or selected files in separate window"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files)
                        (nth 1 marked-files)))
          ((= (length marked-files) 3)
           (ediff-files3 (nth 0 marked-files)
                         (nth 1 marked-files)
                         (nth 2 marked-files)))

          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          ((= (length marked-files) 1)
           (dired-diff))
          (t (error "mark exactly 2 files, at least 1 locally")))))


(defun vinegar/dired-setup ()
  "Setup custom dired settings for vinegar"
  (setq dired-omit-verbose nil)
  (make-local-variable 'dired-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil)

  ;; hide details by default
  (if vinegar-dired-hide-details (dired-hide-details-mode t))
  ;; omit the .. in dired
  (dired-omit-mode t)

  ;; allow selection with mouse
  (make-local-variable 'mouse-1-click-follows-link)
  (setq mouse-1-click-follows-link nil)

  (local-set-key (kbd  "<mouse-1>") 'vinegar/dired-mouse-click)
  (local-set-key (kbd  "<mouse-3>") 'vinegar/up-directory)
  (local-set-key (kbd  "<down-mouse-3>") nil))


(defun vinegar/dired-mouse-click (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (find-alternate-file file)))

(defun vinegar/dired-mouse-click-3 (event)
  "In Dired, show context menu or go up a directory."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (condition-case-unless-debug ex
          (progn
            (setq file (dired-get-file-for-visit))
            (dired-find-file-other-window))
        ('error
         (vinegar/up-directory))))))


;; )
