;;; funcs.el --- EXWM Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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

(defun exwm/exwm-bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil
                                                        ,command)))
    (setq key (pop bindings)
          command
          (pop bindings))))

(defun exwm/exwm-workspace-next ()
  "Switch to next exwm-workspace (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index (1- exwm-workspace-number))))
    (cond
     (only-workspace? nil)
     (overflow? (when exwm-workspace-switch-wrap
                  (exwm-workspace-switch 0)))
     (t (exwm-workspace-switch (1+ exwm-workspace-current-index))))))

(defun exwm/exwm-workspace-prev ()
  "Switch to next exwm-workspace (to the left)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index 0)))
    (cond
     (only-workspace? nil)
     (overflow? (when exwm-workspace-switch-wrap
                  (exwm-workspace-switch (1- exwm-workspace-number))))
     (t (exwm-workspace-switch (1- exwm-workspace-current-index))))))

;; Quick swtiching between workspaces
(defvar exwm--toggle-workspace 0 "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

(defun exwm/jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm--toggle-workspace))

(defadvice exwm-workspace-switch
    (before save-toggle-workspace activate)
  (setq exwm--toggle-workspace exwm-workspace-current-index))

(defun exwm/exwm-app-launcher ()
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm"
  (interactive)
  (call-interactively
   (if (configuration-layer/package-usedp 'helm)
       'helm-run-external-command
     'async-shell-command)))

(defun exwm/exwm-lock ()
  (interactive)
  (start-process "" nil exwm-locking-command))

;; Other utilities
(defun exwm//flatenum (i ls)
  (if ls (cons i (cons (car ls) (exwm//flatenum  (1+ i) (cdr ls)))) (list)))
