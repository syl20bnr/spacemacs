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


(defvar exwm//autostart-process-list nil
  "List of processes run during autostart.")

(defun exwm/autostart-process (name command &optional directory)
  "Can be used during initialization to run COMMAND as a process
  with NAME and add it to the list of autostarted processes.

DIRECTORY can be set to a string which will be used as working directory for the
process.  If not supplied, will be set to `user-home-directory'.
"
  (push (let ((default-directory (or directory user-home-directory)))
          (start-process-shell-command name nil command))
        exwm//autostart-process-list))

(defun exwm//start-desktop-application (basename xdg)
  "Autostart an application from a XDG desktop entry specification."
  (let* ((type (gethash "Type" xdg))
         ;; (name (gethash "Name" xdg))
         (cmd (gethash "Exec" xdg))
         (hidden (gethash "Hidden" xdg))
         (include (gethash "OnlyShowIn" xdg))
         (exclude (gethash "NotShowIn" xdg))
         (try-exec (gethash "TryExec" xdg))
         (exec-directory (gethash "Path" xdg))
         ;; (dbus-p (gethash "DBusActivatable" xdg)) ; TODO: support
         (included-p (cond (include (member "EXWM" (split-string include ";" t)))
                           (exclude (not (member "EXWM" (split-string exclude ";" t))))
                           (t)))
         (should-exec-p (and
                         (string= type "Application")
                         included-p
                         (if try-exec
                             (executable-find try-exec)
                           t))))
    (when should-exec-p (exwm/autostart-process basename cmd exec-directory))))

(defun exwm//read-xdg-autostart-files ()
  "Return a hashtable for autostart applications as defined by the freedesktop specification."
  (cl-loop with xdg-specs = (make-hash-table :test 'equal)
           for dir in (append (xdg-config-dirs) (list (xdg-config-home)))
           for autostart-dir = (expand-file-name "autostart/" dir)
           for autostart-files = (when (file-exists-p autostart-dir)
                                   (directory-files autostart-dir t (rx (1+ word) ".desktop")))
           do
           (cl-loop for file in autostart-files do
                    (setf (gethash (file-name-base file) xdg-specs) (xdg-desktop-read-file file)))
           finally (return xdg-specs)))

(defun exwm//autostart-xdg-applications ()
  "Run the autostart applications as defined by the freedesktop autostart specification."
  (unless exwm//autostart-process-list
    (cl-loop for basename being the hash-keys of
             (exwm//read-xdg-autostart-files)
             using (hash-values xdg) do
             (exwm//start-desktop-application basename xdg))))

(defun exwm//kill-autostart-processes ()
  (cl-loop for p in exwm//autostart-process-list do
           (when (process-live-p p) (kill-process p)))
  (setq exwm//autostart-process-list nil))

;; Other utilities
(defun exwm//flatenum (i ls)
  (if ls (cons i (cons (car ls) (exwm//flatenum  (1+ i) (cdr ls)))) (list)))
