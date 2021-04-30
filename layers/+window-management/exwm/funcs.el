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
  "Launch an application in PATH.
Optionally, this has helm integration when helm is enabled."
  (interactive)
  (call-interactively
   (if (configuration-layer/package-usedp 'helm)
       'helm-run-external-command
     'async-shell-command)))

(defun exwm/exwm-lock ()
  (interactive)
  (start-process "" nil exwm-locking-command))

(defun exwm//autostart-process (name command &optional directory)
  "Start a program in subprocess and append it to `exwm-autostart-process-list'.
NAME is the name for process.
COMMAND is the shell command to run.
DIRECTORY is the working directory in which the process is run.  It defaults to
  `user-home-directory' if not provided.
The started process is also added to `exwm-autostart-process-list'."
  (add-to-list (let ((default-directory (or directory user-home-directory)))
                   (start-process-shell-command name nil command))
               exwm-autostart-process-list))

(defun exwm//start-desktop-application (xdg)
  "Autostart an application from a XDG desktop entry specification."
  (when-let* ((type (gethash "Type" xdg))
              (_application? (string= type "Application"))
              ;; Note that we can't always assume the presence of Exec: when
              ;; DBusActivatable support is added, the application is launched
              ;; through D-Bus rather than a program.
              (basename (gethash "Name" xdg))
              (cmd (gethash "Exec" xdg))
                            ;; (dbus-p (gethash "DBusActivatable" xdg)) ; TODO: support
              (directory (gethash "Path" xdg) user-home-directory)
              (_include? (and (null (gethash "Hidden" xdg))
                              (if-let ((only-show (gethash "OnlyShowIn" xdg)))
                                  (member "EXWM" (split-string only-show ";" t))
                                t)
                              (if-let ((not-show (gethash "NotShowIn" xdg)))
                                  (not (member "EXWM" (split-string not-show ";" t)))
                                t)
                              (if-let ((try-exec (gethash "TryExec" xdg)))
                                  (executable-find try-exec)
                                t))))
    (exwm//autostart-process basename cmd directory)))

(defun exwm//read-xdg-autostart-files ()
  "Return the list of autostart applications."
  (cl-loop for dir in (append (xdg-config-dirs) (list (xdg-config-home)))
           for autostart-dir = (concat dir "/autostart")
           for file in (when (file-exists-p autostart-dir)
                         (directory-files autostart-dir t ".+\\.desktop$"))
           (collect (xdg-desktop-read-file file))))

(defun exwm//autostart-xdg-applications ()
  "Run the autostart applications as defined by the freedesktop autostart specification."
  (unless exwm-autostart-process-list
    (mapc #'exwm//start-desktop-application (exwm//read-xdg-autostart-files))))

;; Other utilities
(defun exwm//flatenum (i ls)
  (if ls (cons i (cons (car ls) (exwm//flatenum  (1+ i) (cdr ls)))) (list)))
