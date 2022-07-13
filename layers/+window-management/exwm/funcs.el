;;; funcs.el --- EXWM Layer packages File for Spacemacs
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



;;;; Workspace

(defun exwm//exwm-workspace-switch (arg)
  ;; arg >= 0 next, arg < 0 previous
  (when-let* ((only-workspace? (> exwm-workspace-number 1)))
    (let ((overflow? (= exwm-workspace-current-index (1- exwm-workspace-number))))
      (if overflow?
          (when exwm-workspace-switch-wrap
            (exwm-workspace-switch (if (natnump arg)
                                       0
                                     (1- exwm-workspace-number))))
        (exwm-workspace-switch (if (natnump arg)
                                   (1+ exwm-workspace-current-index)
                                 (1- exwm-workspace-current-index)))))))

(defun exwm/exwm-workspace-next ()
  "Switch to next exwm-workspace (to the right)."
  (interactive)
  (exwm//exwm-workspace-switch +1))

(defun exwm/exwm-workspace-prev ()
  "Switch to next exwm-workspace (to the left)."
  (interactive)
  (exwm//exwm-workspace-switch -1))

;; Quick swtiching between workspaces
(defvar exwm--toggle-workspace 0 "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

(defun exwm/jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm--toggle-workspace))

(defadvice exwm-workspace-switch
    (before save-toggle-workspace activate)
  (setq exwm--toggle-workspace exwm-workspace-current-index))


;;;; Misc

(defun exwm/terminal-launcher ()
  (interactive)
  (if exwm-terminal-command
      (start-process-shell-command exwm-terminal-command nil exwm-terminal-command)
    (user-error "`exwm-terminal-command' is nil")))

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
  (if exwm-locking-command
      (start-process "" nil exwm-locking-command)
    (user-error "`exwm-locking-command' is nil")))


;;;; Autostart

(defvar exwm--autostart-process-list nil
  "List of processes to run during autostart.
This is a list of strings used as shell commands.")

(defun exwm//autostart-process (name command &optional directory)
  "Start a program in subprocess and append it to `exwm--autostart-process-list'.
NAME is the name for process.
COMMAND is the shell command to run.
DIRECTORY is the working directory in which the process is run.  It defaults to
  `user-home-directory' if not provided.
The started process is also added to `exwm--autostart-process-list'."
  (add-to-list (let ((default-directory (or directory user-home-directory)))
                   (start-process-shell-command name nil command))
               exwm--autostart-process-list))

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
              (directory (or (gethash "Path" xdg) user-home-directory))
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
  (cl-loop with autostart-dirs    =  nil
           with autostart-entries =  nil
           for  xdg-dir           in (cons (xdg-config-home) (xdg-config-dirs))
           for  autostart-dir     =  (expand-file-name (concat xdg-dir "/autostart"))
           when (and (file-directory-p autostart-dir)
                     (not (member autostart-dir autostart-dirs)))
             collect autostart-dir into autostart-dirs
             and nconc (mapcar #'xdg-desktop-read-file
                               (directory-files autostart-dir t ".+\\.desktop$")) into autostart-entries
           finally return autostart-entries))

(defun exwm//autostart-xdg-applications ()
  "Run the autostart applications as defined by the freedesktop autostart specification."
  (unless exwm--autostart-process-list
    (mapc #'exwm//start-desktop-application (exwm//read-xdg-autostart-files))))

;;;; Randr
(defun exwm//randr-setup ()
  "Setup `exwm-randr'."
  (require 'exwm-randr)
  (when exwm-randr-command
    (start-process-shell-command
     "xrandr" nil exwm-randr-command))
  ;; The first workspaces will match the order in RandR
  (setq exwm-randr-workspace-monitor-plist exwm-randr-command)
  (exwm-randr-enable))


;;;; Systray
(defun exwm//systray-setup ()
  "Setup `exwm-systray'."
  (when exwm-enable-systray
    (require 'exwm-systemtray)
    (exwm-systemtray-enable)))
