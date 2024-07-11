;;; funcs.el --- Spacemacs Purpose Layer functions File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Bar Magal
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



;; eyebrowse integration

(defun spacemacs/window-purpose-sync-eyebrowse ()
  "Synchronize window-purpose layer with eyebrowse.
Set `eyebrowse-new-workspace' value depending on the state of `purpose-mode'."
  (defvar spacemacs--window-purpose-eyebrowse-new-workspace
    eyebrowse-new-workspace
    "Internal backup of `eyebrowse-new-workspace'.")
  (require 'window-purpose)
  (if purpose-mode
      (setq eyebrowse-new-workspace #'spacemacs//window-purpose-new-workspace)
    (setq eyebrowse-new-workspace
          spacemacs--window-purpose-eyebrowse-new-workspace)))

(defun spacemacs//window-purpose-new-workspace ()
  "Create a new eyebrowse workspace.
Replacement for `eyebrowse-new-workspace' that handles purpose-dedicated
windows correctly."
  ;; call original `eyebrowse-new-workspace' (partially copied from
  ;; `eyebrowse-switch-to-window-config')
  (cond
   ((stringp spacemacs--window-purpose-eyebrowse-new-workspace)
    (switch-to-buffer (get-buffer-create
                       spacemacs--window-purpose-eyebrowse-new-workspace)))
   ((functionp spacemacs--window-purpose-eyebrowse-new-workspace)
    (funcall spacemacs--window-purpose-eyebrowse-new-workspace))
   (t (switch-to-buffer "*scratch*")))

  ;; in case opening the new buffer splitted the frame (e.g.
  ;; `eyebrowse-switch-to-window-config' was called from a purpose-dedicated
  ;; buffer)
  (delete-other-windows))


;; Popwin integration

(defun spacemacs/window-purpose-save-dedicated-windows (&rest args)
  "Saves the dedicated windows before popwin:create-popup-windows"

  ;; save the dedicated windows
  (setq window-purpose--dedicated-windows
        (cl-loop for window in (window-list)
                 if (purpose-window-purpose-dedicated-p window)
                 collect (window-buffer window))))

(defun spacemacs/window-purpose-restore-dedicated-windows (&rest args)
  "Restores the dedicated windows after popwin:create-popup-windows"
  (cl-loop for buffer in window-purpose--dedicated-windows
           do (cl-loop for window in (get-buffer-window-list buffer)
                       do (purpose-set-window-purpose-dedicated-p
                           window t))))

(defun spacemacs/window-purpose-sync-popwin ()
  "Synchronize window-purpose layer with popwin.
Enable or disable advices to popwin, according to the state of `purpose-mode'."
  (require 'window-purpose)
  (if purpose-mode
      (progn
        (advice-add #'popwin:create-popup-window
                    :before #'spacemacs/window-purpose-save-dedicated-windows)
        (advice-add #'popwin:create-popup-window
                    :after #'spacemacs/window-purpose-restore-dedicated-windows))

    (advice-remove #'popwin:create-popup-window
                   #'spacemacs/window-purpose-save-dedicated-windows)
    (advice-remove #'popwin:create-popup-window
                   #'spacemacs/window-purpose-restore-dedicated-windows)))
