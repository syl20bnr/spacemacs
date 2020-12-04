;;; funcs.el --- Space-macs Purpose Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Bar Magal
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; eyebrowse integration

(defun space-macs/window-purpose-sync-eyebrowse ()
  "Synchronize window-purpose layer with eyebrowse.
Set `eyebrowse-new-workspace' value depending on the state of `purpose-mode'."
  (defvar space-macs--window-purpose-eyebrowse-new-workspace
    eyebrowse-new-workspace
    "Internal backup of `eyebrowse-new-workspace'.")
  (require 'window-purpose)
  (if purpose-mode
      (setq eyebrowse-new-workspace #'space-macs//window-purpose-new-workspace)
    (setq eyebrowse-new-workspace
          space-macs--window-purpose-eyebrowse-new-workspace)))

(defun space-macs//window-purpose-new-workspace ()
  "Create a new eyebrowse workspace.
Replacement for `eyebrowse-new-workspace' that handles purpose-dedicated
windows correctly."
  ;; call original `eyebrowse-new-workspace' (partially copied from
  ;; `eyebrowse-switch-to-window-config')
  (cond
   ((stringp space-macs--window-purpose-eyebrowse-new-workspace)
    (switch-to-buffer (get-buffer-create
                       space-macs--window-purpose-eyebrowse-new-workspace)))
   ((functionp space-macs--window-purpose-eyebrowse-new-workspace)
    (funcall space-macs--window-purpose-eyebrowse-new-workspace))
   (t (switch-to-buffer "*scratch*")))

  ;; in case opening the new buffer splitted the frame (e.g.
  ;; `eyebrowse-switch-to-window-config' was called from a purpose-dedicated
  ;; buffer)
  (delete-other-windows))


;; Popwin integration

(defun space-macs/window-purpose-sync-popwin ()
  "Synchronize window-purpose layer with popwin.
Enable or disable advices to popwin, according to the state of `purpose-mode'."
  (require 'window-purpose)
  (if purpose-mode
      (progn
        (ad-enable-advice 'popwin:create-popup-window
                          'before 'window-purpose/save-dedicated-windows)
        (ad-enable-advice 'popwin:create-popup-window
                          'after 'window-purpose/restore-dedicated-windows)
        (ad-update 'popwin:create-popup-window)
        (ad-activate 'popwin:create-popup-window))
    (ad-disable-advice 'popwin:create-popup-window
                       'before 'window-purpose/save-dedicated-windows)
    (ad-disable-advice 'popwin:create-popup-window
                       'after 'window-purpose/restore-dedicated-windows)
    (ad-update 'popwin:create-popup-window)))


