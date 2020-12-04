;;; funcs.el --- Space-macs UI Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; ansi-colors

(defun space-macs-visual//compilation-buffer-apply-ansi-colors ()
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (goto-char compilation-filter-start)
      (ansi-color-apply-on-region (line-beginning-position) (point-max)))))


;; popwin

(defun space-macs/remove-popwin-display-config (str)
  "Removes the popwin display configurations that matches the passed STR"
  (setq popwin:special-display-config
        (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                 (string-match str (car x))))
                 popwin:special-display-config)))


;; zoom

(defun space-macs//zoom-frm-powerline-reset ()
  (when (fboundp 'powerline-reset)
    (setq-default powerline-height (space-macs/compute-mode-line-height))
    (powerline-reset)))

(defun space-macs//zoom-frm-do (arg)
  "Perform a zoom action depending on ARG value."
  (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                           ((< arg 0) 'zoom-frm-out)
                           ((> arg 0) 'zoom-frm-in)))
        (fm (cdr (assoc 'fullscreen (frame-parameters))))
        (fwp (* (frame-char-width) (frame-width)))
        (fhp (* (frame-char-height) (frame-height))))
    (when (equal fm 'maximized)
      (toggle-frame-maximized))
    (funcall zoom-action)
    (set-frame-size nil fwp fhp t)
    (when (equal fm 'maximized)
      (toggle-frame-maximized))))

(defun space-macs/zoom-frm-in ()
  "zoom in frame, but keep the same pixel size"
  (interactive)
  (space-macs//zoom-frm-do 1)
  (space-macs//zoom-frm-powerline-reset))

(defun space-macs/zoom-frm-out ()
  "zoom out frame, but keep the same pixel size"
  (interactive)
  (space-macs//zoom-frm-do -1)
  (space-macs//zoom-frm-powerline-reset))

(defun space-macs/zoom-frm-unzoom ()
  "Unzoom current frame, keeping the same pixel size"
  (interactive)
  (space-macs//zoom-frm-do 0)
  (space-macs//zoom-frm-powerline-reset))


