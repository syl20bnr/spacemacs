;;; funcs.el --- Spacemacs UI Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; ansi-colors

(defun spacemacs-visual//compilation-buffer-apply-ansi-colors ()
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (goto-char compilation-filter-start)
      (ansi-color-apply-on-region (line-beginning-position) (point-max)))))


;; popwin

(defun spacemacs/remove-popwin-display-config (str)
  "Removes the popwin display configurations that matches the passed STR"
  (setq popwin:special-display-config
        (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                 (string-match str (car x))))
                 popwin:special-display-config)))


;; zoom

(defun spacemacs//zoom-frm-powerline-reset ()
  (when (fboundp 'powerline-reset)
    (setq-default powerline-height (spacemacs/compute-mode-line-height))
    (powerline-reset)))

(defun spacemacs//zoom-frm-do (arg)
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

(defun spacemacs/zoom-frm-in ()
  "zoom in frame, but keep the same pixel size"
  (interactive)
  (spacemacs//zoom-frm-do 1)
  (spacemacs//zoom-frm-powerline-reset))

(defun spacemacs/zoom-frm-out ()
  "zoom out frame, but keep the same pixel size"
  (interactive)
  (spacemacs//zoom-frm-do -1)
  (spacemacs//zoom-frm-powerline-reset))

(defun spacemacs/zoom-frm-unzoom ()
  "Unzoom current frame, keeping the same pixel size"
  (interactive)
  (spacemacs//zoom-frm-do 0)
  (spacemacs//zoom-frm-powerline-reset))
