;;; funcs.el --- Spacemacs UI Layer functions File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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



;; ansi-colors

(defun spacemacs-visual//compilation-buffer-apply-ansi-colors ()
  (when (derived-mode-p 'compilation-mode)
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

(defun spacemacs/advice-popwin (orig-fun &rest args)
  "Advice to `popwin:match-config' around to save the buffer active."
  (let ((result (apply orig-fun args)))
    (when result
      (setq spacemacs-popwin--last-buffer (car args)))
    result))

(defun spacemacs/last-popwin ()
  "Resume last popwin buffer."
  (interactive)
  (if (buffer-live-p spacemacs-popwin--last-buffer)
      (select-window (display-buffer spacemacs-popwin--last-buffer))
    (message "Last popwin buffer not found or killed.")))

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
