;;; funcs.el --- ranger Layer functions File for Spacemacs  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
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


(defun ranger//toggle-ranger-deer ()
  "Toggle between `ranger' and `deer' views."
  (interactive)
  (if (derived-mode-p 'ranger-mode)
      (progn
        (ranger-minimal-toggle)
        (setq ranger-override-dired
              (if (r--fget ranger-minimal) 'deer 'ranger)))
    (setq ranger-override-dired
          (if (eq ranger-override-dired 'ranger) 'deer 'ranger))
    (ranger//apply-override-dired)
    (if (eq ranger-override-dired 'ranger)
        (ranger)
      (deer))))

(defun ranger/dirvish-full-layout ()
  "Open dirvish with the full layout."
  (interactive)
  (let ((dirvish-default-layout (or dirvish-default-layout '(1 0.11 0.55))))
    (dirvish))
  (when (and (dirvish-curr) (not (dv-curr-layout (dirvish-curr))))
    (dirvish-layout-toggle)))

(defun ranger//apply-override-dired ()
  "Enable the appropriate dired override based on `ranger-override-dired'."
  (cond
   ((memq ranger-override-dired '(ranger deer))
    (when (boundp 'dirvish-override-dired-mode)
      (dirvish-override-dired-mode -1))
    (when (fboundp 'ranger-override-dired-mode)
      (ranger-override-dired-mode 1)))
   ((eq ranger-override-dired 'dirvish)
    (when (boundp 'ranger-override-dired-mode)
      (ranger-override-dired-mode -1))
    (when (fboundp 'dirvish-override-dired-mode)
      (dirvish-override-dired-mode 1)))
   (t
    (when (boundp 'ranger-override-dired-mode)
      (ranger-override-dired-mode -1))
    (when (boundp 'dirvish-override-dired-mode)
      (dirvish-override-dired-mode -1)))))

(defun dirvish/dired-find-file-smart ()
  "Open directory in same window, file in other window.
This provides a convenient navigation workflow where directories
are navigated in place while files are opened in a separate window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-file)
      (dired-find-file-other-window))))
