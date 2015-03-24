;;; funcs.el --- Better Emacs Defaults Layer functions File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; from Prelude
;; TODO To adapt for `=' of evil
;; (defvar spacemacs-indent-sensitive-modes
;;   '(coffee-mode
;;     python-mode
;;     slim-mode
;;     haml-mode
;;     yaml-mode
;;     makefile-mode
;;     makefile-gmake-mode
;;     makefile-imake-mode
;;     makefile-bsdmake-mode)
;;   "Modes for which auto-indenting is suppressed.")
;; (defun spacemacs/indent-region-or-buffer ()
;;   "Indent a region if selected, otherwise the whole buffer."
;;   (interactive)
;;   (unless (member major-mode spacemacs-indent-sensitive-modes)
;;     (save-excursion
;;       (if (region-active-p)
;;           (progn
;;             (indent-region (region-beginning) (region-end))
;;             (message "Indented selected region."))
;;         (progn
;;           (evil-indent (point-min) (point-max))
;;           (message "Indented buffer.")))
;;       (whitespace-cleanup))))
