;;; funcs.el --- Spacemacs Editing Visual Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; TODO: Allow direct transition between centered and distraction free states.

(defun spacemacs/toggle-centered-buffer ()
  "Toggle visual centering of the current buffer."
  (interactive)
  (cl-letf ((writeroom-maximize-window nil)
         (writeroom-mode-line t))
    (call-interactively 'writeroom-mode)))

(defun spacemacs/toggle-distraction-free ()
  "Toggle visual distraction free mode."
  (interactive)
  (cl-letf ((writeroom-maximize-window t)
         (writeroom-mode-line nil))
    (call-interactively 'writeroom-mode)))

(defun spacemacs/centered-buffer-transient-state ()
  "Center buffer and enable centering transient state."
  (interactive)
  (cl-letf ((writeroom-maximize-window nil)
         (writeroom-mode-line t))
    (writeroom-mode 1)
    (spacemacs/centered-buffer-mode-transient-state/body)))
