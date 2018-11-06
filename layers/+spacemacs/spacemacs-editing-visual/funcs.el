;;; funcs.el --- Spacemacs Editing Visual Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/toggle-centered-buffer ()
  "Toggle visual centering of the current buffer."
  (interactive)
  (letf ((writeroom-maximize-window nil)
         (writeroom-mode-line t))
    (call-interactively 'writeroom-mode)))

(defun spacemacs/toggle-distraction-free ()
  "Toggle visual distraction free mode."
  (interactive)
  (letf ((writeroom-maximize-window t)
         (writeroom-mode-line nil))
    (call-interactively 'writeroom-mode)))
