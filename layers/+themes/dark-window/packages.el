;;; packages.el --- dark-window layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Oleg Pykhalov <go.wigust@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Thanks to Nicolas Petton for his work on which this code is based.
;; URL: http://nicolas.petton.fr/blog/emacs-dark-window-decoration.html

(defconst dark-window-packages
  '(frame-cmds))

(defun dark-window/init-frame-cmds ()
  (use-package frame-cmds
    :config
    (progn
      (defun set-selected-frame-dark ()
        (interactive)
        (let ((frame-name (get-frame-name (selected-frame))))
          (call-process-shell-command
           (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \"" frame-name "\""))))

      (when (window-system)
        (set-selected-frame-dark)))))

;;; packages.el ends here
