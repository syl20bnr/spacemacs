;;; packages.el --- workgroups2 Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq workgroups2-packages
      '(workgroups2))

(defun workgroups2/init-workgroups2 ()
  (use-package workgroups2
    :config
    (progn
      (spacemacs/declare-prefix "G" "workgroups")
      (evil-leader/set-key "Gc" 'wg-create-workgroup)
      (evil-leader/set-key "Gr" 'wg-rename-workgroup)
      (evil-leader/set-key "Gk" 'wg-kill-workgroup)
      (evil-leader/set-key "Gs" 'wg-switch-to-workgroup)
      (evil-leader/set-key "GS" 'wg-save-session)
      ;; Disable loading multiple frames
      (setq wg-control-frames nil)
      ;; Enable workgroups-mode. Need to override wg-change-modeline because it tries to set modeline which fails on start up.
      (flet ((wg-change-modeline () ()))
        (workgroups-mode 1))
      (spacemacs|diminish workgroups-mode " Ⓖ" " G"))))
