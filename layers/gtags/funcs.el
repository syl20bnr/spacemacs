;;; funcs.el --- gtags functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun helm-gtags-dwim-other-window ()
  "helm-gtags-dwim in the other window"
  (interactive)
  (let ((helm-gtags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-dwim)))

(defun spacemacs/helm-gtags-define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  (when (fboundp mode)
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook 'helm-gtags-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "gc" 'helm-gtags-create-tags
      "gd" 'helm-gtags-find-tag
      "gf" 'helm-gtags-select-path
      "gg" 'helm-gtags-dwim
      "gG" 'helm-gtags-dwim-other-window
      "gi" 'helm-gtags-tags-in-this-function
      "gl" 'helm-gtags-parse-file
      "gn" 'helm-gtags-next-history
      "gp" 'helm-gtags-previous-history
      "gr" 'helm-gtags-find-rtag
      "gR" 'helm-gtags-resume
      "gs" 'helm-gtags-select
      "gS" 'helm-gtags-show-stack
      "gu" 'helm-gtags-update-tags)))

(defun spacemacs/ggtags-enable-eldoc (mode)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
              (ggtags-mode 1)
              (eldoc-mode 1)
              (setq-local eldoc-documentation-function
                          #'ggtags-eldoc-function))))
