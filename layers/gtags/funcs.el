;;; funcs.el --- gtags functions File
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
    (evil-leader/set-key-for-mode mode
      "mgc" 'helm-gtags-create-tags
      "mgd" 'helm-gtags-find-tag
      "mgf" 'helm-gtags-select-path
      "mgg" 'helm-gtags-dwim
      "mgG" 'helm-gtags-dwim-other-window
      "mgi" 'helm-gtags-tags-in-this-function
      "mgl" 'helm-gtags-parse-file
      "mgn" 'helm-gtags-next-history
      "mgp" 'helm-gtags-previous-history
      "mgr" 'helm-gtags-find-rtag
      "mgR" 'helm-gtags-resume
      "mgs" 'helm-gtags-select
      "mgS" 'helm-gtags-show-stack
      "mgu" 'helm-gtags-update-tags)))

(defun spacemacs/ggtags-enable-eldoc (mode)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
              (ggtags-mode 1)
              (eldoc-mode 1)
              (setq-local eldoc-documentation-function
                          #'ggtags-eldoc-function))))
