;;; packages.el --- d Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq d-packages
      '(
        company
        d-mode
        flycheck
        (flycheck-dmd-dub :toggle (configuration-layer/package-usedp 'flycheck))
        ggtags
        helm-gtags
        ))

(defun d/post-init-company ()
  ;; Need to convince company that this C-derived mode is a code mode.
  (with-eval-after-load 'company-dabbrev-code (push 'd-mode company-dabbrev-code-modes))
  (spacemacs|add-company-hook d-mode))

(defun d/init-d-mode ()
  (use-package d-mode :defer t))

(defun d/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'd-mode))

(defun d/init-flycheck-dmd-dub ()
  (use-package flycheck-dmd-dub :defer t
    :init (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)))

(defun d/post-init-ggtags ()
  (add-hook 'd-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun d/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'd-mode))
