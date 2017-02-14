;;; packages.el --- d Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
        (company-dcd :toggle (configuration-layer/package-usedp 'company))
        d-mode
        flycheck
        (flycheck-dmd-dub :toggle (configuration-layer/package-usedp 'flycheck))
        ggtags
        helm-gtags
        ))

(defun d/post-init-company ()
  ;; Need to convince company that this C-derived mode is a code mode.
  (with-eval-after-load 'company-dabbrev-code
    (push 'd-mode company-dabbrev-code-modes)))

(defun d/init-company-dcd ()
  (use-package company-dcd
    :defer t
    :init
    (progn
      (spacemacs|add-company-backends :backends company-dcd :modes d-mode)
      (spacemacs/set-leader-keys-for-major-mode 'd-mode
        "gg" 'company-dcd-goto-definition
        "gb" 'company-dcd-goto-def-pop-marker
        "hh" 'company-dcd-show-ddoc-with-buffer
        "gr" 'company-dcd-ivy-search-symbol))))

(defun d/init-d-mode ()
  (use-package d-mode :defer t))

(defun d/post-init-flycheck ()
  (spacemacs/enable-flycheck 'd-mode))

(defun d/init-flycheck-dmd-dub ()
  (use-package flycheck-dmd-dub :defer t
    :init (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)))

(defun d/post-init-ggtags ()
  (add-hook 'd-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun d/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'd-mode))
