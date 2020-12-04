;;; packages.el --- d Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq d-packages
      '(
        company
        (company-dcd :requires company)
        d-mode
        flycheck
        (flycheck-dmd-dub :requires flycheck)
        ggtags
        counsel-gtags
        helm-gtags
        ))

(defun d/post-init-company ()
  ;; Need to convince company that this C-derived mode is a code mode.
  (with-eval-after-load 'company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'd-mode)))

(defun d/init-company-dcd ()
  (use-package company-dcd
    :defer t
    :init
    (progn
      (space-macs|add-company-backends :backends company-dcd :modes d-mode)
      (space-macs/set-leader-keys-for-major-mode 'd-mode
        "gg" 'company-dcd-goto-definition
        "gb" 'company-dcd-goto-def-pop-marker
        "hh" 'company-dcd-show-ddoc-with-buffer
        "gr" 'company-dcd-ivy-search-symbol))))

(defun d/init-d-mode ()
  (use-package d-mode :defer t))

(defun d/post-init-flycheck ()
  (space-macs/enable-flycheck 'd-mode))

(defun d/init-flycheck-dmd-dub ()
  (use-package flycheck-dmd-dub :defer t
    :init
    (progn
      (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)
      (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables))))

(defun d/post-init-ggtags ()
  (add-hook 'd-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun d/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'd-mode))

(defun d/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'd-mode))


