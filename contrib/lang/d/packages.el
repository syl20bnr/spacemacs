;;; packages.el --- d Layer packages File for Spacemacs
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
(defvar d-packages
  '(
    d-mode
    flycheck-dmd-dub

    flycheck
    company
    ))

(defun d/init-d-mode ()
  (use-package d-mode :defer t))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun d/post-init-flycheck ()
    (add-hook 'd-mode-hook 'flycheck-mode))
  (defun d/init-flycheck-dmd-dub ()
    (use-package flycheck-dmd-dub :defer t
      :init (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun d/post-init-company ()
    ;; Need to convince company that this C-derived mode is a code mode.
    (eval-after-load 'company-dabbrev-code '(push 'd-mode company-dabbrev-code-modes))
    (spacemacs|add-company-hook d-mode)))
