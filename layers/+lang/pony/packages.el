;;; packages.el --- Pony layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Zhe Wang <0x1998@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst pony-packages
  '(company
    flycheck
    flycheck-pony
    ponylang-mode
    pony-snippets
    yasnippet))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun pony/post-init-company ()
    (spacemacs|add-company-hook ponylang-mode))
  (defun pony/init-pony-snippets ()
    (use-package pony-snippets
      :defer t)))

(defun pony/init-ponylang-mode ()
  (use-package ponylang-mode
    :defer t
    :config
    (progn
      (add-hook 'ponylang-mode-hook
                (lambda ()
                  (set-variable 'indent-tabs-mode nil)
                  (set-variable 'tab-width 2)))
      (spacemacs/set-leader-keys-for-major-mode 'ponylang-mode
        "c" 'spacemacs/pony-compile))))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun pony/post-init-flycheck ()
    (spacemacs/add-flycheck-hook 'ponylang-mode))
  (defun pony/init-flycheck-pony ()
    (use-package flycheck-pony)))
