;;; packages.el --- solidity layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Brooklyn Zelenka <be.zelenka@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst solidity-packages
  '(
    add-node-modules-path
    flycheck
    solidity-mode
    solidity-flycheck))

(defun solidity/post-init-add-node-modules-path ()
  (add-hook 'solidity-mode-hook #'add-node-modules-path))

(defun solidity/init-solidity-mode ()
  (use-package solidity-mode
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'solidity-mode
      "g" #'solidity-estimate-gas-at-point)))

(defun solidity/post-init-flycheck ()
  (spacemacs/enable-flycheck 'solidity-mode))

(defun solidity/init-solidity-flycheck ()
  (use-package solidity-flycheck
    :defer t
    :init
    (add-hook 'solidity-mode-hook #'(lambda () (require 'solidity-flycheck)))))
