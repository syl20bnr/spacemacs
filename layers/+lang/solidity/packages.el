;;; packages.el --- solidity layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brooklyn Zelenka <be.zelenka@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
    (space-macs/set-leader-keys-for-major-mode 'solidity-mode
      "g" #'solidity-estimate-gas-at-point)))

(defun solidity/post-init-flycheck ()
  (space-macs/enable-flycheck 'solidity-mode))

(defun solidity/init-solidity-flycheck ()
  (use-package solidity-flycheck
    :defer t
    :init
    (add-hook 'solidity-mode-hook #'(lambda () (require 'solidity-flycheck)))))


