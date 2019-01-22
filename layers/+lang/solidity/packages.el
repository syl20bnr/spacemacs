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
  '(solidity-mode
    company
    flymake-solidity
    ))

 (defun solidity/init-solidity-mode ()
   (use-package solidity-mode
     :defer t
 
     :config
     (spacemacs/set-leader-keys-for-major-mode 'solidity-mode 
       "g" 'solidity-estimate-gas-at-point)))
 
(defun solidity/post-init-company ()
  (spacemacs|add-company-backends :modes solidity-mode))
 
(defun solidity/init-flymake-solidity ()
  (use-package flymake-solidity 
    :defer t

    :init
    (require 'flymake-solidity)
    (add-hook 'solidity-mode-hook 'flymake-solidity-load)))

;;; packages.el ends here
