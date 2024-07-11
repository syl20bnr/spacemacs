;;; packages.el --- solidity layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Brooklyn Zelenka <be.zelenka@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
    (add-hook 'solidity-mode-hook (lambda () (require 'solidity-flycheck)))))
