;;; packages.el --- semantic Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq semantic-packages
      '(
        (semantic :location built-in)
        srefactor
        ))

;; To fix issue #2569 (https://github.com/syl20bnr/spacemacs/issues/2569),
;; we store the `imenu-create-index-function' for every semantic buffer, and
;; restore it when semantic been turned off.
(defvar-local spacemacs--imenu-create-index-fcn-orig nil
  "Save the original `imenu-create-index-function'")

(define-advice semantic-new-buffer-fcn
    (:before (&rest _) store-imenu-create-index-function)
  (setq spacemacs--imenu-create-index-fcn-orig imenu-create-index-function))

(defun spacemacs//restore-imenu-after-semantic ()
  (unless semantic-mode ; semantic turned off
    (dolist (b (buffer-list))
	    (with-current-buffer b
	      (when (bound-and-true-p spacemacs--imenu-create-index-fcn-orig)
          (setq imenu-create-index-function
                spacemacs--imenu-create-index-fcn-orig))))))

(defun semantic/init-semantic ()
  (use-package semantic
    :defer t
    :config
    (add-to-list 'semantic-default-submodes
                 'global-semantic-idle-summary-mode)
    (add-hook 'semantic-mode-hook 'spacemacs//restore-imenu-after-semantic)))

(defun semantic/init-srefactor ()
  (use-package srefactor :defer t))

(defun spacemacs//disable-semantic-idle-summary-mode ()
  (semantic-idle-summary-mode 0))
