;;; packages.el --- semantic Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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
        stickyfunc-enhance
        ))

(defun semantic/init-semantic ()
  (use-package semantic
    :defer t
    :config (add-to-list 'semantic-default-submodes
                         'global-semantic-idle-summary-mode)))

(defun semantic/init-srefactor ()
  (use-package srefactor :defer t))

(defun semantic/pre-init-stickyfunc-enhance ()
  (spacemacs|use-package-add-hook semantic
    :post-init (add-to-list 'semantic-default-submodes
                            'global-semantic-stickyfunc-mode)))

(defun semantic/init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (progn
      (spacemacs|add-toggle semantic-stickyfunc
        :mode semantic-stickyfunc-mode
        :documentation "Enable semantic-stickyfunc."
        :evil-leader "TS")
      (spacemacs|add-toggle semantic-stickyfunc-globally
        :mode global-semantic-stickyfunc-mode
        :documentation "Enable semantic-stickyfunc globally."
        :evil-leader "T C-S"))))

(defun spacemacs//disable-semantic-idle-summary-mode ()
  (semantic-idle-summary-mode 0))
