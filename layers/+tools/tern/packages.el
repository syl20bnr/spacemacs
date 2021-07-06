;;; packages.el --- Tern Layer packages File for Spacemacs
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


(defconst tern-packages
  '(tern))

(defun tern/init-tern ()
  (use-package tern
    :defer t
    :config
    (progn
      (spacemacs|hide-lighter tern-mode)
      (dolist (mode tern--key-bindings-modes)
        (add-to-list (intern (format "spacemacs-jump-handlers-%S" mode))
                     '(tern-find-definition :async t))
        (spacemacs/set-leader-keys-for-major-mode mode
          "rrV" 'tern-rename-variable
          "hd" 'tern-get-docs
          "gG" 'tern-find-definition-by-name
          (kbd "C-g") 'tern-pop-find-definition
          "ht" 'tern-get-type)))))
