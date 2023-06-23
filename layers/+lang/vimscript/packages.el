;;; packages.el --- vimscript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defconst vimscript-packages
  '(
    company
    flycheck
    vimrc-mode
    ggtags
    counsel-gtags
    dactyl-mode))

(defun vimscript/post-init-company ()
  (spacemacs//vimscript-setup-company))

(defun vimscript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'vimrc-mode))

(defun vimscript/init-vimrc-mode ()
  "Initialize vimrc package"
  (use-package vimrc-mode
    :mode "\\.vim[rc]?\\'"
    :mode "_vimrc\\'"
    :defer t
    :init
    (defun spacemacs//vimrc-mode-hook ()
      "Hooked function for `vimrc-mode-hook'."
      (highlight-numbers-mode -1)
      (rainbow-delimiters-mode-disable)
      (spacemacs//vimscript-setup-backend))
    (add-hook 'vimrc-mode-hook 'spacemacs//vimrc-mode-hook)))

(defun vimscript/init-dactyl-mode ()
  (use-package dactyl-mode
    :mode "pentadactylrc\\'"
    :mode "vimperatorrc\\'"
    :mode "_pentadactylrc\\'"
    :mode "_vimperatorrc\\'"
    :mode "\\.penta\\'"
    :mode "\\.vimp\\'"
    :defer t))

(defun vimscript/post-init-ggtags ()
  (add-hook 'vimrc-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun vimscript/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'vimrc-mode))
