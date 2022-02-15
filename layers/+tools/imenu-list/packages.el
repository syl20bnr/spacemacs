;;; packages.el --- imenu-list Layer packages File for Spacemacs
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


(defconst imenu-list-packages
  '(
    golden-ratio
    imenu-list
    ))

(defun imenu-list/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")))

(defun imenu-list/init-imenu-list ()
  (use-package imenu-list
    :defer t
    :init
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t)
    :config
    (evilified-state-evilify-map imenu-list-major-mode-map
      :mode imenu-list-major-mode
      :bindings
      "d" #'imenu-list-display-entry
      "r" #'imenu-list-refresh)
    :spacebind
    (:global
     (("b" "Buffers" ("i" spacemacs/imenu-list-smart-focus "Focus imenu sidebar"))
      ("T" "UI toggles/themes" ("i" imenu-list-smart-toggle "Toggle imenu sidebar"))))))
