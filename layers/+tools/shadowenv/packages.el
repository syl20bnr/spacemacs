;;; packages.el --- Shadowenv Layer packages File for Spacemacs
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


(defconst shadowenv-packages
  '(eshell
    shadowenv))

(defun shadowenv/post-init-eshell ()
  (add-hook 'eshell-mode-hook 'shadowenv-global-mode))

(defun shadowenv/init-shadowenv ()
  (use-package shadowenv
    :defer (spacemacs/defer)
    :spacediminish " 🆂" " [S]"
    :init
    (progn
      (if shadowenv-enable-at-startup
          (shadowenv-global-mode 1)
        (add-hook 'prog-mode-hook 'shadowenv-global-mode))
      (spacemacs|spacebind
       :global
       ("Shadowenv key bindings"
        ("V" "Version/Environment"
         ("S" "Shadowenv"
          ("r" shadowenv-reload "Reload shadowenv configuration")
          ("s" shadowenv-shadows "Display the environment shadows..."))))))))
