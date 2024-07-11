;;; packages.el --- Evernote Layer packages File for Spacemacs
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

(defconst evernote-packages
  '(
    geeknote))

(defun evernote/init-geeknote ()
  "Initializes geeknote and adds keybindings for its exposed functionalities."
  (use-package geeknote
    :commands (geeknote-create
               geeknote-edit
               geeknote-find
               geeknote-show
               geeknote-remove
               geeknote-move)
    :init
    (spacemacs/declare-prefix "awe" "applications-evernote")
    (spacemacs/set-leader-keys
      "awec" 'geeknote-create
      "awee" 'geeknote-edit
      "awef" 'geeknote-find
      "awes" 'geeknote-show
      "awer" 'geeknote-remove
      "awem" 'geeknote-move)))
