;;; packages.el --- Fountain layer packages file for Spacemacs.
;;
;; Copyright (c) 2022-2024 Sylvain Benner & Contributors
;;
;; Author: Damien Picard <dam.pic@free.fr>
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


(defconst fountain-packages
  '(
    fountain-mode
    olivetti))

(defun fountain/init-olivetti ()
  (use-package olivetti
    :defer t
    :hook '(fountain-mode . olivetti-mode)))

(defun fountain/init-fountain-mode ()
  (use-package fountain-mode
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'fountain-mode
      ;; Editing commands
      "RET" 'fountain-upcase-line-and-newline
      "xU" 'fountain-upcase-line
      "cd" 'fountain-add-continued-dialog
      "cD" 'fountain-remove-continued-dialog
      "in" 'fountain-insert-note
      "is" 'fountain-insert-synopsis
      "ii" 'auto-insert
      "cn" 'fountain-add-scene-numbers
      "cN" 'fountain-remove-scene-numbers
      "ib" 'fountain-insert-page-break
      "]" 'fountain-completion-update

      "Tm" 'fountain-toggle-hide-emphasis-markup
      "Te" 'fountain-toggle-hide-element-markup

      ;; Navigation commands
      "js" 'fountain-goto-scene
      "jp" 'fountain-goto-page

      ;; Outline commands
      "TAB"  'fountain-outline-cycle
      "o"  'fountain-outline-to-indirect-buffer
      "ih" 'fountain-insert-section-heading

      ;; Pagination commands
      "cp" 'fountain-count-pages
      "cu" 'fountain-pagination-update

      ;; Exporting commands
      "ee" 'fountain-export-command
      "ev" 'fountain-export-view)

    (spacemacs/declare-prefix-for-mode 'fountain-mode "mi" "fountain/insert")
    (spacemacs/declare-prefix-for-mode 'fountain-mode "mx" "fountain/text")
    (spacemacs/declare-prefix-for-mode 'fountain-mode "mc" "fountain/command")
    (spacemacs/declare-prefix-for-mode 'fountain-mode "mT" "fountain/toggle")
    (spacemacs/declare-prefix-for-mode 'fountain-mode "mj" "fountain/jump")
    (spacemacs/declare-prefix-for-mode 'fountain-mode "me" "fountain/export")
    ))
