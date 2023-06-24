;;; packages.el --- Confluence Layer packages File for Spacemacs
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


(setq confluence-packages
      '(confluence
        (ox-confluence :location built-in)
        ))

(defun confluence/init-confluence ()
  (use-package confluence
    :defer t
    :config
    ;; remove the hook on buffer save that automatically store the buffer
    ;; in confluence, it creates a lot of useless revision in a page history.
    (advice-add 'confluence-base-mode-init
                :after 'spacemacs//confluence-remove-save-hook)
    (dolist (mode '(confluence-mode
                    confluence-xml-mode
                    confluence-search-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "s" 'spacemacs/confluence-save-to-confluence-minor-edit)
      (spacemacs/set-leader-keys-for-major-mode mode
        "S" 'spacemacs/confluence-save-to-confluence-major-edit)
      (spacemacs/set-leader-keys-for-major-mode mode
        "TAB" 'confluence-toggle-page-content-type))))

(defun confluence/pre-init-ox-confluence ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (require 'ox-confluence)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "ec" 'org-confluence-export-as-confluence))))
(defun confluence/init-ox-confluence ())
