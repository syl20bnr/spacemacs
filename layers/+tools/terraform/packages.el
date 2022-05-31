;;; packages.el --- terraform Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
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


(setq terraform-packages
      '(
        company
        (company-terraform :requires company)
        terraform-mode
        ))

(defun terraform/post-init-company ()
  (spacemacs//terraform-setup-company))

(defun terraform/init-company-terraform ()
  (use-package company-terraform
    :defer t))

(defun terraform/init-terraform-mode ()
  (use-package terraform-mode
    :defer t
    :init (add-hook 'terraform-mode-hook
                    'spacemacs//terraform-setup-backend)
    :config (when terraform-auto-format-on-save
              (add-hook 'terraform-mode-hook
                        'terraform-format-on-save-mode))))
