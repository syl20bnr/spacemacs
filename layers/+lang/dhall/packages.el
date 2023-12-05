;;; packages.el --- Dhall Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Colin Woodbury <colin@fosskers.ca>
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


;;; Commentary:

;;; Code:

(defconst dhall-packages
  '(
    lsp-mode
    dhall-mode
    ))

(defun dhall/init-dhall-mode ()
  "Initialize dhall-mode."
  (use-package dhall-mode
    :defer t

    ))

(defun dhall/post-init-lsp-mode ()
  (add-hook 'dhall-mode-hook #'lsp))

;;; packages.el ends here
