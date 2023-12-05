;;; packages.el --- elasticsearch layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Jean Rigotti <gmgotti@pm.me>
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


(defconst elasticsearch-packages
  '(
    company
    org
    es-mode
    ))

(defun elasticsearch/init-es-mode ()
  (use-package es-mode
    :defer t
    :mode ("\\.es\\'" . es-mode)))

(defun elasticsearch/post-init-company ()
  (spacemacs|add-company-backends :backends es-company-backend :modes es-mode))

(defun elasticsearch/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(elasticsearch . t))))
