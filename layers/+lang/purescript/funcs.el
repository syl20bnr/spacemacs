;;; funcs.el --- PureScript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: nobv <6e6f6276@gmail.com>
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


(defun spacemacs//purescript-setup-backend ()
  "Conditionally setup purescript backend."
  (when (eq purescript-backend 'lsp)
    (lsp)))

(defun spacemacs//purescript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase purescript-backend
    ('lsp
     (spacemacs|add-company-backends ;; Activate lsp company explicitly to activate
       :backends company-capf        ;; standard backends as well
       :modes purescript-mode))
    ('psc-ide
     (spacemacs|add-company-backends
       :backends company-psc-ide-backend
       :modes purescript-mode))))
