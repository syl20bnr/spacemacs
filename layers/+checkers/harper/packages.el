;;; packages.el --- Harper layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: sunlin7<at>hotmail.com
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

;; Harper layers provides no elpa package of its own: harper-ls is an external
;; binary and the integration is built on top of `lsp-mode', which is provided
;; by the `lsp' layer (declared as a dependency in `layers.el').  We still list
;; `lsp-mode' here (without owning it) so that the `pre-init-lsp-mode' hook
;; below is associated with the package and runs after lsp-mode is configured.
(defconst harper-packages
  '(
    lsp-mode
    ))

(defun harper/pre-init-lsp-mode ()
  "Register harper-ls as an add-on LSP client once lsp-mode is loaded."
  (spacemacs|use-package-add-hook lsp-mode
    :post-config
    (harper//setup-lsp-client)))
