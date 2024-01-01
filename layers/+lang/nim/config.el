;;; config.el --- Nim Configuration Layer for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Max Gonzih
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


;; Variables

(spacemacs|define-jump-handlers nim-mode)

(defvar nim-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'company-nim)
  "The backend to use for IDE features.
Possible values are `lsp' and `company-nim'.
If `nil' then 'company-nim` is the default backend unless `lsp' layer is used")
