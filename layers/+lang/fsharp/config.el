;;; config.el --- fsharp Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


(spacemacs|define-jump-handlers fsharp-mode fsharp-ac/gotodefn-at-point)

(defvar fsharp-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'eglot)
  "The backend to use for IDE features.
Possible values are `lsp' and `eglot'.
If `nil' then 'eglot` is the default backend unless `lsp' layer is used")
