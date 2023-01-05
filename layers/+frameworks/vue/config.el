;;; config.el --- vue layer config file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
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

(spacemacs|defc vue-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'dumb)
  "The backend to use for IDE features.
Possible values are `lsp' and `dumb'.
If not set then `dumb' is the default backend unless `lsp' layer is used."
  '(choice (const lsp) (const dumb)) nil t)

(spacemacs|defc vue-ignore-lsp-diagnostics nil
  "If VUE-BACKEND is `lsp' the server will handle the linters, if you prefer to have emacs handle these instead set this to `t'"
  'boolean nil t)

(spacemacs|define-jump-handlers vue-mode)
