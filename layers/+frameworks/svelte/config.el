;;; config.el --- svelte layer config file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Marco Süß <msuess@mailbox.org>
;; URL: https://github.com/msuess
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

(defvar svelte-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'dump)
  "The backend to use for IDE features.
Possible values are `dump' and `lsp'.
If `nil' then `dump' is the default backend unless `lsp' layer is used")

(spacemacs|define-jump-handlers svelte-mode)
