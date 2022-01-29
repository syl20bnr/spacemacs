;;; layers.el --- svelte Layer layers File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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

(configuration-layer/declare-layers '(node html prettier javascript))

(when (and (boundp 'svelte-backend)
           (eq svelte-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))
