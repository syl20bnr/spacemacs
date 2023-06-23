;;; packages.el --- graphql layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@gmail.com>
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


(defconst graphql-packages
  '(company
    prettier-js
    graphql-mode))

(defun graphql/init-graphql-mode ()
  (use-package graphql-mode
    :defer t
    :init
    (add-to-list 'spacemacs-jump-handlers-graphql-mode 'ahs-backward-definition)
    (when (configuration-layer/layer-used-p 'prettier)
      (spacemacs/declare-prefix-for-mode 'graphql-mode "m=" "format"))
    (spacemacs/declare-prefix-for-mode 'graphql-mode "mg" "goto")
    (spacemacs/set-leader-keys-for-major-mode 'graphql-mode
      "s" 'graphql-send-query
      "e" 'graphql-select-endpoint
      "h" 'graphql-edit-headers)))

(defun graphql/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-dabbrev
    :modes graphql-mode))

(defun graphql/pre-init-prettier-js ()
  (add-to-list 'spacemacs--prettier-modes 'graphql-mode))
