;;; packages.el --- Protocol Buffers Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Amol Mandhane <https://github.com/amol-mandhane>
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


(defconst protobuf-packages
  '(
    flycheck
    protobuf-mode
    ))

(defun protobuf/post-init-flycheck ()
  (spacemacs/enable-flycheck 'protobuf-mode))

(defun protobuf/init-protobuf-mode ()
  (use-package protobuf-mode
    :init (add-hook 'protobuf-mode-hook 'spacemacs//setup-protobuf-imenu)))
