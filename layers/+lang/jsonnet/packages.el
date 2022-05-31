;;; packages.el --- jsonnet layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Liz <liz@kazkaan>
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


(setq jsonnet-packages
      '(
        jsonnet-mode
        flycheck
        ))

(defun jsonnet/post-init-flycheck ()
  (spacemacs/enable-flycheck 'jsonnet-mode))

(defun jsonnet/init-jsonnet-mode ()
  (use-package jsonnet-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'jsonnet-mode
        "=" 'jsonnet-reformat-buffer
        "gg" 'jsonnet-jump
        "eb" 'jsonnet-eval-buffer))))
