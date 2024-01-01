;;; packages.el --- kubernetes layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Matt Bray <mattjbray@gmail.com>
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


;;; Code:

(defconst kubernetes-packages
  '(
    kubernetes
    kubernetes-evil
    (kubernetes-tramp :toggle (version< emacs-version "29.0.50"))))

(defun kubernetes/init-kubernetes ()
  (use-package kubernetes
    :defer t
    ;; Autoload for 'kubernetes-overview is defined in "kubernetes-overview.el".
    ;; Add an autoload for the whole 'kubernetes package when kubernetes-overview is called.
    :commands (kubernetes-overview)
    :init (spacemacs/set-leader-keys "atk" 'kubernetes-overview)))

(defun kubernetes/init-kubernetes-evil ()
  (use-package kubernetes-evil
    :after kubernetes-overview))

(defun kubernetes/init-kubernetes-tramp ()
  (use-package kubernetes-tramp
    :defer t))
