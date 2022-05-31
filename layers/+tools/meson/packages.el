;;; packages.el --- meson layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Matthew Leach <dev@mattleach.net>
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


(defconst meson-packages
  '(meson-mode))

(defun meson/init-meson-mode ()
  (use-package meson-mode
    :defer t
    :mode (("meson\\.build\\'" . meson-mode))
    :config
    (spacemacs/declare-prefix-for-mode 'meson-mode "mh" "help")
    (spacemacs/set-leader-keys-for-major-mode 'meson-mode "hh" 'meson-lookup-doc-at-point)

    ;; Make sure that standard spacemacs f1 help bindings prevail
    (define-key meson-mode-map [f1] nil)))
