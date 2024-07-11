;;; packages.el --- faust layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author:  Bart Brouns <bart@magnetophon.nl>
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

(defconst faust-packages
  '(
    company
    faust-mode
    yasnippet
    ))

(defun faust/post-init-company ()
  (spacemacs|add-company-backends :modes faust-mode))

(defun faust/init-faust-mode ()
  (use-package faust-mode
    :defer t
    :mode "\\.\\(dsp\\|lib\\)\\'"
    :init
    (spacemacs/set-leader-keys-for-major-mode 'faust-mode
      "cf" 'spacemacs/faust-to-firefox
      "cg" 'spacemacs/faust-to-jack-gtk
      "cq" 'spacemacs/faust-to-jack-qt)))

(defun faust/post-init-yasnippet ()
  (add-hook 'faust-mode-hook 'spacemacs/load-yasnippet))
