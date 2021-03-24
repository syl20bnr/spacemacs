;;; packages.el --- perl6 layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author:  Bahtiar `kalkin-`''Gadimov <bahtiar@gadimov.de>
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


(defconst perl6-packages
  '(
    company
    evil
    flycheck
    (flycheck-perl6 :requires flycheck)
    raku-mode
    ))

(defun perl6/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes perl6-mode))

(defun perl6/post-init-evil ()
  (add-to-list 'spacemacs-jump-handlers-perl6-mode 'evil-jump-to-tag))

(defun perl6/post-init-flycheck ()
  (spacemacs/enable-flycheck 'perl6-mode))

(defun perl6/init-flycheck-perl6 ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-perl6)))

(defun perl6/init-raku-mode ()
  (use-package raku-mode
    :defer t
    :mode (("/perl6/site/sources/" . perl6-mode))))
