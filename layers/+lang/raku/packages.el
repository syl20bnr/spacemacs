;;; packages.el --- Raku layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


(defconst raku-packages
  '(
    company
    evil
    flycheck
    (flycheck-raku :requires flycheck)
    raku-mode
    ))

(defun raku/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes raku-mode))

(defun raku/post-init-evil ()
  (add-to-list 'spacemacs-jump-handlers-raku-mode 'evil-jump-to-tag))

(defun raku/post-init-flycheck ()
  (spacemacs/enable-flycheck 'raku-mode))

(defun raku/init-flycheck-raku ()
  ;; not deferring this load because there are no autoloads
  (use-package flycheck-raku))

(defun raku/init-raku-mode ()
  (use-package raku-mode
    :defer t))
