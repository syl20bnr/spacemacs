;;; packages.el --- rest layer packages file for Spacemacs.  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author:  <wwguo@hiGDP>
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


(defconst restructuredtext-packages
  '(
    company
    ;; Disabled due to package is not longer maintained
    ;; (auto-complete-rst :requires auto-complete)
    (rst :location built-in)
    (rst-directives :location local)
    (rst-lists :location local)
    flyspell
    smartparens
    yasnippet))

(defun restructuredtext/init-rst-directives ()
  (use-package rst-directives))

(defun restructuredtext/init-rst-lists ()
  (use-package rst-lists))

(defun restructuredtext/init-rst ()
  (use-package rst
    :defer t
    :config (add-hook 'rst-adjust-hook 'rst-toc-update)))

(defun restructuredtext/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes rst-mode))

(defun restructuredtext/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'rst-mode-hook))

(defun restructuredtext/post-init-yasnippet ()
  (add-hook 'rst-mode-hook 'spacemacs/load-yasnippet))

(defun restructuredtext/post-init-smartparens ()
  (add-hook 'rst-mode-hook #'spacemacs//activate-smartparens))
