;;; packages.el --- Tree Sitter Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <mail+spacemacs@codrut.pro>
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

(defconst tree-sitter-packages
  '(
    tree-sitter
    tree-sitter-langs))

(defun tree-sitter/init-tree-sitter ()
  (configuration-layer/with-dynamic-modules
    (use-package tree-sitter
      :defer (eq tree-sitter-toggle 'on-demand)
      :init
      (when (eq tree-sitter-toggle 'global)
        (global-tree-sitter-mode))
      :config
      (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))))

(defun tree-sitter/init-tree-sitter-langs ()
  (configuration-layer/with-dynamic-modules
    (use-package tree-sitter-langs
     :defer (eq tree-sitter-toggle 'on-demand))))
