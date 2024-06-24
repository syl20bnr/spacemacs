;;; packages.el --- tree-sitter layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Elliott Shugerman <eeshugerman@gmail.com>
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
  '(tree-sitter
    tree-sitter-langs
    (tree-sitter-indent
     :toggle tree-sitter-indent-enable)
    (ts-fold
     :toggle tree-sitter-fold-enable
     :location (recipe
                :fetcher github
                :repo "emacs-tree-sitter/ts-fold"))))

(defun tree-sitter/init-tree-sitter ()
  (use-package tree-sitter
    :init
    (when tree-sitter-syntax-highlight-enable
      (add-hook 'tree-sitter-after-on-hook #'spacemacs//tree-sitter-hl-maybe))
    :config
    (global-tree-sitter-mode)))

(defun tree-sitter/init-tree-sitter-langs ()
  (use-package tree-sitter-langs))

(defun tree-sitter/init-tree-sitter-indent ()
  (use-package tree-sitter-indent
    :if tree-sitter-indent-enable
    :init
    (add-hook 'rust-mode-hook #'tree-sitter-indent-mode)))

(defun tree-sitter/init-ts-fold ()
  (use-package ts-fold
    :if tree-sitter-fold-enable
    :init
    (when tree-sitter-fold-enable
      (if tree-sitter-fold-indicators-enable
          (progn
            (setq ts-fold-indicators-priority 0)
            (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode))
        (global-ts-fold-mode)))))
