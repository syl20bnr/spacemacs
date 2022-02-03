;;; packages.el --- tree-sitter layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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
                :repo "jcs090218/ts-fold"))))

(defun tree-sitter/init-tree-sitter ()
  (use-package tree-sitter
    :defer t
    :init
    (progn
      (when tree-sitter-syntax-highlight-enable
       (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))
    :config
    (progn
      (global-tree-sitter-mode))))

(defun tree-sitter/init-tree-sitter-langs ()
  (use-package tree-sitter-langs
    :defer t))

(defun tree-sitter/init-tree-sitter-indent ()
  (use-package tree-sitter-indent
    :if tree-sitter-indent-enable
    :defer t
    :init
    (progn
      (add-hook 'rust-mode-hook #'tree-sitter-indent-mode))))

(defconst tree-sitter--ts-fold-supported-major-mode-hooks
  '(agda-mode-hook
    sh-mode-hook
    c-mode-hook
    c++-mode-hook
    csharp-mode-hook
    css-mode-hook
    ess-r-mode-hook
    go-mode-hook
    html-mode-hook
    java-mode-hook
    javascript-mode-hook
    js-mode-hook
    js2-mode-hook
    js3-mode-hook
    json-mode-hook
    jsonc-mode-hook
    nix-mode-hook
    php-mode-hook
    python-mode-hook
    rjsx-mode-hook
    ruby-mode-hook
    rust-mode-hook
    rustic-mode-hook
    scala-mode-hook
    swift-mode-hook
    typescript-mode-hook))

(defun tree-sitter/init-ts-fold ()
  (use-package ts-fold
    :if tree-sitter-fold-enable
    :defer t
    :init
    (progn
      (when tree-sitter-fold-enable
        (dolist (mode-hook tree-sitter--ts-fold-supported-major-mode-hooks)
          (when (boundp mode-hook)
            (add-hook mode-hook #'ts-fold-mode)
            (when tree-sitter-fold-indicators-enable
              (add-hook mode-hook #'ts-fold-indicators-mode)))))

      (when tree-sitter-fold-indicators-enable
        ;; don't obscure lint and breakpoint indicators
        (setq ts-fold-indicators-priority 0)))))
