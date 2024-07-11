;;; packages.el --- Gleam layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Qynn Schwaab <qynn@riseup.net>
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


(defconst gleam-packages
  '((tree-sitter-indent)
    (gleam-mode :location (recipe
                           :fetcher github
                           :repo "gleam-lang/gleam-mode"
                           :files ("*.el" "tree-sitter-gleam")))))

(defun gleam/post-init-tree-sitter-indent ()
  "Initialize tree-sitter-indent"
  (use-package tree-sitter-indent
    :defer t))

(defun gleam/init-gleam-mode ()
  "Initialize gleam-mode"
  (use-package gleam-mode
    :mode ("\\.gleam\\'" . gleam-mode)
    :hook ((gleam-mode . spacemacs//gleam-setup-format-on-save)
           (gleam-mode . spacemacs//gleam-setup-lsp))
    :config
    (spacemacs/declare-prefix-for-mode 'gleam-mode "m=" "format")
    (spacemacs/declare-prefix-for-mode 'gleam-mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode 'gleam-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'gleam-mode "mt" "tests")
    (spacemacs/declare-prefix-for-mode 'gleam-mode "mT" "toggle")
    (spacemacs/set-leader-keys-for-major-mode 'gleam-mode
        "==" 'gleam-format
        "cb" 'spacemacs//gleam-build
        "cc" 'spacemacs//gleam-run
        "cm" 'spacemacs//gleam-run-module
        "ca" 'spacemacs//gleam-run-project
        "ta" 'spacemacs//gleam-test-project
        "T=" 'spacemacs//gleam-toggle-format-on-save)))
