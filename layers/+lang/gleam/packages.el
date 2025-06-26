;;; packages.el --- Gleam layer packages file for Spacemacs.  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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
  '((treesit)
    (gleam-ts-mode)))

(defun gleam/init-gleam-ts-mode ()
  "Initialize gleam-ts-mode"
  (unless (treesit-available-p)
    (error "Gleam layer requires Emacs to be compiled with treesit support (built-in with Emacs 29+)"))
  (use-package gleam-ts-mode
    :mode ("\\.gleam\\'" . gleam-ts-mode)
    :hook ((gleam-ts-mode . spacemacs//gleam-setup-format-on-save)
           (gleam-ts-mode . spacemacs//gleam-setup-lsp))
    :config
    (spacemacs//gleam-setup-treesit)
    (spacemacs/declare-prefix-for-mode 'gleam-ts-mode "m=" "format")
    (spacemacs/declare-prefix-for-mode 'gleam-ts-mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode 'gleam-ts-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'gleam-ts-mode "mt" "tests")
    (spacemacs/declare-prefix-for-mode 'gleam-ts-mode "mT" "toggle")
    (spacemacs/set-leader-keys-for-major-mode 'gleam-ts-mode
      "==" 'gleam-ts-format
      "cb" 'spacemacs//gleam-build
      "cc" 'spacemacs//gleam-run
      "cm" 'spacemacs//gleam-run-module
      "ca" 'spacemacs//gleam-run-project
      "ta" 'spacemacs//gleam-test-project
      "T=" 'spacemacs//gleam-toggle-format-on-save)))
