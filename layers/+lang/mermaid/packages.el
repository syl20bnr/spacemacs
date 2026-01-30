;;; packages.el --- mermaid layer packages file for Spacemacs.  -*- lexical-binding: t; -*-

;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Lin Sun <lin.sun AT hotmail.com>
;; URL: https://github.com/syl20bnr/spacemacs/
;;
;;; Commentary:
;;
;; Adds Mermaid support to Spacemacs using mermaid-mode.
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

(defconst mermaid-packages
  '(mermaid-mode))

(defun mermaid/init-mermaid-mode ()
  (use-package mermaid-mode
    :defer t
    :config
    ;; Set up key bindings
    (spacemacs/declare-prefix-for-mode 'mermaid-mode
      "mc" "compile")
    (spacemacs/set-leader-keys-for-major-mode 'mermaid-mode
      "cc" 'mermaid-compile
      "cf" 'mermaid-compile-file
      "cb" 'mermaid-compile-buffer
      "cr" 'mermaid-compile-region
      "co" 'mermaid-open-browser
      "cd" 'mermaid-open-doc)))

;;; packages.el ends here
