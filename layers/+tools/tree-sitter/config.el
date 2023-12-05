;;; config.el --- tree-sitter layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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

(defvar tree-sitter-syntax-highlight-enable t
  "If non nil, use tree-sitter for syntax highlighting where supported.")

(defvar tree-sitter-indent-enable nil
  "If non nil, use tree-sitter for indentation where supported.")

(defvar tree-sitter-fold-enable nil
  "If non nil, use tree-sitter for code folding where supported.")

(defvar tree-sitter-fold-indicators-enable t
  "If non nil, and `tree-sitter-fold-enable' is non nil, show fold indicators in fringe.")

(defvar spacemacs-tree-sitter-hl-black-list nil
  "List of major modes where `tree-sitter-hl-mode' is disabled.")
