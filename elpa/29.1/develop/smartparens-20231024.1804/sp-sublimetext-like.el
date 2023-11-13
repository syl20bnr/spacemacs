;;; sp-sublimetext-like.el --- Behavior for inserting parentheses similar to SublimeText editor.  -*- lexical-binding: t; -*-
;;
;; Author: Konstantin Kharlamov <Hi-Angel@yandex.ru>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 16 December 2020
;; Keywords: convenience editing
;; URL: https://github.com/Fuco1/smartparens
;;
;; This file is not part of GNU Emacs.
;;
;;; License:
;;
;; This file is part of Smartparens.
;;
;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file configuation to make smartparens insertion behavae similarly to
;; SublimeText editor.  To use it, simply add:
;;
;;     (require 'sp-sublimetext-like)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;;; Code:

(require 'smartparens)

(defun sp-point-not-before-word (_ action __)
  "In insert and autoskip actions returns t when next symbol is
not a word constituent."
  (if (memq action '(insert autoskip))
      (looking-at "\\(\\Sw\\|$\\)")
    t))

(let ((when '(sp-point-not-before-word))
      (actions  '(insert wrap autoskip navigate)))
  (sp-pair "{" "}" :when when :actions actions)
  (sp-pair "[" "]" :when when :actions actions)
  (sp-pair "(" ")" :when when :actions actions))

(provide 'sp-sublimetext-like)

;;; sp-sublimetext-like.el ends here
