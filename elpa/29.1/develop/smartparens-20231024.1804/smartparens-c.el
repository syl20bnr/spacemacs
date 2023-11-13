;;; smartparens-c.el --- Additional configuration for C/C++ mode.  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019-2020, 2022 Naoya Yamashita, Matus Goljer
;;
;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 23 June 2019
;; Keywords: abbrev convenience editing
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
;; This file provides some additional configuration for C/C++ mode.
;; To use it, simply add:
;;
;; (require 'smartparens-c)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;;; Code:

(require 'smartparens)

;; remap electric delete functions to smartparens function
(define-key smartparens-strict-mode-map [remap c-electric-delete-forward] 'sp-delete-char)
(define-key smartparens-strict-mode-map [remap c-electric-backspace] 'sp-backward-delete-char)

(sp-with-modes sp-c-modes
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; inline formulas for doxygen
(sp-with-modes sp-c-modes
  (sp-local-pair "\\f[" "\\f]" :when '(sp-in-comment-p))
  (sp-local-pair "\\f$" "\\f$" :when '(sp-in-comment-p)))

(provide 'smartparens-c)
;;; smartparens-c.el ends here
