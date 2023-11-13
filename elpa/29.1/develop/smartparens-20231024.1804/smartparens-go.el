;;; smartparens-go.el --- Additional configuration for go-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jimmy Yuen Ho Wong

;; Created: 10 June 2022
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for Go.  To use
;; it, simply add:
;;
;; (require 'smartparens-config)
;;
;; alternatively, you can explicitly load these preferences:
;;
;; (require 'smartparens-go)
;;
;; in your configuration.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:
(require 'smartparens)

(declare-function go-mode "go-mode")

(sp-with-modes 'go-mode
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '(("| " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; Go has no sexp suffices.  This fixes slurping
;; (|foo).bar -> (foo.bar)
(add-to-list 'sp-sexp-suffix (list #'go-mode 'regexp ""))

(provide 'smartparens-go)

;;; smartparens-go.el ends here
