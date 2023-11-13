;;; smartparens-rst.el --- Additional configuration for rst based modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 28th January 2019
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

;; This file provides some additional configuration for rst based
;; modes.  To use it, simply add:
;;
;; (require 'smartparens-rst)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.
;;
;; If you have good ideas about what should be added please file an
;; issue on the github tracker.
;;
;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)
(require 'smartparens-text)
(require 'smartparens-markdown)

(defun sp-rst-point-after-backtick (_id action _context)
  (when (eq action 'insert)
    (sp--looking-back-p "`_")))

(sp-with-modes 'rst-mode
  (sp-local-pair "*" "*"
                 :unless '(sp--gfm-point-after-word-p sp-point-at-bol-p)
                 :post-handlers '(("[d1]" "SPC"))
                 :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "**" "**")
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p sp-rst-point-after-backtick))
  (sp-local-pair "``" "``"))

(provide 'smartparens-rst)
;;; smartparens-rst.el ends here
