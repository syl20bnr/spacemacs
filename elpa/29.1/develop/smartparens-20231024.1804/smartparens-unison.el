;;; smartparens-unison.el --- Additional configuration for Unison -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph Morag

;; Author: Joseph Morag <jm@josephmorag.com>
;; Maintainer: Joseph Morag <jm@josephmorag.com>
;; Created: 04 December 2022
;; Keywords: smartparens, unison
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

;; This file provides some additional configuration for Unison.
;; To use it, simply add:
;;
;; (require 'smartparens-unison)
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

(sp-with-modes '(unisonlang-mode)
  ;; Disable ' because it is used as a shortcut to wrap a value in
  ;; lambda () -> value
  ;; to make it lazy
  (sp-local-pair "'" nil :actions nil))

(provide 'smartparens-unison)
;;; smartparens-unison.el ends here
