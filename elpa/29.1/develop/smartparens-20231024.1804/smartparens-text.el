;;; smartparens-latex.el --- Additional configuration for text-mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 16 July 2017
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

;; This file provides some additional configuration for `text-mode'.
;; To use it, simply add:
;;
;; (require 'smartparens-text)
;;
;; into your configuration.  You can use this in conjunction with the
;; default config or your own configuration.

;; If you have good ideas about what should be added please file an
;; issue on the github tracker.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:

(require 'smartparens)

(defun sp-text-mode-emoticon-p (_id action _context)
  (when (memq action '(insert navigate))
    (sp--looking-back-p ":-?[()]" 3)))

(defun sp-text-mode-skip-emoticon (ms mb _me)
  (when (member ms '("(" ")"))
    (save-excursion
      (goto-char mb)
      (sp--looking-back-p ":-?" 2))))

(sp-local-pair 'text-mode "(" nil
               :unless '(:add sp-text-mode-emoticon-p)
               :skip-match 'sp-text-mode-skip-emoticon)

(provide 'smartparens-text)
;;; smartparens-text.el ends here
