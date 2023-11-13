;;; evil-collection-debug.el --- Evil bindings for the debugger -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, debug, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for the debugger.

;;; Code:

(require 'evil-collection)
(require 'debug)

(defconst evil-collection-debug-maps '(debugger-mode-map))

;;;###autoload
(defun evil-collection-debug-setup ()
  "Set up `evil' bindings for `debug'."
  (evil-set-initial-state 'debugger-mode 'normal)

  (evil-collection-define-key 'normal 'debugger-mode-map
    ;; motion
    (kbd "<tab>") 'forward-button
    (kbd "S-<tab>") 'backward-button
    (kbd "RET") (if (fboundp 'debug-help-follow)
                    'debug-help-follow
                  'backtrace-help-follow-symbol)
    (kbd "SPC") 'next-line

    "R" 'debugger-record-expression
    "c" 'debugger-continue
    "d" 'debugger-step-through

    "x" 'debugger-eval-expression
    "E" 'debugger-eval-expression

    "J" 'debugger-jump

    "gl" 'debugger-list-functions
    "gb" 'debugger-frame
    "r" 'debugger-return-value
    "u" 'debugger-frame-clear
    "L" (if (fboundp 'debugger-toggle-locals)
            'debugger-toggle-locals
          'backtrace-toggle-locals)
    "p" (if (fboundp 'debugger-toggle-locals)
            'debugger-toggle-locals
          'backtrace-toggle-locals)
    
    "zo" 'backtrace-multi-line
    "zc" 'backtrace-single-line
    
    ;; quit
    "q" 'top-level
    "ZQ" 'evil-quit
    "ZZ" 'top-level))

(provide 'evil-collection-debug)
;;; evil-collection-debug.el ends here
