;;; evil-matchit-sh.el --- sh (bash/zsh) plugin of evil-matchit

;; Copyright (C) 2014-2020 Chen Bin

;; Author: Chen Bin

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK don make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'evil-matchit-sdk)

(defvar evilmi-sh-match-tags
  '((("if") ("elif" "else") ("fi") "MONOGAMY")
    ("case" (";;") ("esac") "MONOGAMY")
    (("for" "do" "while" "until") () ("done"))))

(defvar evilmi-sh-extract-keyword-howtos
  '(("^[ \t]*\\([a-z]+\\)\\( .*\\| *\\)$" 1)
    ("^.*\\(;;\\) *$" 1)
    ("^\\(\} *\\)" 1)))

;;;###autoload
(defun evilmi-sh-get-tag ()
  (evilmi-sdk-get-tag evilmi-sh-match-tags evilmi-sh-extract-keyword-howtos))

;;;###autoload
(defun evilmi-sh-jump (info num)
  (evilmi-sdk-jump info num evilmi-sh-match-tags evilmi-sh-extract-keyword-howtos))

(provide 'evil-matchit-sh)
;;; evil-matchit-sh.el ends here
