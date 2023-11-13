;;; evil-matchit-script.el --- script (vimrc/lua) plugin of evil-matchit

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

;;; Commentary:
;;

;;; Code:

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK do make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'evil-matchit-sdk)

(defvar evilmi-script-match-tags
  '((("unless" "if") ("elif" "elsif" "elseif" "else") ("end" "fi" "endif"))
    ("begin" ("rescue" "ensure") "end")
    ("case" ("when" "else") ("esac" "end"))
    ("for" () "end")
    (("fun!" "function!" "class" "def" "while" "function" "do") () ("end" "endfun" "endfunction"))
    ("repeat" ()  "until")))

(defvar evilmi-script-extract-keyword-howtos
  '(("^.*\\(=\\|local[ \t]\\)[ \t]*\\(function\\)[ \t]*.*$" 2)
    ;; lua code: "local thread = coroutine.create(function() ... )"
    ("\\(function\\)([^()]*)[ \t]*$" 1)
    ("^[ \t]*\\([a-z]+!?\\)[)]?\\([ \t].*\\| *\\)$" 1)
    ("^.*[ \t]\\(do\\)[ \t]|[a-z0-9A-Z,|]+|$" 1)))

;;;###autoload
(defun evilmi-script-get-tag ()
  "Get tag at point."
  (evilmi-sdk-get-tag evilmi-script-match-tags evilmi-script-extract-keyword-howtos))

;;;###autoload
(defun evilmi-script-jump (info num)
  "Use INFO returned by `evilmi-script-get-tag' and NUM to jump to matched tag."
  (evilmi-sdk-jump info num evilmi-script-match-tags evilmi-script-extract-keyword-howtos))

(provide 'evil-matchit-script)
;;; evil-matchit-script.el ends here