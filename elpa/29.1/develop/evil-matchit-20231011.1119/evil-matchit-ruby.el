;;; evil-matchit-ruby.el --- ruby plugin of evil-matchit

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

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK do make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
;;
;;; Commentary:
;;
;;; Code:

(require 'evil-matchit-sdk)

(defvar evilmi-ruby-extract-keyword-howtos
  '(("^[ \t]*[^ \t=]+[ \t]*=[ \t]*\\([a-z]+\\)\\([ \t].*\\|(.*\\|[ \t]*\\)$" 1)
    ("^[ \t]*\\([a-z]+\\)\\([ \t].*\\|(.*\\|[ \t]*\\)$" 1)
    ("^.* \\(do\\)[ \t]|[a-z0-9A-Z_, *]+|[ \t]*$" 1)
    ("^.* \\(do\\)[ \t]*$" 1)
    ("^.* \\(begin\\)[ \t]*$" 1)
    ("^.* \\(end\\)\\..*$" 1)))

(defvar evilmi-ruby-match-tags
  '((("unless" "if") ("elsif" "else") "end")
    ("begin" ("rescue" "ensure") "end")
    ("case" ("when" "else") "end")
    (("class" "def" "while" "do" "module" "for" "until") () "end" "ENDLESS")))

;;;###autoload
(defun evilmi-ruby-get-tag ()
  "Get tag at point."
  (evilmi-sdk-get-tag evilmi-ruby-match-tags evilmi-ruby-extract-keyword-howtos))

;;;###autoload
(defun evilmi-ruby-jump (info num)
  "Use INFO to jump NUM times."
  (evilmi-sdk-jump info num evilmi-ruby-match-tags evilmi-ruby-extract-keyword-howtos))

(provide 'evil-matchit-ruby)
;;; evil-matchit-ruby.el ends here
