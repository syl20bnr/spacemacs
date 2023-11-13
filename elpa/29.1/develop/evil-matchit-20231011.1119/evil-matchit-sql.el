;;; evil-matchit-sql.el --- sql plugin of evil-matchit

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
;; but SDK do make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'evil-matchit-sdk)

;; copied from sql.vim
;;  Handle the following:
;;  if
;;  elseif | elsif
;;  else [if]
;;  end if
;;
;;  [while condition] loop
;;      leave
;;      break
;;      continue
;;      exit
;;  end loop
;;
;;  for
;;      leave
;;      break
;;      continue
;;      exit
;;  end loop
;;
;;  do
;;      statements
;;  doend
;;
;;  case
;;  when
;;  when
;;  default
;;  end case
;;
;;  merge
;;  when not matched
;;  when matched
;;
;;  EXCEPTION
;;  WHEN column_not_found THEN
;;  WHEN OTHERS THEN
;;
;;  create[ or replace] procedure|function|event
    ;;   '^[ \t]*\<\%(do\|for\|while\|loop\)\>.*:'.
;; TODO for one howto, if it cannot match any keyword,
;; should try next howto, the purpose is avoid missing any howto
(defvar evilmi-sql-extract-keyword-howtos
  '(("^[ \t]*\\([a-zA-Z]+[ \t][a-zA-Z]+\\)" 1)
    ("^[ \t]*\\([a-zA-Z]+\\)" 1)
    ("^.*[ \t]\\(loop\\)[;]?[ \t]*$" 1)))

(defvar evilmi-sql-match-tags
  '(("if" ("elsif" "else" "elseif" "else *if") ("end" "end *if"))
    (("loop") ("leave" "break" "continue" "exit") ("end loop"))
    ("begin" () "end")
    ("case" ("when *others") ("end *case" "end"))
    (("do") () "do *end")))

;;;###autoload
(defun evilmi-sql-get-tag ()
  "Get tag at point."
  (evilmi-sdk-get-tag evilmi-sql-match-tags evilmi-sql-extract-keyword-howtos))

;;;###autoload
(defun evilmi-sql-jump (info num)
  "Use INFO returned by `evilmi-sql-get-tag' and NUM to jump to matched tag."
  (evilmi-sdk-jump info num evilmi-sql-match-tags evilmi-sql-extract-keyword-howtos))

(provide 'evil-matchit-sql)
;;; evil-matchit-sql.el ends here
