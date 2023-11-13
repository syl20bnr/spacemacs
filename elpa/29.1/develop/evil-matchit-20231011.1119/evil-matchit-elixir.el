;;; evil-matchit-elixir.el --- elixir plugin of evil-matchit

;; Copyright (C) 2018-2020 Chen Bin

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

;; {{ Sample elixir code:
;; defmodule MyValidator do
;;   use Validator
;;   validate_length :name, 1..100
;;   validate_matches :email, ~r/@/
;; end
;; }}

;; should try next howto, the purpose is avoid missing any howto
(defvar evilmi-elixir-extract-keyword-howtos
  '(("^.*\\(do\\)[ \t]*$" 1)
    ("^[ \t]*\\(fn\\)[( \t]?" 1)
    ("^[ \t]*\\(end\\|else\\)[ \t]*$" 1)))

(defvar evilmi-elixir-match-tags
  '((("fn") () "end")
    (("do") ("else") "end")))

;;;###autoload
(defun evilmi-elixir-get-tag ()
  (let* ((cur-line (evilmi-sdk-curline))
         rlt)
    (cond
     ((string-match-p "^[ \t]*fn.*end$" cur-line)
      (setq rlt (if (string= (thing-at-point 'symbol) "end")
                    (line-beginning-position)
                  (line-end-position))))
     (t
      (setq  rlt (evilmi-sdk-get-tag evilmi-elixir-match-tags
                                     evilmi-elixir-extract-keyword-howtos))))
    rlt))

;;;###autoload
(defun evilmi-elixir-jump (rlt num)
  (cond
   ((integerp rlt)
    (goto-char rlt))
   (t
    (evilmi-sdk-jump rlt
                     num
                     evilmi-elixir-match-tags
                     evilmi-elixir-extract-keyword-howtos))))

(provide 'evil-matchit-elixir)
;;; evil-matchit-elixir.el ends here
