;;; evil-matchit-template.el --- web template plugin of evil-matchit

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

(defvar evilmi-template-extract-keyword-howtos
  '(("^[ \t]*<\\?php +\\([a-z]+\\).*$" 1)
    ("^[ \t]*\\([@a-z]+\\).*$" 1)))

(defvar evilmi-template-match-tags
  '(("if" ("elseif" "else") "endif" "MONOGAMY")
    ("foreach" () "endforeach" "MONOGAMY")
    ("for" () "endfor" "MONOGAMY")
    ("while" () "endwhile" "MONOGAMY")
    ("@section" () ("@show" "@stop" "@overwrite") "MONOGAMY")
    ("@if" ("@elseif" "@else") "@endif" "MONOGAMY")
    ("@unless" () "@endunless")
    ("@for" () "@endfor" "MONOGAMY")
    ("@foreach" () "@endforeach" "MONOGAMY")
    ("@forelse" "@empty" "@endforelse" "MONOGAMY")
    ("@while" () "@endwhile" "MONOGAMY")))

;;;###autoload
(defun evilmi-template-get-tag ()
  "Get tag at point."
  (evilmi-sdk-get-tag evilmi-template-match-tags evilmi-template-extract-keyword-howtos))

;;;###autoload
(defun evilmi-template-jump (info num)
  "Jump to the matching tag using INFO and NUM."
  (evilmi-sdk-jump info num evilmi-template-match-tags evilmi-template-extract-keyword-howtos))

(provide 'evil-matchit-template)
;;; evil-matchit-template.el ends here
