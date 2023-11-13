;;; evil-matchit-octave.el --- octave plugin of evil-matchit

;; Copyright (C) 2020 Chen Bin

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

(defvar evilmi-octave-extract-keyword-howtos
  '(("^[ \t]*\\([a-zA-Z]+\\)[ \t]*" 1)))

;; Octave (http://www.octave.org) syntax
(defvar evilmi-octave-match-tags
  '((("if" "for" "while" "switch" "function") ("elseif" "else" "case" "otherwise") ("end"))))

;;;###autoload
(defun evilmi-octave-get-tag ()
  "Get current tag info."
  (evilmi-sdk-get-tag evilmi-octave-match-tags
                      evilmi-octave-extract-keyword-howtos))

;;;###autoload
(defun evilmi-octave-jump (info num)
  "Use INFO returned by `evilmi-octave-get-tag' and NUM to jump to matched tag."
  (evilmi-sdk-jump info
                   num
                   evilmi-octave-match-tags
                   evilmi-octave-extract-keyword-howtos))

(provide 'evil-matchit-octave)
;;; evil-matchit-octave.el ends here
