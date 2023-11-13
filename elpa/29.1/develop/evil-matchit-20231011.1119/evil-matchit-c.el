;;; evil-matchit-c.el -- C like language (c/c++/perl/java/javascript) plugin of evil-matchit

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

;;; Code:
(require 'evil-matchit-sdk)

(defvar evilmi-c-match-tags
  '((("# *ifdef" "# *ifndef" "# *if") ("# *elif" "# *else")  "# *endif" "MONOGAMY")
    ("switch" "case" "default" "MONOGAMY")))

(defvar evilmi-c-extract-keyword-howtos
  '(("^[ \t]*\\(# *[a-z]+\\)" 1)
    ("^[ \t]*\\([a-z]+\\)\\([ (:].*\\| *\\)$" 1)))

;;;###autoload
(defun evilmi-c-get-tag ()
  "Get tag at point."
  (evilmi-sdk-get-tag evilmi-c-match-tags
                      evilmi-c-extract-keyword-howtos))

;;;###autoload
(defun evilmi-c-jump (info num)
  "Use INFO to jump NUM times."
  (let* ((old-pos (point))
         (new-pos (evilmi-sdk-jump info
                               num
                               evilmi-c-match-tags
                               evilmi-c-extract-keyword-howtos))
         (orig-tag (and info (nth 3 (cadr info)))))

    ;; Place cursor over last case of 'switch' statement and press %:
    ;; Should go back to beginning of switch:
    ;;   switch(c) {
    ;;   case 'a':
    ;;       break;
    ;;   case 'b':
    ;;       break;
    ;;   }
    (when (and (string= orig-tag "case")
               ;; failed to find matching tag
               (not new-pos))
      (goto-char old-pos)
      ;; Goto outer bracket
      (backward-up-list)
      (setq new-pos (beginning-of-line)))
    new-pos))

(provide 'evil-matchit-c)
;;; evil-matchit-c.el ends here
