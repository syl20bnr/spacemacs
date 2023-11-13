;;; evil-matchit-prog.el --- all programming languages plugin of evil-matchit

;; Copyright (C) 2021 Chen Bin

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

(require 'evil-matchit-sdk)

(defvar evilmi-prog-extract-keyword-howtos
  '(("^\\(<<<<<<<\\|=======\\|>>>>>>>\\)" 1)))

(defvar evilmi-prog-match-tags
  '((("<<<<<<<") ("=======") (">>>>>>>"))))

;;;###autoload
(defun evilmi-prog-get-tag ()
  "Get tag at point."
  (evilmi-sdk-get-tag evilmi-prog-match-tags evilmi-prog-extract-keyword-howtos))

;;;###autoload
(defun evilmi-prog-jump (info num)
  "Use INFO to jump NUM times."
  (evilmi-sdk-jump info num evilmi-prog-match-tags evilmi-prog-extract-keyword-howtos))

(provide 'evil-matchit-prog)
;;; evil-matchit-prog.el ends here
