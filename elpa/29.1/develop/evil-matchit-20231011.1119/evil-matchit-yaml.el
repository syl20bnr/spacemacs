;;; evil-matchit-yaml.el --- yaml plugin of evil-matchit

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

(require 'evil-matchit-indent)

;;;###autoload
(defun evilmi-yaml-get-tag ()
  "Return '(start-position tag-type keyword)."
  (let* ((evilmi-spaces-per-tab 2)
         (rlt (evilmi-indent-get-tag)))
    (when (and evilmi-debug rlt)
      (message "evilmi-yaml-get-tag called. rlt=%s" rlt))
    rlt))

;;;###autoload
(defun evilmi-yaml-jump (info num)
  "Use INFO returned by `evilmi-yaml-get-tag' and NUM to jump to matched tag."
  (ignore num)
  (if evilmi-debug (message "evilmi-yaml-jump called => %s" info))

  (let* ((evilmi-spaces-per-tab 2)
         (rlt (evilmi-indent-jump info)))
    rlt))

(provide 'evil-matchit-yaml)
;;; evil-matchit-yaml.el ends here
