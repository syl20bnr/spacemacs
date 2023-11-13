;;; evil-matchit-markdown.el --- markdown-mode plugin of evil-matchit

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


;;; Code:

(require 'evil-matchit-sdk)

;;;###autoload
(defun evilmi-markdown-get-tag ()
  "Get current tag.  Return (list start-position tag)."
  (when (string-match "^\\(```\\)" (evilmi-sdk-curline))
    (let* ((str "```")
           (text (evilmi-sdk-text-before-current-line))
           (forward-p (eq (% (evilmi-sdk-count-matches str text) 2) 0)))
      (list (if forward-p (line-beginning-position) (line-end-position))
            forward-p
            str))))

;;;###autoload
(defun evilmi-markdown-jump (info num)
  "Jump to the next tag using INFO and NUM."
  (ignore num)
  (let* ((forward-p (nth 1 info))
         (str (nth 2 info))
         (pos (point))
         (rlt pos))
    (when (string= str "```")
      (cond
       (forward-p
        ;; jump forward
        (goto-char (+ pos (length str)))
        (search-forward str)
        (setq rlt (line-end-position)))
       (t
        ;; jump backward
        (goto-char (- pos (length str)))
        (search-backward str)
        (setq rlt (line-beginning-position)))))
    rlt))

(provide 'evil-matchit-markdown)
