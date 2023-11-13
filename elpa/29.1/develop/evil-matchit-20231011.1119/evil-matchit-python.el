;;; evil-matchit-python.el --- python plugin of evil-matchit

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

(defun evilmi-python-back-to-first-tag (cur-indent)
 "Jump to the open tag based on CUR-INDENT.
For example, jump from the tag \"finally\" to \"try\".
Only python need this hack."
  (let* (out-of-loop
         where-to-go
         regexp
         (cur-line (evilmi-sdk-curline))
         (keyword (evilmi-indent-extract-keyword cur-line)))

    (cond
     ((string= keyword "else")
      (setq regexp "^[ \t]*\\(if\\) *.*:[ \t]*\\(#.*\\)?$"))

     ((or (string= keyword "finally") (string= keyword "except"))
      (setq regexp "^[ \t]*\\(try\\) *.*:[ \t]*\\(#.*\\)?$")))

    (when regexp
      (save-excursion
        (while (not out-of-loop)
          (forward-line -1)
          (setq cur-line (evilmi-sdk-curline))

          (when (and (= cur-indent (evilmi-indent-tab-count cur-line))
                     (string-match regexp cur-line))
            (setq where-to-go (line-beginning-position))
            (setq out-of-loop t))

          ;; if it's first line, we need get out of loop
          (if (= (point-min) (line-beginning-position))
              (setq out-of-loop t))))
      (when where-to-go
        (goto-char where-to-go)
        (skip-chars-forward " \t")))))

(defun evilmi-python-goto-next-tag (keyword cur-indent)
  "Move to next open tag using KEYWORD and CUR-INDENT."
  (let* (out-of-loop
         where-to-go
         regexp
         cur-line)

    (cond
     ((string= keyword "try")
      (setq regexp "^[ \t]*\\(except\\) *.*:[ \t]*\\(#.*\\)?$"))

     ((string= keyword "except")
      (setq regexp "^[ \t]*\\(except\\|finally\\) *.*:[ \t]*\\(#.*\\)?$"))

     ((or (string= keyword "elif") (string= keyword "if"))
      (setq regexp "^[ \t]*\\(elif\\|else\\) *.*:[ \t]*\\(#.*\\)?$")))

    (save-excursion
      (while (not out-of-loop)
        (forward-line)
        (setq cur-line (evilmi-sdk-curline))

        (when (= cur-indent (evilmi-indent-tab-count cur-line))
          (when (and regexp (string-match regexp cur-line))
            (setq where-to-go (line-beginning-position)))
          (setq out-of-loop t))
        ;; if it's last line, we need get out of loop
        (when (= (point-max) (line-end-position))
          (setq out-of-loop t))))

    (when where-to-go
      (goto-char where-to-go)
      (skip-chars-forward " \t"))))

;;;###autoload
(defun evilmi-python-get-tag ()
  "Return '(start-position tag-type keyword)."
  (let ((rlt (evilmi-indent-get-tag)))
    (when evilmi-debug
      (message "evilmi-python-get-tag called. rlt=%s" rlt))

    rlt))

(defun evilmi-python-match-first-tag (keyword)
  "Generate first tag's regexp from KEYWORD."
  (cond
   ((string= keyword "else")
    "^[ \t]*\\(if\\) *.*:[ \t]*\\(#.*\\)?$")

   ((or (string= keyword "finally")
        (string= keyword "except"))
    "^[ \t]*\\(try\\) *.*:[ \t]*\\(#.*\\)?$")))

(defun evilmi-python-match-next-tag (keyword)
  "Generate next tag's regexp from KEYWORD."
  (cond
   ((string= keyword "try")
    "^[ \t]*\\(except\\) *.*:[ \t]*\\(#.*\\)?$")

   ((string= keyword "except")
    "^[ \t]*\\(except\\|finally\\) *.*:[ \t]*\\(#.*\\)?$")

   ((or (string= keyword "elif") (string= keyword "if"))
    "^[ \t]*\\(elif\\|else\\) *.*:[ \t]*\\(#.*\\)?$")))

;;;###autoload
(defun evilmi-python-jump (info num)
  "Use INFO from `evilmi-python-get-tag' to jump to matched tag.
NUM is ignored."
  (ignore num)
  (let* ((evilmi-indent-first-tag-function 'evilmi-python-match-first-tag)
         (evilmi-indent-next-tag-function 'evilmi-python-match-next-tag)
         (rlt (evilmi-indent-jump info)))

    (when evilmi-debug
      (message "evilmi-python-jump called. rlt=%s" rlt))
    rlt))

(provide 'evil-matchit-python)
;;; evil-matchit-python.el ends here
