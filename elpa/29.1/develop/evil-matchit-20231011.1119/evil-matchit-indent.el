;;; evil-matchit-indent.el --- indent algorithm for evil-matchit

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

(defvar evilmi-indent-open-tag-regexp
  "^[ \t]*\\([a-zA-Z]+\\) *.*:[ \t]*\\(#.*\\)?$"
  "Open tag regexp for algorithm based on indent.")

(defvar evilmi-indent-first-tag-function nil
  "Function to test if current line is the first open tag.
Tag keyword from initial position is the only parameter.
It returns regexp to match the line candidate.")

(defvar evilmi-indent-next-tag-function nil
  "Function to test if current line is the next open tag.
Tag keyword from initial position is the only parameter.
It returns regexp to match the line candidate.")

(defvar evilmi-spaces-per-tab 4
  "Number of spaces of one tab character.")

(defun evilmi-indent-next-nonempty-line ()
  "Return next non-empty line content or nil."
  (let* ((b (line-beginning-position))
         (e (line-end-position))
         (cur-pos (point))
         (continue t)
         line
         rlt)
    (save-excursion
      (forward-line)
      (while (and continue (> (point) e))
        (setq line (evilmi-sdk-curline))
        (cond
         ((string-blank-p line)
          (setq b (line-beginning-position))
          (setq e (line-end-position))
          (forward-line))
         (t
          (setq continue nil)
          (setq rlt line)))))
    rlt))

(defun evilmi-indent-tab-count (line)
  "Return number of tabs at the beginning of LINE.
SPACES-PER-TAB defines the number of spaces of one tab character."
  (cond
   ((string-match "^[ \t]*$" line)
    ;; empty line
    most-positive-fixnum)

   ((string-match "^\\([ \t]+\\).*$" line)
    (let ((prefix (match-string 1 line)))
      ;; test if the first character of line is whitespace character
      ;; please note ascii code of TAB character is 9
      (if (= (elt prefix 0) 9)
          (length prefix)
        (/ (length prefix) evilmi-spaces-per-tab))))
   (t
    ;; line beginning of first column
    0)))

;;;###autoload
(defun evilmi-indent-get-tag ()
  "Return '(start-position tag-type keyword)."
  (let* (rlt
         (cur-line (evilmi-sdk-curline))
         next-line)

    (cond
     ((string-match evilmi-indent-open-tag-regexp cur-line)
      ;; we are at open tag now, and will jump forward
      (setq rlt (list (line-beginning-position)
                      0
                      (match-string 1 cur-line))))

     ((or (not (setq next-line (evilmi-indent-next-nonempty-line)))
          (< (evilmi-indent-tab-count next-line)
             (evilmi-indent-tab-count cur-line)))
      ;; double check next line to make sure current line is close tag
      ;; if indent tab count in next line is less than current line
      ;; or next line is empty line, then current line is closed tag
      (setq rlt (list (line-end-position) 1 "")))

     (t
      (message "next-line=%s" next-line)
      (message "(evilmi-indent-tab-count next-line)=%s" (evilmi-indent-tab-count next-line))
      (message "(evilmi-indent-tab-count cur-line)=%s" (evilmi-indent-tab-count cur-line))
      (setq rlt nil)))

    rlt))

;;;###autoload
(defun evilmi-indent-extract-keyword (line)
  "Extract keyword from LINE."
  (let* (keyword)
    (when (string-match evilmi-indent-open-tag-regexp line)
      (setq keyword (match-string 1 line)))
    keyword))

(defun evilmi-indent-back-to-first-tag (cur-indent)
  "Jump to the open tag based on CUR-INDENT.
For example, jump from the tag \"finally\" to \"try\"."
  (let* (out-of-loop
         where-to-go
         (cur-line (evilmi-sdk-curline))
         (keyword (evilmi-indent-extract-keyword cur-line))
         regexp)

    (setq regexp
          (and evilmi-indent-first-tag-function
               (funcall evilmi-indent-first-tag-function keyword)))

    (when evilmi-debug
      (message "evilmi-indent-back-to-first-tag called. keyword=%s regexp=%s cur-line=%s"
               keyword regexp cur-line))

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

(defun evilmi-indent-goto-next-tag (keyword cur-indent)
  "Move to next open tag using KEYWORD and CUR-INDENT."
  (let* (out-of-loop
         where-to-go
         regexp
         cur-line)

    (setq regexp
          (and evilmi-indent-next-tag-function
               (funcall evilmi-indent-next-tag-function keyword)))

    (when evilmi-debug
      (message "evilmi-indent-goto-next-tag called. keyword=%s regexp=%s" keyword regexp))
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
(defun evilmi-indent-jump (info)
  "Use INFO from `evilmi-indent-get-tag' to jump to matched tag."
  (let* ((p (nth 0 info))
         (tag-type (nth 1 info))
         (keyword (nth 2 info))
         (cur-line (evilmi-sdk-curline))
         (cur-indent (evilmi-indent-tab-count cur-line))
         dendent
         rlt)

    (when evilmi-debug
      (message "evilmi-indent-jump called. tag-type=%d position=%d" tag-type p))

    (cond
     ;; start from closed tag
     ((=  1 tag-type)

      ;; jump back to first open tag when current indent is NOT zero
      (unless (= cur-indent 0)
        (goto-char p)
        (while (not dendent)
          (forward-line -1)
          ;; first line
          (setq cur-line (evilmi-sdk-curline))

          (if evilmi-debug (message "cur-line=%s" cur-line))

          ;; skip empty lines
          (when (and (not (string-match "^[ \t]*$" cur-line))
                     (< (evilmi-indent-tab-count cur-line) cur-indent))
            (setq dendent t)
            (skip-chars-forward " \t")
            (evilmi-indent-back-to-first-tag (1- cur-indent))
            (setq rlt (point))))))

     ;; start from open tag
     ((=  0 tag-type)
      ;; jump to closed tag
      (while (not dendent)
        (forward-line)
        (setq cur-line (evilmi-sdk-curline))

        ;; skip empty line
        (unless (string-match "^[ \t]*$" cur-line)
          (cond
           ;; search forward for the first line with same/less indent
           ((<= (evilmi-indent-tab-count cur-line) cur-indent)
            ;; stop the loop, now rlt has the position of last line which
            ;; indents more then original line
            (setq dendent t))
           (t
            ;; record the previous line which indents more than original line
            (setq rlt (line-end-position)))))

        ;; stop the loop at the end of the buffer
        (when (= (point-max) (line-end-position))
          (setq dendent t)))

      (if rlt (goto-char rlt))

      (evilmi-indent-goto-next-tag keyword cur-indent)))

    rlt))

(provide 'evil-matchit-indent)
;;; evil-matchit-indent.el ends here
