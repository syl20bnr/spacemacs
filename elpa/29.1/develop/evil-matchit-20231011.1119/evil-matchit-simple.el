;;; evil-matchit-simple.el --- simple match plugin of evil-matchit

;; Copyright (C) 2014-2021 Chen Bin


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

(defvar evilmi-always-simple-jump nil
  "`major-mode' like `python-mode' use optimized algorithm by default.
If it's t, use simple jump.")

(defvar evilmi-simple-supported-major-modes
  '(java-mode c-mode c++-mode go-mode js-mode cperl-mode perl-mode)
  "Major mode the simple rule supports.")

(defun evilmi-simple-open-tag-p (token)
  "TOKEN is the open tag."
  (and (eq (car token) 'semantic-list)
       (string= "{"
                (buffer-substring-no-properties (cadr token)
                                                (1+ (cadr token))))))

(defun evilmi--simple-find-open-brace (cur-line)
  "Find open brace from CUR-LINE."
  (if evilmi-debug (message "evilmi--simple-find-open-brace called => cur-line=%s (point)=%d" cur-line (point)))
  (cond

   ;; code: function(...) { ...
   ;; code: } else {'
   ;; code: "jsonField": {
   ;; Please note css-mode use characters ".:-"
   ((string-match "^[ \t]*[\(\}]?[.:_a-zA-Z0-9\"-]+.*{ *\\(\/\/.*\\|\/\*[^/]*\*\/\\)?$" cur-line)
    (evilmi-forward-and-locate 1))

   ;; code "} if (...) {"
   ;; code "} else (...) {"
   ((and (string-match "^[ \t]*[\}]? \\(if\\|el[a-z]*\\) *.*{ *?$" cur-line)
         (not (eq (following-char) ?})))
    (evilmi-forward-and-locate 1))

   ;; If current line has text in `font-lock-function-name-face' and it's C like
   ;; programming language, find the open brace character in next lines
   ((and (apply 'derived-mode-p evilmi-simple-supported-major-modes)
         (evilmi-sdk-defun-p))
    (let* ((tokens (evilmi-sdk-tokens 5))
           tag
           info)
      (when (and tokens
                 (> (length tokens) 1)
                 (setq tag
                       (cl-find-if #'evilmi-simple-open-tag-p (cdr tokens))))
        (goto-char (cadr tag))
        ;; find the bracket
        (list (cadr tag)))))

   ;; next line is "{"
   (t
    (let ((found (save-excursion
                   (forward-line)
                   (string-match "^[ \t]*{ *$" (evilmi-sdk-curline)))))
      (when found
        (evilmi-forward-and-locate 1))))))

(defun evilmi--char-is-simple (ch)
  "Special handling of character CH for `python-mode'."
  (cond
   ((and (not evilmi-always-simple-jump)
         (memq major-mode '(python-mode))
         ;; in evil-visual-state, (point) could equal to (line-end-position)
         (>= (point) (1- (line-end-position))))
    ;; handle follow python code,
    ;;
    ;; if true:
    ;;     a = "hello world"
    ;;
    ;; If current cursor is at end of line, rlt should be nil!
    ;; or else, matching algorithm can't work in above python sample
    nil)
   (t
    (or (memq ch evilmi-forward-chars)
        (memq ch evilmi-backward-chars)
        ;; sorry we could not jump between ends of string in python-mode
        (memq ch evilmi-quote-chars)))))

(defun evilmi-simple-following-char ()
  "Get the character at point or find matching tag start point nearby."
  (let* ((b (line-beginning-position))
         (whitespaces '(9 10 13 32))
         new-pos)

    (save-excursion
      (let* (ch found)
        (while (and (setq ch (following-char))
                    (> (point) b)
                    (not (setq found (or (memq ch evilmi-backward-chars)
                                         (memq ch evilmi-forward-chars))))
                    (memq ch whitespaces))
          (goto-char (1- (point))))
        (if found (setq new-pos (point)))))

    (if new-pos (goto-char new-pos))

    (following-char)))

(defun evilmi-forward-and-locate (n)
  "Forward N lines and locate the open tag character."
  (let ((rlt (list (line-beginning-position))))
    ;; need handle case "if () \n { ... }".
    ;; move cursor over "{", prepare for `evil-jump-item'
    (forward-line (1- n))
    (search-forward "{" nil nil)
    (backward-char)
    rlt))

;;;###autoload
(defun evilmi-simple-get-tag ()
  "Get current tag in simple language."
  (let* (forward-line-num
         (ch (evilmi-simple-following-char))
         rlt)

    (cond
     ;; In `evil-visual-state', (preceding-char) is the character under cursor
     ((not (evilmi--char-is-simple ch))
      (evilmi--simple-find-open-brace (evilmi-sdk-curline)))

     ((and (memq ch evilmi-quote-chars)
           (eq ch ?/)
           (not (eq ?* (evilmi-sdk-get-char (1- (point)))))
           (not (eq ?* (evilmi-sdk-get-char (1+ (point))))))
      ;; character at point is not "/*" or "*/"
      (setq rlt nil))

     (t
      ;; use evil's own evilmi-sdk-simple-jump
      (setq rlt (list (point)))))

    (if (and evilmi-debug rlt) (message "evilmi-simple-get-tag called char=%s => %s" ch rlt))
    rlt))

;;;###autoload
(defun evilmi-simple-jump (info num)
  "Use INFO of current tag to jump to matching tag.  NUM is ignored."
  (ignore num)
  (when evilmi-debug
    (message "evilmi-simple-jump: info=%s (point)=%s" info (point)))
  (when info
    ;; In latex-mode `scan-sexps' does NOT work properly between "[]"
    ;; so we have to fallback to evil's API.
    (cond
     ((and (memq major-mode '(latex-mode))
           (functionp 'evil-jump-item))
      ;; I'm sorry for non-evil users
      (funcall 'evil-jump-item))

     (t
      (evilmi-sdk-simple-jump)))

    ;; hack for javascript
    (cond
     ((string-match "^[ \t]*})\\((.*)\\)?\; *$"
                    (evilmi-sdk-curline))
      (line-end-position))
     (t
      (1+ (point))))))

(provide 'evil-matchit-simple)
;;; evil-matchit-simple.el ends here
