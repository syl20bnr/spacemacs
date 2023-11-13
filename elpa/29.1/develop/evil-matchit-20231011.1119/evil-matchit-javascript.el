;;; evil-matchit-javascript.el --- simple match plugin of evil-matchit

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

(require 'evil-matchit-sdk)

;; should try next howto, the purpose is avoid missing any howto
(defvar evilmi-javascript-extract-keyword-howtos
  '(("const .* *= *\\(styled\\)[^`]*` *$" 1) ; styled component
    ("^[ \t]*\\(`\\);? *$" 1)))

(defvar evilmi-javascript-match-tags
  '((("styled") () "`")))

(defvar evilmi-javascript-matching-chars
  (string-to-list "{[(}}])"))

;; javascript code: "(function(...) { ..."
;; C code: "} else {"
;; React JS code: " return ("
;; line could ends with C++ or C comment
(defvar evilmi-javascript-open-brace-pattern
  "^[ \t]*[(}]?[$_a-zA-Z0-9]+.*\\([{(\[]\\)[ \t]*\\(\/\/.*\\|\/\*[^/]*\*\/\\)?$"
  "Pattern to match line which ends with brace or bracket character.")

(defun evilmi--javascript-find-open-brace (cur-line)
  "Find open brace from CUR-LINE."
  (let* (rlt)
    (cond
     ((string-match evilmi-javascript-open-brace-pattern
                    cur-line)
      (setq rlt (list 1 (match-string 1 cur-line))))

     (t
      (save-excursion
        (forward-line)
        (if (string-match "^[ \t]*{[ \t]*$" (evilmi-sdk-curline))
            (setq rlt (list 2 "{"))))))
    rlt))

;;;###autoload
(defun evilmi-javascript-get-tag ()
  "Get tag at point."
  ;; only handle open tag
  (when evilmi-debug
    (message "evilmi-javascript-get-tag called"))
  (let* (rlt)
    (cond
     ;; bracket
     ((memq (following-char)
            evilmi-javascript-matching-chars)
      (when evilmi-debug
        (message "evilmi-javascript-get-tag. following char=%s is in `evilmi-javascript-matching-chars'"
                 (following-char)))
      (setq rlt (list (point))))

     ;; use defined tag
     ((setq rlt (evilmi-sdk-get-tag evilmi-javascript-match-tags
                                    evilmi-javascript-extract-keyword-howtos))

      (when evilmi-debug
        (message "evilmi-javascript-get-tag. current line has tag=%s in `evilmi-javascript-extract-keyword-howtos'"
                 rlt)))

     ;; other javascript statements containing brackets
     (t
      (let* ((r (evilmi--javascript-find-open-brace (evilmi-sdk-curline)))
             (p (line-beginning-position)))
        (when r
          (when evilmi-debug
            (message "evilmi-javascript-get-tag. open brace=%s" r))
          (forward-line (1- (car r)))
          (search-forward (cadr r) nil nil)
          (backward-char)
          (setq rlt (list p))))))
    rlt))

;;;###autoload
(defun evilmi-javascript-jump (info num)
  "Jump to the matching tag using INFO and NUM."
  (cond
   ((not info)
    ;; do nothing
    )
   ((evilmi-sdk-get-tag evilmi-javascript-match-tags
                        evilmi-javascript-extract-keyword-howtos)
    (evilmi-sdk-jump info
                     num
                     evilmi-javascript-match-tags
                     evilmi-javascript-extract-keyword-howtos))
   (t
    (evilmi-sdk-simple-jump)
    (let* ((cur-line (evilmi-sdk-curline)))
      ;; hack for javascript
      (if (or (string-match "^[ \t]*}\)\(.*\)\; *$" cur-line)
              (string-match "^[ \t]*}\(.*\))\; *$" cur-line)
              (string-match "^[ \t]*}\])\; *$" cur-line))
          (line-end-position)
        (1+ (point)))))))

(provide 'evil-matchit-javascript)
;;; evil-matchit-javascript.el ends here
