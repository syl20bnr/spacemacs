;;; evil-matchit-html.el --- html plugin of evil-matchit

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

(autoload 'sgml-skip-tag-backward "sgml-mode" nil t)
(autoload 'sgml-skip-tag-forward "sgml-mode" nil t)

(defun evilmi-html--open-tag-candidate (position)
  "Get html open tag candidate.
It starts from POSITION and possibly ends at line end."
  (let* ((partial-line (save-excursion
                         (goto-char position)
                         (buffer-substring position (line-end-position)))))
    (car (split-string partial-line "[ \t]+"))))

(defun evilmi-html--detect-self-closing-tag-end (char position)
  "Use CHAR at POSITION to test if it's the end of self closing tag."
  (when evilmi-debug
    (message "evilmi-html--detect-self-closing-tag-end called => %s %s"
             char
             position))
  (when (or (and (eq char ?>)
                 (eq (evilmi-sdk-get-char (1- position)) ?/))
            (and (eq char ?/)
                 (eq (evilmi-sdk-get-char (1+ position)) ?>)))
    (list (if (eq char ?>) position (1+ position)) 1 "")))

(defun evilmi-html--detect-normal-tags (char position)
  "Test matched tags or beginning of self closing tag.
Use CHAR at POSITION."
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (looping t)
         (found_tag -1))
    (when evilmi-debug
      (message "evilmi-html--detect-normal-tags: position=%s" position))
    (save-excursion
      ;; search backward for "<"
      (unless (eq char ?<)
        (while (and looping (<= begin (point)) (not (eq char ?<)))
          (setq char (following-char))
          (if (eq (setq position (point)) (point-min))
              ;; need get out of loop anyway
              (setq looping nil)
            (backward-char))))

      ;; search forward for "<"
      (unless (eq char ?<)
        (save-excursion
          (while (and (>= end (point))
                      (not (eq char ?<))
                      (< (point) (point-max)))
            (setq char (following-char))
            (setq position (point))
            (unless (eq (point) (point-max)) (forward-char)))))

      ;; a valid html tag should be like <[^;]
      (unless (and (eq char ?<)
                   ;; html tags should not contain " ,;"
                   (string-match "^<[^ ;,]+$" (evilmi-html--open-tag-candidate position)))
        (setq char nil))

      ;; is end tag?
      (when (and (eq char ?<) (< position end))
        (goto-char position)
        (forward-char)
        (cond
         ((eq (following-char) ?/)
          ;; </
          (skip-chars-forward "^>")
          (forward-char)
          (setq found_tag 1))
         (t
          ;; < , looks fine
          (backward-char)
          (setq found_tag 0)))
        (setq position (point))))

    (when (eq found_tag 0)
      ;; `sgml-skip-tag-forward' can't handle the open html tag whose attribute containing "<" or ">" character
      ;; unless the start position is above "<" character
      (goto-char position) ; move to the closest "<"
      (when (or (evilmi-sdk-font-p (point) evilmi-ignored-fonts)
                ;; In `rjsx-mode', the attribute value's font face could be nil
                ;; like <Component a1={3 > 2} />
                (and (eq ?< (following-char))
                     ;; if it's html tag, the character next "<" should
                     ;; have some font face
                     (not (get-text-property (1+ position) 'face))))
        ;; since current "<" is not part of open html tag,
        ;; skip backward to move cursor over the "<" of open html tag
        (sgml-skip-tag-backward 1)
        (setq position (point))))
    (list position found_tag "")))

;;;###autoload
(defun evilmi-html-get-tag ()
  "Get current tag."
  (let* ((char (following-char))
         (position (point))
         rlt)
    (if evilmi-debug (message "evilmi-html-get-tag called. position=%s" position))
    (setq rlt (or (evilmi-html--detect-self-closing-tag-end char position)
                  (evilmi-html--detect-normal-tags char position)))
    ;; restore original position
    (goto-char position)
    rlt))

;;;###autoload
(defun evilmi-html-jump (info num)
  "Use INFO from current tag to jump NUM times."
  (let* ((tag-type (nth 1 info))
         ;; `web-mode-forward-sexp' is assigned to `forward-sexp-function'
         ;; it's buggy in web-mode v11, here is the workaround
         (forward-sexp-function nil))
    (when evilmi-debug
      (message "evilmi-html-jump called. tag-type=%s info=%s" tag-type info))
    (cond
     ((eq 1 tag-type)
      (sgml-skip-tag-backward num))
     ((eq 0 tag-type)
      (sgml-skip-tag-forward num)))
    (point)))

(provide 'evil-matchit-html)
