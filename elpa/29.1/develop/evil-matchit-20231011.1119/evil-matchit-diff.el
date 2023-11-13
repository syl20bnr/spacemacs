;;; evil-matchit-diff.el -- diff-mode  plugin of evil-matchit

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

(defun evilmi-diff-begin ()
  "Find diff begin."
  (let* (rlt)
    (save-excursion
      (cond
       ((string-match "^diff " (evilmi-sdk-curline))
        (setq rlt (line-beginning-position)))

       ((string-match "^index " (evilmi-sdk-curline))
        (forward-line -1)
        (setq rlt (line-beginning-position)))

       (t
        ;; make sure the current line has prefix "---"
        (when (string-match "^\\+\\+\\+ " (evilmi-sdk-curline))
          (forward-line -1))
        (cond
         ((string-match "^--- " (evilmi-sdk-curline))
          (setq rlt (line-beginning-position))
          (forward-line -2))
         (t
          (forward-line -1)))
        (when (string-match "^diff " (evilmi-sdk-curline))
          (setq rlt (line-beginning-position))))))
    rlt))

(defun evilmi-diff-end ()
  "Find diff end."
  (let* (rlt)
    ;; find next diff beginning
    (save-excursion
      (when (re-search-forward "^diff " (point-max) t)
        (setq rlt (line-beginning-position))))
    (save-excursion
      (when (re-search-forward "^--- " (point-max) t)
        (when (or (not rlt) (< (line-beginning-position) rlt))
          (setq rlt (line-beginning-position)))))
    ;; calculate diff end
    (cond
     (rlt
      (save-excursion
        (goto-char rlt)
        (forward-line -1)
        (setq rlt (line-end-position))))
     (t
      (setq rlt (point-max))))
    rlt))

;;;###autoload
(defun evilmi-diff-get-tag ()
  "Get tag at point."
  (let* (pos)
    (cond
     ((string-match "^\\(\\+\\+\\+\\|---\\|diff\\|index\\) " (evilmi-sdk-curline))
      (when (setq pos (evilmi-diff-begin))
        ;; open tags
        (list pos 0)))
     (t
      (when (setq pos (evilmi-diff-end))
        ;; close tags
        (list pos 1))))))

;;;###autoload
(defun evilmi-diff-jump (info num)
  "Jump to the matching tag using INFO and NUM."
  (ignore num)
  (when info
    (let* ((pos (nth 0 info))
           (type (nth 1 info))
           dest)
      (cond
       ((eq type 0)
        (goto-char pos)
        (re-search-forward "^\\+\\+\\+ ")
        (forward-line 1)
        (when (setq dest (evilmi-diff-end))
          (goto-char dest)))

       (t
        (goto-char pos)
        (re-search-backward "^\\+\\+\\+ ")
        (when (setq dest (evilmi-diff-begin))
          (goto-char dest))))
      dest)))

(provide 'evil-matchit-diff)
;;; evil-matchit-diff.el ends here
