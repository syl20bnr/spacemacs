;;; string-edit-at-point.el --- Avoid escape nightmares by editing string in separate buffer

;; Copyright (C) 2013 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((dash "1.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Avoid escape nightmares by editing string in separate buffer

;;; Code:

(require 'dash)

(defvar se/original)
(defvar se/original-buffer)

;; Don't kill the variables if user changes major mode
(put 'se/original 'permanent-local t)
(put 'se/original-buffer 'permanent-local t)

(defvar string-edit-at-point-hook ()
  "Hook to run just before enabling `string-edit-at-point-mode'.
This hook provides an opportunity to enable a custom major mode
before the minor mode is enabled.")

(defun se/after-change-major-mode ()
  "Reenable `string-edit-at-point-mode' after major mode change."
  (string-edit-at-point-mode 1))
(put 'se/after-change-major-mode 'permanent-local-hook t)

;;;###autoload
(defun string-edit-at-point ()
  "Pop up a buffer to edit the string at point.
This saves you from needing to manually escape characters."
  (interactive)
  (when (se/point-inside-string-p)
    (let* ((p (point))
           (original-buffer (current-buffer))
           (original (se/find-original)))
      (select-window (split-window-vertically -4))
      (switch-to-buffer (generate-new-buffer "*string-edit-at-point*"))
      (insert (se/aget :raw original))
      (goto-char (- p (se/aget :beg original) -1))
      (funcall (se/aget :cleanup original))
      (enlarge-window (1- (line-number-at-pos (point-max))))
      (se/guess-at-major-mode)
      (run-hooks 'string-edit-at-point-hook)
      (string-edit-at-point-mode 1)
      (set (make-local-variable 'se/original) original)
      (set (make-local-variable 'se/original-buffer) original-buffer)
      (add-hook 'after-change-major-mode-hook 'se/after-change-major-mode nil t)
      (font-lock-fontify-buffer))))

(defun string-edit-at-point-abort ()
  "Used in string-edit-at-point-mode to close the popup window"
  (interactive)
  (kill-buffer)
  (delete-window))

(defun string-edit-at-point-conclude ()
  (interactive)
  (funcall (se/aget :escape se/original))
  (let ((p (point))
        (contents (buffer-substring-no-properties (point-min) (point-max)))
        (original se/original)
        (original-buffer se/original-buffer)
        (beg (se/aget :beg se/original)))
    (kill-buffer)
    (delete-window)
    (switch-to-buffer original-buffer)
    (goto-char beg)
    (delete-char (length (se/aget :raw original)))
    (insert "\"" contents "\"")
    (let ((end (point)))
      (goto-char (+ p beg))
      (indent-region beg end))))

(defun se/find-original ()
  (if (derived-mode-p 'js2-mode 'js-mode)
      (se/js-strings-at-point)
    (se/string-at-point)))

(defun se/guess-at-major-mode ()
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "<")
      (html-mode))))

(defun se/unescape (quote)
  (goto-char (point-min))
  (while (search-forward (concat "\\" quote) nil t)
    (replace-match "")
    (insert quote)))

(defun se/escape (quote)
  (goto-char (point-min))
  (while (search-forward quote nil t)
    (replace-match "")
    (insert "\\" quote)))

(defun se/unescape-ws (signifier char)
  (goto-char (point-min))
  (while (search-forward (concat "\\" signifier) nil t)
    (replace-match "")
    (insert char)))

(defun se/escape-ws (signifier char)
  (goto-char (point-min))
  (while (search-forward char nil t)
    (replace-match "")
    (insert "\\" signifier)))

(defvar string-edit-at-point-mode-map nil
  "Keymap for string-edit-at-point minor mode.")

(unless string-edit-at-point-mode-map
  (setq string-edit-at-point-mode-map (make-sparse-keymap)))

(--each '(("C-c C-k" . string-edit-at-point-abort)
          ("C-c C-c" . string-edit-at-point-conclude))
  (define-key string-edit-at-point-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-minor-mode string-edit-at-point-mode
  "Minor mode for useful keybindings while editing string."
  :init-value nil
  :lighter " StringEdit"
  :keymap string-edit-at-point-mode-map
  (if string-edit-at-point-mode
      (add-hook 'post-command-hook 'se/post-command nil t)
    (remove-hook 'post-command-hook 'se/post-command t)))

(defun se/post-command ()
  (se/adjust-window-size-to-fit-text))

(defun se/adjust-window-size-to-fit-text ()
  (when (and (> (+ 2 (line-number-at-pos (point-max)))
                (window-height))
             (> (/ (frame-height) (window-height))
                1))
    (enlarge-window 1)))

(defun se/aget (key map)
  (cdr (assoc key map)))

(defun se/current-quotes-char ()
  "The char that is the current quote delimiter, or nil if not in a string."
  (let ((delimiter (nth 3 (syntax-ppss))))
    (cond ((stringp delimiter) delimiter)
          ;; `syntax-ppss' can return t meaning 'a generic string delimiter'.
          (delimiter ?\"))))

(defalias 'se/point-inside-string-p 'se/current-quotes-char)

(defun se/move-point-backward-out-of-string ()
  "Move point backward until it exits the current quoted string."
  (while (se/point-inside-string-p) (backward-char)))

(defun se/move-point-forward-out-of-string ()
  "Move point forward until it exits the current quoted string."
  (while (se/point-inside-string-p) (forward-char)))

(defun se/string-position-at-point ()
  (let (beg)
    (save-excursion
      (se/move-point-backward-out-of-string)
      (setq beg (point))
      (forward-sexp 1)
      (cons beg (point)))))

;;; String conversion types

;; Regular old strings

(defun se/string-at-point ()
  (let* ((pos (se/string-position-at-point))
         (p (point))
         (beg (car pos))
         (end (cdr pos))
         (raw (buffer-substring-no-properties beg end))
         (quote (char-to-string (se/current-quotes-char))))
    `((:beg . ,beg)
      (:end . ,end)
      (:raw . ,raw)
      (:cleanup . ,(-partial 'se/string-at-point/clean-up quote))
      (:escape . ,(-partial 'se/string-at-point/escape quote)))))

(defun se/string-at-point/clean-up (quote)
  (save-excursion
    (goto-char (point-max))
    (delete-char (- (length quote)))
    (goto-char (point-min))
    (delete-char (length quote))
    (se/unescape quote)
    (se/unescape-ws "n" "\n")
    (se/unescape-ws "r" "\r")
    (se/unescape-ws "t" "\t")
    (se/unescape "\\")))

(defun se/string-at-point/escape (quote)
  (save-excursion
    (se/escape "\\")
    (se/escape-ws "n" "\n")
    (se/escape-ws "r" "\r")
    (se/escape-ws "t" "\t")
    (se/escape quote)))

;; JavaScript strings, can be concatenated

(defun se/js-strings-at-point ()
  (let ((quote (char-to-string (se/current-quotes-char)))
        beg end)
    (save-excursion
      (se/move-point-backward-out-of-string)
      (while (looking-back (concat (regexp-quote quote) "[\n\r\t ]*\\+[\n ]*"))
        (goto-char (match-beginning 0))
        (se/move-point-backward-out-of-string))
      (setq beg (point)))
    (save-excursion
      (se/move-point-forward-out-of-string)
      (while (looking-at (concat "[\n ]*\\+[\n ]*" (regexp-quote quote)))
        (goto-char (match-end 0))
        (se/move-point-forward-out-of-string))
      (setq end (point)))
    `((:beg . ,beg)
      (:end . ,end)
      (:raw . ,(buffer-substring-no-properties beg end))
      (:cleanup . ,(-partial 'se/js-strings-at-point/clean-up quote))
      (:escape . ,(-partial 'se/js-strings-at-point/escape quote)))))

(defun se/js-strings-at-point/clean-up (quote)
  (save-excursion
    (goto-char (point-max))
    (delete-char (- (length quote)))
    (goto-char (point-min))
    (delete-char (length quote))
    (while (search-forward (concat quote " + " quote) nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward (concat (regexp-quote quote) " \\+ *\n *" (regexp-quote quote)) nil t)
      (replace-match "\n"))
    (se/unescape quote)
    (se/unescape "\\")))

(defun se/js-strings-at-point/escape (quote)
  (save-excursion
    (se/escape "\\")
    (se/escape quote)
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (unless (bobp)
        (insert "\""))
      (end-of-line)
      (unless (eobp)
        (insert "\" +")))))

(provide 'string-edit-at-point)
;;; string-edit-at-point.el ends here
