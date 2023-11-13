;;; evil-collection-unimpaired.el --- Evil Collection port of Unimpaired -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, unimpaired, tools

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
;; `evil' port of unimpaired for `evil-collection'.
;; https://github.com/tpope/vim-unimpaired

;;; Code:
(require 'evil-collection)

;; Externals
(defvar flycheck-current-errors)

(defgroup evil-collection-unimpaired nil
  "Evil port of unimpaired for `evil-collection'."
  :group 'evil-collection)

(defcustom evil-collection-unimpaired-want-repeat-mode-integration nil
  "Whether or not to enable `repeat-mode' integration."
  :type 'boolean
  :group 'evil-collection)

(defconst evil-collection-unimpaired-maps '(evil-collection-unimpaired-mode-map))

(defvar evil-collection-unimpaired-mode-map (make-sparse-keymap)
  "Keymap for `evil-collection-unimpaired-mode'.")

(define-minor-mode evil-collection-unimpaired-mode
  "Evil port of unimpaired."
  :lighter " unimpaired"
  :group 'evil-collection-unimpaired)

;;;###autoload
(define-global-minor-mode global-evil-collection-unimpaired-mode
  evil-collection-unimpaired-mode evil-collection-unimpaired-mode-on
  :group 'evil-collection-unimpaired)

(defun evil-collection-unimpaired-mode-on ()
  "Turn on `evil-collection-unimpaired-mode'."
  (evil-collection-unimpaired-mode 1))

(evil-define-motion evil-collection-unimpaired-next-error (count)
  "Go to next error."
  :jump t
  (setq count (or count 1))
  (cond
   ((and (bound-and-true-p flycheck-mode)
         (fboundp 'flycheck-next-error))
    (flycheck-next-error count))
   ((and (bound-and-true-p flymake-mode)
         (fboundp 'flymake-goto-next-error))
    (flymake-goto-next-error count))
   (:default
    (message "No linting modes are on."))))

(evil-define-motion evil-collection-unimpaired-previous-error (count)
  "Go to previous error."
  :jump t
  (evil-collection-unimpaired-next-error (- (or count 1))))

(defun evil-collection-unimpaired--flycheck-count-errors ()
  "Count the number of flycheck errors."
  (length (delete-dups (mapcar 'flycheck-error-line flycheck-current-errors))))

(evil-define-motion evil-collection-unimpaired-first-error ()
  "Go to the first error."
  :jump t
  (cond
   ((and (bound-and-true-p flycheck-mode)
         (fboundp 'flycheck-first-error))
    (flycheck-first-error))
   ((bound-and-true-p flymake-mode)
    (message "flymake unsupported."))
   (:default
    (message "No linting modes are on."))))

(evil-define-motion evil-collection-unimpaired-last-error ()
  "Go to the last error."
  :jump t
  (cond
   ((and (bound-and-true-p flycheck-mode)
         (fboundp 'flycheck-first-error))
    (flycheck-first-error (evil-collection-unimpaired--flycheck-count-errors)))
   ((bound-and-true-p flymake-mode)
    (message "flymake unsupported."))
   (:default
    (message "No linting modes are on."))))

(defconst evil-collection-unimpaired--SCM-conflict-marker "^\\(@@@ .* @@@\\|[<=>]\\{7\\}\\)"
  "A regexp to match SCM conflict marker.")

(evil-define-motion evil-collection-unimpaired-previous-SCM-conflict-marker (count)
  "Go to the previous SCM conflict marker or diff/patch hunk."
  :jump t
  (evil-collection-unimpaired-next-SCM-conflict-marker (- (or count 1))))

(evil-define-motion evil-collection-unimpaired-next-SCM-conflict-marker (count)
  "Go to the next SCM conflict marker or diff/patch hunk."
  :jump t
  (evil-motion-loop (dir (or count 1))
    (cond
     ((> dir 0)
      (forward-line 1)
      (when (not (search-forward-regexp evil-collection-unimpaired--SCM-conflict-marker nil t))
        (forward-line -1))
      (move-beginning-of-line nil))
     (t
      (search-backward-regexp evil-collection-unimpaired--SCM-conflict-marker nil t)
      (move-beginning-of-line nil)))))

(defun evil-collection-unimpaired-paste-above ()
  "Paste above current line with preserving indentation."
  (interactive)
  (let ((indent (current-indentation))
        (column (current-column)))
    (evil-insert-newline-above)
    (indent-to indent)
    (evil-paste-after 1)
    (move-to-column column)))

(defun evil-collection-unimpaired-paste-below ()
  "Paste below current line with preserving indentation."
  (interactive)
  (let ((indent (current-indentation))
        (column (current-column)))
    (evil-insert-newline-below)
    (indent-to indent)
    (evil-paste-after 1)
    (move-to-column column)))

(defun evil-collection-unimpaired-insert-newline-above (count)
  "Insert COUNT blank line(s) above current line."
  (interactive "p")
  (save-excursion (dotimes (_ count) (evil-insert-newline-above)))
  (when (bolp) (forward-char count)))

(defun evil-collection-unimpaired-insert-newline-below (count)
  "Insert COUNT blank line(s) below current line."
  (interactive "p")
  (save-excursion (dotimes (_ count) (evil-insert-newline-below))))

(defun evil-collection-unimpaired--encode (beg end fn)
  "Apply FN from BEG to END."
  (save-excursion
    (goto-char beg)
    (let* ((end (if (eq evil-this-type 'line) (1- end) end))
           (text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (funcall fn text)))))

(evil-define-operator evil-collection-unimpaired-url-encode (count &optional beg end)
  "Encode a URL string."
  (interactive "<c><r>")
  (ignore count)
  (evil-collection-unimpaired--encode beg end #'url-encode-url))

(evil-define-operator evil-collection-unimpaired-url-decode (count &optional beg end)
  "Decode a URL string."
  (interactive "<c><r>")
  (ignore count)
  (evil-collection-unimpaired--encode beg end #'url-unhex-string))

(evil-define-operator evil-collection-unimpaired-b64-encode (count &optional beg end)
  "Encode a base64 string."
  (interactive "<c><r>")
  (ignore count)
  (evil-collection-unimpaired--encode beg end #'base64-encode-string))

(evil-define-operator evil-collection-unimpaired-b64-decode (count &optional beg end)
  "Decode a base64 string."
  (interactive "<c><r>")
  (ignore count)
  (evil-collection-unimpaired--encode beg end #'base64-decode-string))

;; https://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun evil-collection-unimpaired--move-text (arg)
  "Move text down if ARG is positive, otherwise move text up."
  (cond
   ((and mark-active transient-mark-mode)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column :force)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column)))))

(defun evil-collection-unimpaired-move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (evil-collection-unimpaired--move-text arg))

(defun evil-collection-unimpaired-move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (evil-collection-unimpaired--move-text (- arg)))

;;; 'repeat-mode' integration. Emacs 28+
(defmacro evil-collection-unimpaired-defvar-keymap (name &rest bindings)
  "Define NAME a keymap with BINDINGS."
  (declare (indent 1))
  (cl-assert (cl-evenp (length bindings)))
  `(progn
     (defvar ,name
       (let ((map (make-sparse-keymap)))
         map))
     (evil-collection-define-key nil ',name ,@bindings)))

;; "[b" and "]b"
(evil-collection-unimpaired-defvar-keymap evil-prev-buffer-repeat-map
  "b" #'evil-prev-buffer
  "B" #'evil-next-buffer)
(evil-collection-unimpaired-defvar-keymap evil-next-buffer-repeat-map
  "b" #'evil-next-buffer
  "B" #'evil-prev-buffer)

;; "[e" and "]e"
(evil-collection-unimpaired-defvar-keymap evil-collection-unimpaired-move-text-up-repeat-map
  "e" #'evil-collection-unimpaired-move-text-up
  "E" #'evil-collection-unimpaired-move-text-down)
(evil-collection-unimpaired-defvar-keymap evil-collection-unimpaired-move-text-down-repeat-map
  "e" #'evil-collection-unimpaired-move-text-down
  "E" #'evil-collection-unimpaired-move-text-up)

;; "[q" and "]q"
(evil-collection-unimpaired-defvar-keymap evil-collection-unimpaired-previous-error-repeat-map
  "q" #'evil-collection-unimpaired-previous-error
  "Q" #'evil-collection-unimpaired-next-error)
(evil-collection-unimpaired-defvar-keymap evil-collection-unimpaired-next-error-repeat-map
  "q" #'evil-collection-unimpaired-next-error
  "Q" #'evil-collection-unimpaired-previous-error)

;;;###autoload
(defun evil-collection-unimpaired-setup ()
  "Set up unimpaired-like bindings."
  (global-evil-collection-unimpaired-mode 1)
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
    "[b" 'evil-prev-buffer
    "]b" 'evil-next-buffer
    "[e" 'evil-collection-unimpaired-move-text-up
    "]e" 'evil-collection-unimpaired-move-text-down
    "[l" 'evil-collection-unimpaired-previous-error
    "]l" 'evil-collection-unimpaired-next-error
    "[L" 'evil-collection-unimpaired-first-error
    "]L" 'evil-collection-unimpaired-last-error
    "[q" 'evil-collection-unimpaired-previous-error
    "]q" 'evil-collection-unimpaired-next-error
    "[Q" 'evil-collection-unimpaired-first-error
    "]Q" 'evil-collection-unimpaired-last-error
    "[n" 'evil-collection-unimpaired-previous-SCM-conflict-marker
    "]n" 'evil-collection-unimpaired-next-SCM-conflict-marker
    "[p" 'evil-collection-unimpaired-paste-above
    "]p" 'evil-collection-unimpaired-paste-below
    "[P" 'evil-collection-unimpaired-paste-above
    "]P" 'evil-collection-unimpaired-paste-below
    (kbd "[ SPC") 'evil-collection-unimpaired-insert-newline-above
    (kbd "] SPC") 'evil-collection-unimpaired-insert-newline-below)
  (evil-collection-define-key 'visual 'evil-collection-unimpaired-mode-map
    "[e" 'evil-collection-unimpaired-move-text-up
    "]e" 'evil-collection-unimpaired-move-text-down
    "[n" 'evil-collection-unimpaired-previous-SCM-conflict-marker
    "]n" 'evil-collection-unimpaired-next-SCM-conflict-marker)
  (evil-collection-define-key 'motion 'evil-collection-unimpaired-mode-map
    "[u" 'evil-collection-unimpaired-url-encode
    "]u" 'evil-collection-unimpaired-url-decode
    "[6" 'evil-collection-unimpaired-b64-encode
    "]6" 'evil-collection-unimpaired-b64-decode)

  (when evil-collection-unimpaired-want-repeat-mode-integration
    (dolist (cmd '(evil-prev-buffer
                   evil-next-buffer
                   evil-collection-unimpaired-move-text-up
                   evil-collection-unimpaired-move-text-down
                   evil-collection-unimpaired-previous-error
                   evil-collection-unimpaired-next-error))
      (put cmd 'repeat-map (intern (format "%s-repeat-map" cmd))))))

(provide 'evil-collection-unimpaired)
;;; evil-collection-unimpaired.el ends here
