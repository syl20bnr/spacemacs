;;; funcs.el --- Emacs Lisp functions File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.




;; Idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun spacemacs/eval-current-form ()
  "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun spacemacs/nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))


;; edebug

(defun spacemacs/edebug-instrument-defun-on ()
  "Toggle on instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun 'edebugit))

(defun spacemacs/edebug-instrument-defun-off ()
  "Toggle off instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun nil))

(defun spacemacs/elisp-toggle-debug-expr-and-eval-func ()
  "Insert or remove debug expression, evaluate function and save buffer."
  (interactive)
  (let ((trace "(debug)")
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (newline-and-indent))))
  (eval-defun nil)
  (save-buffer))

(defun spacemacs//edebug-mode (&rest args)
  "Additional processing when `edebug-mode' is activated or deactivated."
  (let ((evilified? (spacemacs//support-evilified-buffer-p)))
    (if (not edebug-mode)
        ;; disable edebug-mode
        (when evilified? (evil-evilified-state-exit))
      ;; enable edebug-mode
      (when evilified? (evil-evilified-state))
      (when (and (fboundp 'golden-ratio-mode)
                 golden-ratio-mode)
        (golden-ratio)))))


;; smartparens integration

(defun spacemacs/eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (cl-decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp))))

(defun spacemacs/eval-current-symbol-sp ()
  "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
  (interactive)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))

(defun spacemacs/eval-current-form-to-comment-sp (&optional arg)
  "Same as `spacemacs/eval-current-form-sp' but inserts output as a comment."
  (interactive "p")
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (cl-decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (let ((ret-val (format ";; %S" (call-interactively 'eval-last-sexp))))
        (goto-char (point-at-eol))
        (open-line 1)
        (forward-line 1)
        (insert ret-val)))))


;; elisp comment text-object definition

(defun spacemacs//define-elisp-comment-text-object ()
  "Define a text object and a surround pair for elisp comments.
Intended for use in mode hooks."
  (spacemacs|define-text-object ";" "elisp-comment" ";; " ""))


;; Elisp autoformat buffer functions

(defun spacemacs//make-elisp-buffers-format-on-save-maybe ()
  "Add a function to format buffers on save when required."
  (when emacs-lisp-format-on-save
    (add-hook 'emacs-lisp-mode-hook #'spacemacs//make-elisp-buffer-format-on-save)))

(defun spacemacs//make-elisp-buffer-format-on-save ()
  "Make sure that this buffer is formatted on save"
  (add-hook 'before-save-hook #'spacemacs//format-elisp-buffer nil t))

(defun spacemacs//format-elisp-buffer ()
  "Format the given buffer if required."
  (when emacs-lisp-format-on-save
    (indent-region (point-min) (point-max))
    (whitespace-cleanup)))
