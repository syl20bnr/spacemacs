;;; funcs.el --- e-macs Lisp functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3



;; Idea from http://www.reddit.com/r/e-macs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun space-macs/eval-current-form ()
  "Find and evaluate the current def* or set* command.
Unlike `eval-defun', this does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun space-macs/nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))


;; edebug

(defun space-macs/edebug-instrument-defun-on ()
  "Toggle on instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun 'edebugit))

(defun space-macs/edebug-instrument-defun-off ()
  "Toggle off instrumentalisation for the function under `defun'."
  (interactive)
  (eval-defun nil))

(defun space-macs/elisp-toggle-debug-expr-and-eval-func ()
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

(defun space-macs//edebug-mode (&rest args)
  "Additional processing when `edebug-mode' is activated or deactivated."
  (let ((evilified (or (eq 'vim dotspace-macs-editing-style)
                       (and (eq 'hybrid dotspace-macs-editing-style)
                            hybrid-style-enable-evilified-state))))
    (if (not edebug-mode)
        ;; disable edebug-mode
        (when evilified (evil-normal-state))
      ;; enable edebug-mode
      (when evilified (evil-evilified-state))
      (evil-normalize-keymaps)
      (when (and (fboundp 'golden-ratio-mode)
                 golden-ratio-mode)
        (golden-ratio)))))


;; smartparens integration

(defun space-macs/eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp))))

(defun space-macs/eval-current-symbol-sp ()
  "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
  (interactive)
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))


;; elisp comment text-object definition

(defun space-macs//define-elisp-comment-text-object ()
  "Define a text object and a surround pair for elisp comments.
Intended for use in mode hooks."
  (space-macs|define-text-object ";" "elisp-comment" ";; " ""))


