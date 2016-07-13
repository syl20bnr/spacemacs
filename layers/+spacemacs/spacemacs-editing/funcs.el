;;; funcs.el --- Spacemacs editing Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; smartparens

(defun spacemacs/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun spacemacs/smartparens-pair-newline-and-indent (id action context)
  (spacemacs/smartparens-pair-newline id action context)
  (indent-according-to-mode))

(defun spacemacs/smart-closing-parenthesis ()
  (interactive)
  (let* ((sp-navigate-close-if-unbalanced t)
         (current-pos (point))
         (current-line (line-number-at-pos current-pos))
         (next-pos (save-excursion
                     (sp-up-sexp)
                     (point)))
         (next-line (line-number-at-pos next-pos)))
    (cond
     ((and (= current-line next-line)
           (not (= current-pos next-pos)))
      (sp-up-sexp))
     (t
      (insert-char ?\))))))

(defun spacemacs//conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode)))

(defun spacemacs//adaptive-smartparent-pair-overlay-face ()
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit 'lazy-highlight
                      :background nil
                      :foreground nil))


;; uuidgen
;; TODO spacemacs/uuidgen-3 and spacemacs/uuidgen-5

(defun spacemacs/uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun spacemacs/uuidgen-4 (arg)
  "Return an UUID from random numbers (UUIDv4).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-4)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))
