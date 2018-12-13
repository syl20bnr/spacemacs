;;; funcs.el --- Haskell Layer funcs File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-haskell//disable-electric-indent ()
  "Disable electric indent mode if available"
  ;; use only internal indentation system from haskell
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))

(defun spacemacs/haskell-format-imports ()
  "Sort and align import statements from anywhere in the source file."
  (interactive)
  (save-excursion
    (haskell-navigate-imports)
    (haskell-mode-format-imports)))

;; Dante Functions

(defun spacemacs-haskell//dante-insert-type ()
  (interactive)
  (dante-type-at :insert))


;; Intero functions

(defun haskell-intero/insert-type ()
  (interactive)
  (intero-type-at :insert))

(defun haskell-intero/display-repl (&optional prompt-options)
  (interactive "P")
  (let ((buffer (intero-repl-buffer prompt-options t)))
    (unless (get-buffer-window buffer 'visible)
      (display-buffer buffer))))

(defun haskell-intero/pop-to-repl (&optional prompt-options)
  (interactive "P")
  (pop-to-buffer (intero-repl-buffer prompt-options t)))

(defun haskell-intero//preserve-focus (f &rest args)
  (let ((buffer (current-buffer)))
    (apply f args)
    (pop-to-buffer buffer)))
