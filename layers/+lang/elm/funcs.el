;;; funcs.el --- Elm Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; elm-mode

(defun spacemacs//elm-find-root ()
  (setq default-directory (elm--find-dependency-file-path)))

(defun spacemacs/elm-compile-buffer-output ()
  (interactive)
  (let* ((fname (format "%s.js" (downcase (file-name-base (buffer-file-name))))))
    (elm-compile--file (elm--buffer-local-file-name) fname)))

(defun spacemacs/elm-repl-push-decl-focus ()
  "Send current function to the REPL and focus it in insert state."
  (interactive)
  (elm-repl-push-decl)
  (run-elm-interactive)
  (evil-insert-state))

(defun spacemacs/elm-repl-push-focus ()
  "Send current region to the REPL and focus it in insert state."
  (elm-repl-push)
  (run-elm-interactive)
  (evil-insert-state))
