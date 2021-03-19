;;; funcs.el --- Windows-scripts Layer Functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun windows-scripts/bat-outline-setup ()
  "Select position by mouse and return to `bat-mode'."
  (local-set-key [mouse-1] (lambda () (interactive) (bat-mode) (beginning-of-line)))
  (local-set-key [return] 'bat-mode))

;;;###autoload
(defun windows-scripts/bat-outline ()
  "Navigate within Batch script using outline-mode."
  (interactive)
  (setq-local outline-regexp ":[^:]")
  (outline-mode)
  (hide-body)
  (define-key evil-normal-state-local-map (kbd "SPC m z") 'bat-mode))
