;;; funcs.el --- Scala Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//scala-setup-ensime ()
  "Setup ENSIME for Scala."
  (spacemacs//java-setup-ensime)
  (add-to-list 'spacemacs-jump-handlers-scala-mode 'ensime-edit-definition))

(defun spacemacs//scala-setup-metals ()
  "Setup LSP metals for Scala."
  (setq-local lsp-prefer-flymake nil))

(defun spacemacs//scala-disable-flycheck-scala ()
  (push 'scala flycheck-disabled-checkers))

(defun spacemacs//scala-backend-ensime-p ()
  "Return true if the selected backend is ensime"
  (eq scala-backend 'scala-ensime))

(defun spacemacs//scala-backend-metals-p ()
  "Return true if the selected backend is metals"
  (eq scala-backend 'scala-metals))

(defun spacemacs/scala-join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))
