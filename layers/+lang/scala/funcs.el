;;; funcs.el --- Scala Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(autoload 'projectile-project-p "projectile")

(defun spacemacs//scala-setup-metals ()
  "Setup LSP metals for Scala."
  (add-hook 'scala-mode-hook #'lsp))

(defun spacemacs//scala-setup-dap ()
  "Setup DAP in metals for Scala."
  (when (spacemacs//scala-backend-metals-p)
    (add-hook 'scala-mode-hook #'dap-mode)))

(defun spacemacs//scala-display-sbt-at-bottom (buffer args)
  "Display a short buffer in a dedicated window at frame bottom.
For use with `sbt:display-buffer-action'."
  (set-window-dedicated-p
   (display-buffer-at-bottom buffer (cons '(window-height . 12) args))
   t))

(defun spacemacs//scala-setup-treeview ()
  "Setup lsp-treemacs for Scala."
  (setq lsp-metals-treeview-show-when-views-received scala-auto-treeview))

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
