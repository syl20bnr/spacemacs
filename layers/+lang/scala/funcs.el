;;; funcs.el --- Scala Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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

(defun spacemacs/scala-sbt-scalafmt-all ()
  "Run `scalafmtAll' via SBT"
  (interactive)
  (sbt-command "scalafmtAll"))

(defun spacemacs/scala-sbt-compile ()
  "Run `compile' via SBT"
  (interactive)
  (sbt-command "compile"))

(defun spacemacs/scala-sbt-test ()
  "Run `test' via SBT"
  (interactive)
  (sbt-command "test"))
