;;; funcs.el --- Scala Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(autoload 'ensime-config-find-file "ensime-config")
(autoload 'ensime-config-find "ensime-config")
(autoload 'projectile-project-p "projectile")

(defun space-macs//ensime-maybe-start ()
  (when (buffer-file-name)
    (let ((ensime-buffer (space-macs//ensime-buffer-for-file (buffer-file-name)))
          (file (ensime-config-find-file (buffer-file-name)))
          (is-source-file (s-matches? (rx (or "/src/" "/test/"))
                                      (buffer-file-name))))

      (when (and is-source-file (null ensime-buffer))
        (noflet ((ensime-config-find (&rest _) file))
                (save-window-excursion
                  (ensime)))))))

(defun space-macs//ensime-buffer-for-file (file)
  "Find the Ensime server buffer corresponding to FILE."
  (let ((default-directory (file-name-directory file)))
    (-when-let (project-name (projectile-project-p))
      (--first (-when-let (bufname (buffer-name it))
                 (and (s-contains? "inferior-ensime-server" bufname)
                      (s-contains? (file-name-nondirectory project-name)
                                   bufname)))
               (buffer-list)))))

(defun space-macs//ensime-flyspell-verify ()
  "Prevent common flyspell false positives in scala-mode."
  (and (flyspell-generic-progmode-verify)
       (not (s-matches? (rx bol (* space) "package") (current-line)))))

(defun space-macs//scala-setup-ensime ()
  "Setup ENSIME."
  (add-to-list 'space-macs-jump-handlers-scala-mode 'ensime-edit-definition)
  ;; ensure the file exists before starting `ensime-mode'
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode))
   ((buffer-file-name)
    (add-hook 'after-save-hook 'ensime-mode nil t))))

(defun space-macs//scala-setup-metals ()
  "Setup LSP metals for Scala."
  (add-hook 'scala-mode-hook #'lsp))

(defun space-macs//scala-setup-dap ()
  "Setup DAP in metals for Scala."
  (when (space-macs//scala-backend-metals-p)
    (add-hook 'scala-mode-hook #'dap-mode)))

(defun space-macs//scala-setup-treeview ()
  "Setup lsp-tree-macs for Scala."
  (setq lsp-metals-treeview-show-when-views-received scala-auto-treeview))

(defun space-macs//scala-disable-flycheck-scala ()
  (push 'scala flycheck-disabled-checkers))

(defun space-macs//scala-backend-ensime-p ()
  "Return true if the selected backend is ensime"
  (eq scala-backend 'scala-ensime))

(defun space-macs//scala-backend-metals-p ()
  "Return true if the selected backend is metals"
  (eq scala-backend 'scala-metals))

(defun space-macs/scala-join-line ()
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

(defun space-macs//scala-setup-ensime-flyspell ()
  "Setup ENSIME spell checking."
  (flyspell-mode)
  (setq-local flyspell-generic-check-word-predicate
              'space-macs//ensime-flyspell-verify))

(defun space-macs//scala-setup-ensime-eldoc ()
  "Setup ENSIME eldoc."
  (setq-local eldoc-documentation-function
              (lambda ()
                (when (ensime-connected-p)
                  (ensime-type-at-point))))
  (eldoc-mode))

;; interactive functions

(defun space-macs/ensime-gen-and-restart()
  "Regenerate `.ensime' file and restart the ensime server."
  (interactive)
  (progn
    (sbt-command ";ensimeConfig;ensimeConfigProject")
    (ensime-shutdown)
    (ensime)))

(defun space-macs/ensime-inf-eval-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (ensime-inf-eval-buffer)
  (ensime-inf-switch)
  (evil-insert-state))

(defun space-macs/ensime-inf-eval-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (ensime-inf-switch)
  (ensime-inf-eval-region start end)
  (evil-insert-state))

(defun space-macs/ensime-refactor-accept ()
  (interactive)
  (funcall continue-refactor)
  (ensime-popup-buffer-quit-function))

(defun space-macs/ensime-refactor-cancel ()
  (interactive)
  (funcall cancel-refactor)
  (ensime-popup-buffer-quit-function))

(defun space-macs/ensime-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (company-abort)
  (insert ".")
  (company-complete))

(defun space-macs/ensime-yank-type-at-point ()
  "Yank to kill ring and print short type name at point to the minibuffer."
  (interactive)
  (ensime-type-at-point '(4)))

(defun space-macs/ensime-yank-type-at-point-full-name ()
  "Yank to kill ring and print full type name at point to the minibuffer."
  (interactive)
  (ensime-type-at-point '(4) t))


