(defun yarn//may-call-flycheck-buffer ()
  (if (bound-and-true-p flycheck-mode)
      (flycheck-buffer)))

;;;###autoload
(defun yarn/yarn-add ()
  (interactive)
  (let* ((pkg (read-string "yarn add: ")))
    (shell-command (concat "yarn add " (shell-quote-argument pkg))))
  (yarn//may-call-flycheck-buffer))

;;;###autoload
(defun yarn/yarn-add-dev ()
  (interactive)
  (let* ((pkg (read-string "yarn add --dev: ")))
    (shell-command (concat "yarn add " (shell-quote-argument pkg) " --dev")))
  (yarn//may-call-flycheck-buffer))
