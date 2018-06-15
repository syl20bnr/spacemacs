(defun spacemacs//loadenv ()
  "Gets and sets all the environment variables from a terminal instance."
  (interactive)
  (let ((envvars (split-string (shell-command-to-string "env") "\n")))
    (dolist (env envvars)
      (if (string-match "^[a-zA-Z_]+[a-zA-Z0-9_]*=" env)
          (let* ((var (split-string env "="))
                 (k (car var))
                 (v (cadr var)))
            (setenv k v))))))

(provide 'spacemacs-environment)
