(defun ranger-control/cd-tab (path)
  (start-process "ranger-control-curl" nil "curl" "-X" "POST" "--data" path
                 "--connect-timeout" "0.1" "http://localhost:5964/cdtab-e"))

(defun ranger-control/kill-result-buffer (status)
      "Kill the buffer returned by `url-retrieve'."
      (kill-buffer (current-buffer)))

(defun ranger-control/projectile-cd ()
  (interactive)
  (use-package projectile :init
    (ranger-control/cd-tab (projectile-project-root))))
