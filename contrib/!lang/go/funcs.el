(defun spacemacs/go-create-new-file-in-gopath(pkg src)
  (interactive "spackage pathname: \nssource filename: \n")
  (let* ((dir (concatenate 'string
						   (car (split-string (getenv "GOPATH") ":"))
						   "/src/"
						   pkg))
		 (filename (concatenate 'string dir "/" src)))
	(unless (file-exists-p dir)
	  (shell-command (concatenate 'string "mkdir -p " dir)))
	(when (file-exists-p dir)
	  (find-file filename))))

(defun spacemacs/go-run-current-buffer-other-window ()
  (interactive)
  (setq spacemacs-go-run-current-buffer-other-window (not spacemacs-go-run-current-buffer-other-window)))

(defun spacemacs/go-run-current-buffer ()
  "go run file"
  (interactive)
  (let* ((buff (current-buffer))
         (go-source-filename (buffer-file-name buff)))
	(when go-source-filename
	  (save-excursion
		(save-buffer)
		(let* ((cmd (concatenate 'string
								 "go run "
								 go-source-filename))
			   (process (get-buffer-process (get-buffer cmd))))
		  (when process
			(delete-process process))
		  (when (get-buffer cmd)
			(kill-buffer cmd))
		  (async-shell-command cmd cmd)
          (message cmd)))
      (if spacemacs-go-run-current-buffer-other-window
          (switch-to-buffer-other-window cmd)
        (switch-to-buffer buff)))))

(defun spacemacs/go-run-godoc-server ()
  (interactive)
  (let* ((port spacemacs-go-godoc-port)
         (cmd (format "godoc -http=:%d" port)))
    (save-window-excursion
      (unless (get-buffer cmd)
        (async-shell-command cmd cmd)))))

(defun spacemacs/go-open-godoc-in-browser ()
  (interactive)
  (browse-url (format "http://localhost:%d/pkg" spacemacs-go-godoc-port)))
