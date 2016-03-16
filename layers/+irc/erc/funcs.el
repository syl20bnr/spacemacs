(defun erc//server (erc-func &rest args)
  (funcall erc-func :server (plist-get args :server)
           :nick (plist-get args :nick)
           :port (plist-get args :port)
           :password (plist-get args :password)))

(defun erc/server (&rest args)
  (apply #'erc//server
         (if (plist-get args :ssl)
             'erc-tls
           'erc)
         args))

(defun erc//servers (server-list)
  (dolist (a-server server-list)
    (eval a-server)))

(defun erc/default-servers ()
  (interactive)
  (if erc-server-list
      (erc//servers erc-server-list)
    (message "You must define erc-server-list")))
