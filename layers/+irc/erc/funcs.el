(defun erc//servers (server-list)
  (dolist (s server-list)
    (apply (if
               (plist-get (cdr s) :ssl)
               (progn
                 (remf (cdr s) :ssl)
                 'erc-tls)
             'erc)
           :server s)))

(defun erc/default-servers ()
  (interactive)
  (if erc-server-list
      (erc//servers erc-server-list)
    (message "You must define erc-server-list")))
