;;; funcs.el --- rcirc Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; authinfo ------------------------------------------------------------------

(defun spacemacs//rcirc-authinfo-config ()
  "Initialize authinfo.
Allow rcirc to read authinfo from ~/.authinfo.gpg via the auth-source API.
This doesn't support the chanserv auth method. "
  (require 'auth-source)
  (dolist (p (auth-source-search :port '("nickserv" "bitlbee" "quakenet")
                                 :require '(:port :user :secret)
                                 :max (length rcirc-server-alist)))
    (let ((secret (plist-get p :secret))
          (method (intern (plist-get p :port))))
      (add-to-list
       'rcirc-authinfo
       (list (plist-get p :host) method (plist-get p :user)
             (if (functionp secret) (funcall secret) secret))))))


;; ZNC with authinfo ---------------------------------------------------------

(defun spacemacs//znc-auth-source-fetch-password (server)
  "Given a server with at least :host :port :login, return the :password"
  (destructuring-bind (&key host auth &allow-other-keys)
      (cdr server)
    (destructuring-bind (&key secret &allow-other-keys)
        (car (auth-source-search :host host
                                 :port "irc"
                                 :user auth
                                 :require '(:user :secret)))
      (if (functionp secret) (funcall secret) secret))))

;; (setq auth (auth-source--aput :host ""))
;; build rcirc-authinfo from rcirc-server-alist and authinfo
(defun spacemacs//znc-rcirc-server-alist-get-authinfo (server-alist)
  "replace :auth in rcirc-server-alist with :password \"user:password\"
 from .authinfo.gpg"
  (dolist (server server-alist server-alist)
    (let* ((host  (car server))
           (plist (cdr server))
           (auth  (plist-get plist :auth))
           (pass  (spacemacs//znc-auth-source-fetch-password server)))
      (when auth
        (plist-put plist :password (format "%s:%s" auth pass))))))

;; rcirc does not know how to connect to the same server more than once, so
;; we build our own connection routine from our own rcirc-server-alist,
;; using :host rather than the server name for connecting.
(defun spacemacs//znc-rcirc-connect ()
  "Connect to rcirc-server-alist servers."
  (loop
   for s in rcirc-server-alist
   collect
   (destructuring-bind (&key host
                             (port rcirc-default-port)
                             (nick rcirc-default-nick)
                             (user-name rcirc-default-user-name)
                             (full-name rcirc-default-full-name)
                             channels
                             password
                             encryption
                             &allow-other-keys
                             &aux contact (server (car s)))
       (cdr s)
     (let ((host (or host server)) ; catter with server without :host
           (connected
            (loop for p in (rcirc-process-list)
                  thereis (string= server (process-get p :rcirc-server)))))
       (unless connected
         (let ((process
                (rcirc-connect host port nick user-name
                               full-name channels password encryption)))
           (process-put process :rcirc-server server)))))))
