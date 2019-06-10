;;; funcs.el --- rcirc Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun spacemacs/rcirc (arg)
  "Launch rcirc."
  (interactive "P")
  (require 'rcirc)
  ;; dispatch to rcirc launcher with appropriate support
  (cond
   (rcirc-enable-authinfo-support (spacemacs//rcirc-with-authinfo arg))
   (rcirc-enable-znc-support (spacemacs//rcirc-with-znc arg))
   (t (rcirc arg))))

(defun spacemacs//rcirc-with-authinfo (arg)
  "Fire rcirc with support for authinfo."
  (unless arg
    (if (file-exists-p "~/.authinfo.gpg")
        (spacemacs//rcirc-authinfo-config)
      (message "Cannot find file ~/.authinfo.gpg")))
  (rcirc arg))

(defun spacemacs//rcirc-with-znc (arg)
  "Fire rcirc with support for znc."
  (if arg
      (rcirc arg)
    (setq rcirc-server-alist
          ;; This will replace :auth with the correct thing, see the
          ;; doc for that function
          (spacemacs//znc-rcirc-server-alist-get-authinfo
           rcirc-server-alist))
    (spacemacs//znc-rcirc-connect)))

(defun spacemacs/rcirc-notify-beep (msg)
  "Beep when notifying."
  (let ((player "mplayer")
        (sound (concat spacemacs-start-directory "site-misc/startup.ogg")))
    (when (and (executable-find player)
               (file-exists-p sound)))
    (start-process "beep-process" nil player sound)))



;; persp

(defun spacemacs//rcirc-persp-filter-save-buffers-function (buffer)
  "Filter for rcirc layout."
  (with-current-buffer buffer
    (eq major-mode 'rcirc-mode)))

(defun spacemacs//rcirc-buffer-to-persp ()
  "Add buffer to rcirc layout."
  (persp-add-buffer (current-buffer) (persp-get-by-name
                                      rcirc-spacemacs-layout-name)))


;; logging

(defun spacemacs//rcirc-write-log (process sender response target text)
  (when rcirc-log-directory
    (with-temp-buffer
      ;; Sometimes TARGET is a buffer :-(
      (when (bufferp target)
        (setq target (with-current-buffer buffer rcirc-target)))
      ;; Sometimes buffer is not anything at all!
      (unless (or (null target) (string= target ""))
        ;; Print the line into the temp buffer.
        (insert (format-time-string "%Y-%m-%d %H:%M "))
        (insert (format "%-16s " (rcirc-user-nick sender)))
        (unless (string= response "PRIVMSG")
          (insert "/" (downcase response) " "))
        (insert text "\n")
        ;; Append the line to the appropriate logfile.
        (let ((coding-system-for-write 'no-conversion)
              (logfile (concat rcirc-log-directory  (downcase target))))
          (when (not (file-directory-p (file-name-directory logfile)))
            (make-directory (file-name-directory logfile)))
          (write-region (point-min) (point-max)
                        logfile
                        t 'quietly))))))



;; emms

(defun spacemacs/rcirc-insert-current-emms-track ()
  (interactive)
  (insert (emms-track-description (emms-playlist-current-selected-track))))



;; authinfo

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



;; ZNC with authinfo

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
  (cl-loop
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
            (cl-loop for p in (rcirc-process-list)
                  thereis (string= server (process-get p :rcirc-server)))))
       (unless connected
         (let ((process
                (rcirc-connect host port nick user-name
                               full-name channels password encryption)))
           (process-put process :rcirc-server server)))))))

;; erc-image -----------------------------------------------------------------

(defun spacemacs//rcirc-image-show-url (_sender _response)
  (unless (boundp 'erc-fill-prefix)
    (setq erc-fill-prefix rcirc-fill-prefix))
  (erc-image-show-url))

;; erc-yt --------------------------------------------------------------------

(defun spacemacs//rcirc-youtube-show-info (_sender _response)
  (erc-yt-show-info))

;; erc-tweet --------------------------------------------------------------------

(defun spacemacs//rcirc-tweet-show-tweet (_sender _response)
  (erc-tweet-show-tweet))
