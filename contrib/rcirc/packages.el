(defvar rcirc-packages
  '(
    rcirc
    rcirc-notify
    rcirc-color
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar rcirc-excluded-packages '()
  "List of packages to exclude.")

(defun rcirc/init-rcirc ()
  (use-package rcirc
    :init
    (progn
      (setq rcirc-fill-column 80
            rcirc-buffer-maximum-lines 2048
            rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
            rcirc-time-format "%Y-%m-%d %H:%M "
            rcirc-omit-threshold 20)

      ;; add a key for EMMS integration
      (when (boundp 'emms-track-description)
        (defun rcirc/insert-current-emms-track ()
          (interactive)
          (insert (emms-track-description (emms-playlist-current-selected-track))))

        (define-key rcirc-mode-map (kbd "C-c C-e") 'rcirc/insert-current-emms-track)
        )

      ;; Exclude rcirc properties when yanking, in order to be able to send mails
      ;; for example.
      (add-to-list 'yank-excluded-properties 'rcirc-text)
      ;;###autoload
      (add-hook 'rcirc-mode-hook '(lambda ()
                                    ;; Turn on spell checking.
                                    (flyspell-mode 1)
                                    (rcirc-omit-mode)
                                    ;; (set-input-method "latin-1-prefix")
                                    (set (make-local-variable 'scroll-conservatively) 8192)
                                    ))

      (let ((dir (configuration-layer/get-layer-property 'rcirc :ext-dir)))
        (require 'rcirc-reconnect
                 (concat dir "rcirc-reconnect/rcirc-reconnect.el")))
      (require 'pinit-rcirc nil 'noerror)

      (define-key rcirc-mode-map (kbd "C-j") 'rcirc-insert-prev-input)
      (define-key rcirc-mode-map (kbd "C-k") 'rcirc-insert-next-input)

      ;; Join these channels at startup.
      ;; (setq rcirc-server-alist
      ;;       '(("irc.freenode.net"
      ;;          :user "spacemacs_user"
      ;;          :port "1337"
      ;;          :password "le_passwd"
      ;;          :channels ("#emacs"))))

      (if (and rcirc-authinfo-support
               (file-exists-p "~/.authinfo.gpg"))
          (progn
            (defun barebones-rcirc-config ()
              (defadvice rcirc (before rcirc-read-from-authinfo activate)
                "Allow rcirc to read authinfo from ~/.authinfo.gpg via the auth-source API.
This doesn't support the chanserv auth method"
                (unless arg
                  (dolist (p (auth-source-search :port '("nickserv" "bitlbee" "quakenet")
                                                 :require '(:port :user :secret)))
                    (let ((secret (plist-get
                                   p :secret))
                          (method (intern (plist-get p :port))))
                      (add-to-list 'rcirc-authinfo
                                   (list (plist-get p :host)
                                         method
                                         (plist-get p :user)
                                         (if (functionp secret)
                                             (funcall secret)
                                           secret)))))))
              (rcirc nil)
              )
            (defun znc-rcirc-config ()
              (defun dim:auth-source-fetch-password (server)
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
              (defun dim:rcirc-server-alist-get-authinfo (server-alist)
                "replace :auth in rcirc-server-alist with :password \"user:password\" from .authinfo.gpg"
                (dolist (server server-alist server-alist)
                  (let* ((host  (car server))
                         (plist (cdr server))
                         (auth  (plist-get plist :auth))
                         (pass  (dim:auth-source-fetch-password server)))
                    (when auth
                      (plist-put plist
                                 :password (format "%s:%s" auth pass)))
                    ))
                )

              ;; rcirc does not know how to connect to the same server more than once, so
              ;; we build our own connection routine from our own rcirc-server-alist,
              ;; using :host rather than the server name for connecting.
              (defun dim:rcirc ()
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
                   (let ((host (or host server))	; catter with server without :host
                         (connected
                          (loop for p in (rcirc-process-list)
                                thereis (string= server (process-get p :rcirc-server)))))
                     (unless connected
                       (let ((process
                              (rcirc-connect host port nick user-name
                                             full-name channels password encryption)))
                         (process-put process :rcirc-server server)))))))

              (setq rcirc-server-alist
                    ;; This will replace :auth with the correct thing, see the
                    ;; doc for that function
                    (dim:rcirc-server-alist-get-authinfo
                     rcirc-server-alist))

              ;; you should have sth like this in your dotfile
              ;; (setq rcirc-server-alist
              ;;       '(("freenode"
              ;;          :host "freenode.spacemacsserver.me"
              ;;          :port "1337"
              ;;          :auth "spacemacs_user/freenode"
              ;;          :channels ("#emacs"))
              ;;         ("geekshed"
              ;;          :host "geekshed.spacemacsserver.me"
              ;;          :port "1337"
              ;;          :auth "spacemacs_user/geekshed"
              ;;          :channels ("#jupiterbroadcasting")))
              ;;       )

              ;; This is what the line on the ~/.authinfo.gpg file should look like
              ;; machine freenode.spacemacsserver.me port irc user spacemacs_user/freenode password my_znc_passwd
              ;; machine geekshed.spacemacsserver.me port irc user spacemacs_user/geekshed password my_znc_passwd
              (dim:rcirc)
              )
            (defun rcirc-config()
              (interactive)
              (if rcirc-uses-znc
                  (znc-rcirc-config)
                (barebones-rcirc-config)))
            (evil-leader/set-key
              "ai" 'rcirc-config))
        (evil-leader/set-leader
         "ai" 'irc)
        )

      ;; Minimal logging to `~/.rcirc-logs/channel'
      ;; by courtesy of Trent Buck.
      (add-hook 'rcirc-print-hooks 'rcirc-write-log)
      (setq rcirc-log-directory "~/.emacs/.cache/rcirc-logs/")
      (setq rcirc-log-flag t)
      (defun rcirc-write-log (process sender response target text)
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
              (let ((coding-system-for-write 'no-conversion))
                (write-region (point-min) (point-max)
                              (concat rcirc-log-directory  (downcase target))
                              t 'quietly))))))
      )
    ))

(defun rcirc/init-rcirc-notify ()
  (use-package rcirc-notify
    :init
    (progn
      (rcirc-notify-add-hooks)
      (add-hook 'rcirc-notify-page-me-hooks
                (lambda (msg)
                  (start-process "beep-process" nil
                                 "mplayer" "~/.emacs.d/site-misc/startup.ogg" )))
      )
    )
  )

(defun rcirc/init-rcirc-color ()
  (use-package rcirc-color()
    :init)
  )
