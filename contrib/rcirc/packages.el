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
    :defer t
    :init
    (progn
      (add-to-hook 'rcirc-mode-hook '(flyspell-mode rcirc-omit-mode))

      (defun spacemacs//rcirc-with-authinfo (arg)
        "Fire rcirc with support for authinfo."
        (unless arg
          (if (file-exists-p "~/.authinfo.gpg")
              (spacemacs//rcirc-authinfo-config)
            (message "Warning: Cannot find file ~/.authinfo.gpg")))
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

      (evil-leader/set-key "ai" 'spacemacs/rcirc)
      (defun spacemacs/rcirc (arg)
        "Launch rcirc."
        (interactive "P")
        (require 'rcirc)
        ;; dispatch to rcirc launcher with appropriate support
        (cond
         (rcirc-enable-authinfo-support (spacemacs//rcirc-with-authinfo arg))
         (rcirc-enable-znc-support (spacemacs//rcirc-with-znc arg))
         (t (rcirc arg)))))
    :config
    (progn
      ;; (set-input-method "latin-1-prefix")
      (set (make-local-variable 'scroll-conservatively) 8192)

      (setq rcirc-fill-column 80
            rcirc-buffer-maximum-lines 2048
            rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
            rcirc-time-format "%Y-%m-%d %H:%M "
            rcirc-omit-threshold 20)

      ;; Exclude rcirc properties when yanking, in order to be able to send mails
      ;; for example.
      (add-to-list 'yank-excluded-properties 'rcirc-text)

      ;; rcirc-reconnect
      (let ((dir (configuration-layer/get-layer-property 'rcirc :ext-dir)))
        (require 'rcirc-reconnect
                 (concat dir "rcirc-reconnect/rcirc-reconnect.el")))

      ;; load this file from the dropbox location load-path
      ;; this is where you can store personal information
      (require 'pinit-rcirc nil 'noerror)

      (define-key rcirc-mode-map (kbd "C-j") 'rcirc-insert-prev-input)
      (define-key rcirc-mode-map (kbd "C-k") 'rcirc-insert-next-input)

      ;; add a key for EMMS integration
      (when (boundp 'emms-track-description)
        (defun rcirc/insert-current-emms-track ()
          (interactive)
          (insert (emms-track-description (emms-playlist-current-selected-track))))
        (define-key rcirc-mode-map (kbd "C-c C-e") 'rcirc/insert-current-emms-track))

      ;; Minimal logging to `~/.emacs.d/.cache/rcirc-logs/'
      ;; by courtesy of Trent Buck.
      (setq rcirc-log-directory (concat spacemacs-cache-directory "/rcirc-logs/"))
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
      (add-hook 'rcirc-print-hooks 'rcirc-write-log)

      ;; dependencies
      ;; will autoload rcirc-notify
      (rcirc-notify-add-hooks)
      (require 'rcirc-color))))

(defun rcirc/init-rcirc-notify ()
  (use-package rcirc-notify
    :defer t
    :config
    (progn
      (defun spacemacs/rcirc-notify-beep (msg)
        "Beep when notifying."
        (let ((player "mplayer")
              (sound "~/.emacs.d/site-misc/startup.ogg"))
          (when (and (executable-find player)
                     (file-exists-p sound)))
          (start-process "beep-process" nil player sound)))
      (add-hook 'rcirc-notify-page-me-hooks 'spacemacs/rcirc-notify-beep))))

(defun rcirc/init-rcirc-color ()
  (use-package rcirc-color :defer t))
