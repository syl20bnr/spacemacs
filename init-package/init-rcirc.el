(use-package rcirc
  :init
  (progn
    (add-to-hook 'rcirc-mode-hook '(rcirc-track-minor-mode
                                    rcirc-omit-mode
                                    flyspell-mode)))
  :config
  (progn
    (setq rcirc-fill-column 160)
    (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
    (setq rcirc-omit-threshold 20)
    (setq rcirc-server-alist
          '(("chat.freenode.net" :port 6697 :encryption tls
             :nick "syl20bnr"
             :full-name "Sylvain Benner"
             :channels ("#emacs" "#nupic" "#python"))))
    (require 'rcirc-color nil 'noerror)
    ;; identify info are stored in a separate location, skip errors
    ;; if the feature cannot be found.
    (require 'pinit-rcirc)))

;; from the rcirc manual
(eval-after-load "rcirc"
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))
