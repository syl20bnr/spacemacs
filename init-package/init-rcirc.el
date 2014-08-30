(use-package rcirc
  :commands irc
  :init
  (progn
    (add-to-hook 'rcirc-mode-hook '(rcirc-track-minor-mode
                                    rcirc-omit-mode
                                    ;; rcirc-reconnect-mode
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
    (require 'rcirc-color)
    (require 'rcirc-reconnect
             (concat user-extensions-directory "rcirc-reconnect/rcirc-reconnect.el"))
    ;; identify info are stored in a separate location, skip errors
    ;; if the feature cannot be found.
    (require 'pinit-rcirc nil 'noerror)))
