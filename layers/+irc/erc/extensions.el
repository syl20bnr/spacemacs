(setq erc-post-extensions
  '(
    erc-tex
    (erc-yank :excluded t)
    erc-sasl
    ))

(defun erc/init-erc-sasl ()
  (use-package erc-sasl
    :if erc-enable-sasl-auth
    ;; Following http://www.emacswiki.org/emacs/ErcSASL
    ;; Maybe an advice would be better?
    :config
    (progn
      ;; Add any server like this
      ;; (add-to-list 'erc-sasl-server-regexp-list "host\\.server\\.com")
      (add-to-list 'erc-sasl-server-regexp-list "irc\\.freenode\\.net")
      (defun erc-login ()
        "Perform user authentication at the IRC server."
        (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                         (erc-current-nick)
                         (user-login-name)
                         (or erc-system-name (system-name))
                         erc-session-server
                         erc-session-user-full-name))
        (if erc-session-password
            (erc-server-send (format "PASS %s" erc-session-password))
          (message "Logging in without password"))
        (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
          (erc-server-send "CAP REQ :sasl"))
        (erc-server-send (format "NICK %s" (erc-current-nick)))
        (erc-server-send
         (format "USER %s %s %s :%s"
                 ;; hacked - S.B.
                 (if erc-anonymous-login erc-email-userid (user-login-name))
                 "0" "*"
                 erc-session-user-full-name))
        (erc-update-mode-line)))))

(defun erc/init-erc-tex ()
  (require 'erc-tex))

(defun erc/init-erc-yank ()
  (use-package erc-yank
    :if (configuration-layer/package-usedp 'gist)
    :init
    (evil-define-key 'normal erc-mode-map
      "p" 'erc-yank)))
