(setq restclient-packages
  '(
    restclient
    )
  )

(defun restclient/init-restclient ()
  (use-package restclient
    :mode ("\\.http\\'" . restclient-mode)
    :defer t
    :init
    (progn

      (defun restclient-http-send-current-raw-stay-in-window ()
        (interactive)
        (restclient-http-send-current t t))

      (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
        "s" 'restclient-http-send-current-stay-in-window
        "S" 'restclient-http-send-current
        "r" 'restclient-http-send-current-raw-stay-in-window
        "R" 'restclient-http-send-current-raw
        ))
    )
  )
