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

      (evil-leader/set-key-for-mode 'restclient-mode
        "ms" 'restclient-http-send-current-stay-in-window
        "mS" 'restclient-http-send-current
        "mr" 'restclient-http-send-current-raw-stay-in-window
        "mR" 'restclient-http-send-current-raw
        ))
    )
  )
