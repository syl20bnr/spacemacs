(defvar restclient-packages
  '(
    restclient
    )
  )

(defun restclient/init-restclient ()
  (use-package restclient
    :mode ("\\.http\\'" . restclient-mode)
    :commands (restclient-http-send-current
               restclient-http-send-current-raw
               restclient-http-send-current-stay-in-window)
    :init
    (evil-leader/set-key-for-mode 'restclient-mode
      "mc" 'restclient-http-send-current
      "mr" 'restclient-http-send-current-raw
      "mv" 'restclient-http-send-current-stay-in-window
      )
    )
  )
