(defun spacemacs//gptel-send-wrapper ()
  "Wrapper function for gptel-send that sets the flag."
  (interactive)
  (call-interactively 'gptel-send)
  (setq llm-client--gptel-send-called t))

(defun spacemacs//gptel-abort-wrapper ()
  "Wrapper function for gptel-abort that checks if gptel-send has been called."
  (interactive)
  (if llm-client--gptel-send-called
      (call-interactively 'gptel-abort)))
