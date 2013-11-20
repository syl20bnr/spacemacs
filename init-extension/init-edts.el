(use-package edts
  :init
  (require 'edts-start)
  :config
  ;; (setq edts-log-level 'debug)
  ;; inherits faces from flycheck
  (custom-set-faces
   '(edts-face-error-line ((t (:inherit flycheck-error-face))))
   '(edts-face-error-mode-line ((t (:inherit flycheck-error-face-mode-line))))
   '(edts-face-warning-line ((t (:inherit flycheck-warning-face))))
   '(edts-face-warning-mode-line ((t (:inherit flycheck-warning-face-mode-line))))
   ))
