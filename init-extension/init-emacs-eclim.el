;; Load both eclim and eclimd (so that we can control eclimd from within Emacs)
(require 'eclim)
(require 'eclimd)

;; Variables
(setq eclim-auto-save t
      eclimd-wait-for-process nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

;; Call the help framework with the settings above & activate eclim-mode
(help-at-pt-set-timer)
(add-hook 'java-mode-hook '(lambda () (eclim-mode t)))

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
