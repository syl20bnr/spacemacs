;; Variables
(setq eclim-auto-save t
      eclimd-wait-for-process nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

;; Load both eclim and eclimd (so that we can control eclimd from within Emacs)
(require 'eclim)
(require 'eclimd)

;; Call the help framework with the settings above & activate eclim-mode
(help-at-pt-set-timer)
(global-eclim-mode)

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook (lambda ()
                             (add-to-list 'ac-sources 'ac-source-emacs-eclim)
                             (add-to-list 'ac-sources 'ac-source-emacs-eclim-c-dot)))
