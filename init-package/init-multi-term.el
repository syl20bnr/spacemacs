(require 'multi-term)
;; zsh
(setq multi-term-program "/usr/bin/zsh")
;; for solarized dark theme
(custom-set-variables
   '(term-default-bg-color "#002b36")
   '(term-default-fg-color "#93a1a1"))
;; enable evil
(evil-set-initial-state 'term-mode 'emacs)
;; don't switch to other multi-term when closing
;; the current one
(setq multi-term-switch-after-close nil)
;; let TAB key for the terminal
