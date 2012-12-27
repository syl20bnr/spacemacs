;; font size
(set-face-attribute 'default nil :height 110)
;; whitespace-mode
(setq-default show-trailing-whitespace nil)
;; When point is on paranthesis, highlight the matching one
(show-paren-mode t)
;; use only spaces and no tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
;; highlight current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#073642")
