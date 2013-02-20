;; edit area full screen
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 0)
;; Show column number in mode line
(setq column-number-mode t)
;; line number
(setq linum-format "%4d")
;; highlight current line
(global-hl-line-mode t)
(set-face-background 'hl-line "#073642")
;; no blink
(blink-cursor-mode (- (*) (*) (*)))
;; tool tips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
;; When emacs asks for "yes" or "no", let "y" or "n" sufficide
(fset 'yes-or-no-p 'y-or-n-p)
;; font
(if (eq window-system 'x)
  (set-default-font "DejaVu Sans Mono-10"))
;; setup right and left margins
;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 0 0)))
