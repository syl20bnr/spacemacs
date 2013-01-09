;; edit area full screen
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)
;; Show column number in mode line
(setq column-number-mode t)
;; line number
(setq linum-format "%4d ")
;; no blink
(blink-cursor-mode (- (*) (*) (*)))
;; tool tips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
;; When emacs asks for "yes" or "no", let "y" or "n" sufficide
(fset 'yes-or-no-p 'y-or-n-p)
;; move focus to newly split window
(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))
;; font
(if (eq window-system 'x)
  (set-default-font "DejaVu Sans Mono-10"))
;; setup right and left margins
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 0 0)))
