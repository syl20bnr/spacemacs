;; important for golden-ratio to better work
(setq window-combination-resize t)
;; edit area full screen
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; fringes
(set-fringe-mode nil)  ; default
(setq-default fringe-indicator-alist
              '((truncation . nil) (continuation . nil)))
;; Show column number in mode line
(setq column-number-mode t)
;; line number
(setq linum-format "%4d")
;; highlight current line
(global-hl-line-mode t)
;; no blink
;; (blink-cursor-mode (- (*) (*) (*)))
;; tool tips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
;; When emacs asks for "yes" or "no", let "y" or "n" sufficide
(fset 'yes-or-no-p 'y-or-n-p)
;; font
;; (set-default-font "DejaVu Sans Mono-10")
(pcase system-type
  (`windows-nt
   (progn
     (add-to-list 'default-frame-alist '(font . "Source Code Pro-9"))
     (set-default-font "Source Code Pro-9")))
  (`darwin
   (progn
     (add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
     (set-default-font "Source Code Pro-12")))
  (other (progn
    (add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
    (set-default-font "Source Code Pro-10")))
)
;; setup right and left margins
;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 0 0)))
