(setq evil-leader/in-all-states t
      evil-leader/leader "SPC"
      evil-leader/non-normal-prefix "s-")
(require 'evil-leader)
;; Unset shortcuts which shadow evil leader
 (eval-after-load "compile"
   '(define-key compilation-mode-map (kbd "SPC") nil))
;; make leader available in visual mode
(define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
(define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
(define-key evil-emacs-state-map  (kbd "SPC") evil-leader--default-map)
