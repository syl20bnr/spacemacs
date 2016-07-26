;;; tmux.el --- Seamlessly navigate between Emacs and tmux

;; Author:   Keith Smiley <keithbsmiley@gmail.com>
;; Created:  April 25 2014
;; Version:  0.1.5
;; Keywords: tmux, evil, vi, vim

;;; Commentary:

;; This package is inspired by vim-tmux-navigator.
;; It allows you to navigate splits in evil mode
;; Along with tmux splits with the same commands
;; Include with:
;;
;;    (require 'navigate)
;;

;;; Code:

(require 'evil)

(defgroup navigate nil
  "seamlessly navigate between Emacs and tmux"
  :prefix "navigate-"
  :group 'evil)

; This requires windmove commands
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun tmux-navigate (direction)
  (let
    ((cmd (concat "windmove-" direction)))
      (condition-case nil
          (funcall (intern cmd))
        (error
          (tmux-command direction)))))

(defun tmux-command (direction)
  (shell-command-to-string
    (concat "tmux select-pane -"
      (tmux-direction direction))))

(defun tmux-direction (direction)
  (upcase
    (substring direction 0 1)))

(defun tmux-nav-left ()
  (interactive)
  (tmux-navigate "left"))

(defun tmux-nav-right ()
  (interactive)
  (tmux-navigate "right"))

(defun tmux-nav-up ()
  (interactive)
  (tmux-navigate "up"))

(defun tmux-nav-down ()
  (interactive)
  (tmux-navigate "down"))

(define-key evil-normal-state-map (kbd "C-h") #'tmux-nav-left)
(define-key evil-normal-state-map (kbd "C-j") #'tmux-nav-down)
(define-key evil-normal-state-map (kbd "C-k") #'tmux-nav-up)
(define-key evil-normal-state-map (kbd "C-l") #'tmux-nav-right)
(define-key evil-motion-state-map (kbd "C-h") #'tmux-nav-left)
(define-key evil-motion-state-map (kbd "C-j") #'tmux-nav-down)
(define-key evil-motion-state-map (kbd "C-k") #'tmux-nav-up)
(define-key evil-motion-state-map (kbd "C-l") #'tmux-nav-right)

;; Modify `evil-evilified-state-map-original' because `evil-evilified-state-map'
;; is reset to this value each time the evilify macro is run.
(when (boundp 'evil-evilified-state-map-original)
  (define-key evil-evilified-state-map-original (kbd "C-h") #'tmux-nav-left)
  (define-key evil-evilified-state-map-original (kbd "C-j") #'tmux-nav-down)
  (define-key evil-evilified-state-map-original (kbd "C-k") #'tmux-nav-up)
  (define-key evil-evilified-state-map-original (kbd "C-l") #'tmux-nav-right))

(provide 'tmux)

;;; tmux.el ends here
