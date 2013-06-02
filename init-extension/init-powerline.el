(require 'powerline)
;; Setup modeline items
(defun gcs-propertized-evil-mode-tag ()
  (propertize evil-mode-line-tag 'font-lock-face
    ;; Don't propertize if we're not in the selected buffer
    (cond ((not (eq (current-buffer) (car (buffer-list)))) '())
          ((evil-insert-state-p) '(:background "green3" :foreground "black"))
          ((evil-emacs-state-p)  '(:background "red" :foreground "black"))
          ((evil-motion-state-p) '(:background "purple" :foreground "black"))
          ((evil-visual-state-p) '(:background "gray" :foreground "black"))
          ((evil-normal-state-p)  '(:background "orange" :foreground "black"))
          (t '()))))

(defpowerline powerline-window-number 
  (concat "(" (window-numbering-get-number-string) ")"))

(setq-default mode-line-format
'("%e"
  (:eval
   (let* ((active (eq (frame-selected-window) (selected-window)))
          (face1 (if active 'powerline-active1 'powerline-inactive1))
          (face2 (if active 'powerline-active2 'powerline-inactive2))
          (lhs (list
                (powerline-window-number face1 'l)
                (powerline-raw " " face1)
                (powerline-arrow-right face1 nil)

                (powerline-raw "%*" nil 'l)
                (powerline-buffer-size nil 'l)
                (powerline-buffer-id nil 'l)
                (powerline-raw " " nil)

                (powerline-arrow-right nil face1)
                (powerline-major-mode face1 'l)
                (powerline-raw " " face1)

                (powerline-arrow-right face1 nil)
                (powerline-minor-modes nil 'l)
                (powerline-raw mode-line-process nil 'l)
                (powerline-raw " " nil)
                (powerline-arrow-right nil face2)

                (powerline-vc face2)
                ))
          (rhs (list
                (powerline-raw global-mode-string face2 'r)
                (powerline-raw " " face2)

                (powerline-arrow-left face2 face1)
                (powerline-raw " " face1)
                (powerline-raw "%l:%2c" face1 'r)
                (powerline-arrow-left face1 nil)
                (powerline-raw " " nil)
                (powerline-raw "%p" nil 'r)

                (powerline-hud face2 face1))))
     (concat
      (gcs-propertized-evil-mode-tag)
      (powerline-render lhs)
      (powerline-fill face2 (powerline-width rhs))
      (powerline-render rhs))))))
