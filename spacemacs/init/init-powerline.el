(use-package powerline
  :init
  (progn
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
      (let ((num (window-numbering-get-number-string)))
        (cond ((not (display-graphic-p)) (concat "(" num ")"))
              ((equal num "1")  "➊")
              ((equal num "2")  "➋")
              ((equal num "3")  "➌")
              ((equal num "4")  "➍")
              ((equal num "5")  "➎")
              ((equal num "6")  "❻")
              ((equal num "7")  "➐")
              ((equal num "8")  "➑")
              ((equal num "9")  "➒")
              ((equal num "0")  "➓")
              (t (concat "(" num ")")))))

    (defpowerline powerline-evil-mode
      (gcs-propertized-evil-mode-tag))

    (defvar powerline-minor-modesp nil)
    (defun powerline-minor-modes-toggle ()
      "Toggle display of minor modes."
      (interactive)
      (if powerline-minor-modesp
          (setq powerline-minor-modesp nil)
        (setq powerline-minor-modesp t)))

    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (eq (frame-selected-window) (selected-window)))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (lhs (append (list
                                          (powerline-window-number face1 'l)
                                          (powerline-evil-mode face1 'l)

                                          (powerline-raw "%*" nil 'l)
                                          (powerline-buffer-size nil 'l)
                                          (powerline-buffer-id nil 'l)
                                          (powerline-raw " " nil)

                                          (powerline-arrow-right nil face1)
                                          (powerline-major-mode face1 'l)
                                          (powerline-raw " " face1))

                                         (if powerline-minor-modesp
                                             (list (powerline-arrow-right face1 nil)
                                                   (powerline-minor-modes nil 'l)
                                                   (powerline-raw mode-line-process nil 'l)
                                                   (powerline-raw " " nil)
                                                   (powerline-arrow-right nil face2))
                                           (list (powerline-raw " " face1)
                                                 (powerline-arrow-right face1 face2)))

                                         (list (powerline-vc face2))))
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
                        (powerline-render lhs)
                        (powerline-fill face2 (powerline-width rhs))
                        (powerline-render rhs))))))
    ))
