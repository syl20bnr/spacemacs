(defvar colors-post-extensions '(nyan-mode))

(defun colors/init-nyan-mode ()
  (use-package nyan-mode
    :if colors-enable-nyan-cat-progress-bar
    :init
    (progn
      (setq nyan-wavy-trail t)
      (setq nyan-animate-nyancat t)
      (nyan-mode)

      (spacemacs|add-toggle nyan-cat-progress-bar
                            :status nyan-mode
                            :on (nyan-mode)
                            :off (nyan-mode -1)
                            :documentation "Show a nyan cat progress bar in the mode-line."
                            :evil-leader "tmn")

      (defun spacemacs/powerline-nyan-cat ()
        "Construct a powerline segment for nyan cat."
        (let* ((active (powerline-selected-window-active))
               (l (1+ (truncate (powerline-width lhs))))
               (r (1+ (truncate (powerline-width rhs))))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (separator-left
                (intern (format "powerline-%s-%s"
                                powerline-default-separator
                                (car powerline-default-separator-dir))))
               (separator-right
                (intern (format "powerline-%s-%s"
                                powerline-default-separator
                                (cdr powerline-default-separator-dir)))))
          (setq nyan-bar-length (min 32 (/ (- (window-total-width) l r) 2)))
          (list
           (funcall separator-right face2 face1)
           (powerline-raw (nyan-create) face1)
           (funcall separator-left face1 face2)))))))
