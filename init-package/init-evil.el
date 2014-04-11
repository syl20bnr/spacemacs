(use-package evil
  :init
  (progn
    (setq evil-mode-line-format 'before)
    (setq evil-emacs-state-cursor  '("red" box))
    (setq evil-normal-state-cursor '("orange" box))
    (setq evil-visual-state-cursor '("black" box))
    (setq evil-insert-state-cursor '("green3" box))
    (setq evil-motion-state-cursor '("purple" box))
    ;; This is an endless debate and is just a matter of convention
    ;; I prefer to stay on the original character when leaving insert mode
    ;; (initiated with 'i').
    (setq evil-move-cursor-back nil)
    (evil-mode 1))
  :config
  (progn
    ;; inspired from:
    ;; https://github.com/roman/emacs.d/blob/master/zoo/zoo-evil.el
    (evil-define-command fd-trigger (callback)
      "Allows to execute the passed function using 'fd'."
      :repeat change
      (let ((modified (buffer-modified-p)))
        (insert "f")
        (let ((evt (read-event
                    (format "Insert %c to exit insert state" ?d)
                    nil 0.2)))
          (cond
           ((null evt)
              (message ""))
           ((and (integerp evt)
                 (char-equal evt ?d))
              ;; remove the f character
              (delete-char -1)
              (set-buffer-modified-p modified)
              (funcall callback))
           (t ; otherwise
              (setq unread-command-events (append unread-command-events
                                                  (list evt))))))))
    ;; load evil-leader
    (use-package evil-leader
      :init
      (progn
        (setq evil-leader/in-all-states t
              evil-leader/leader "SPC"
              evil-leader/non-normal-prefix "s-"))
      :config
      (progn
      ;; Unset shortcuts which shadow evil leader
      (eval-after-load "compile"
        '(define-key compilation-mode-map (kbd "SPC") nil))
      (eval-after-load "dired"
        '(define-key dired-mode-map (kbd "SPC") nil))
      ;; make leader available in visual mode
      (define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
      (define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
      (define-key evil-emacs-state-map  (kbd "SPC") evil-leader--default-map)))
    ;; load surround
    (use-package surround
      :init
      (global-surround-mode 1)) 
    ;; load evil-exchange
    (use-package evil-exchange
      :init
      (evil-exchange-install)))
  )


