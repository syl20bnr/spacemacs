(setq evil-mode-line-format 'before)

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("orange" box))
(setq evil-visual-state-cursor '("black" box))
(setq evil-insert-state-cursor '("green3" box))
(setq evil-motion-state-cursor '("purple" box))

;; from https://github.com/roman/emacs.d/blob/master/zoo/zoo-evil.el
(evil-define-command fd-to-normal-mode ()
  "Allows to get into 'normal' mode using 'fd'."
  :repeat change
  (let ((modified (buffer-modified-p)))
    (insert "f")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?d)
                           nil 0.5)))
      (cond
       ((null evt)
          (message ""))
       ((and (integerp evt)
             (char-equal evt ?d))
          ;; remove the f character
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t ; otherwise
          (setq unread-command-events (append unread-command-events
                                              (list evt))))))))

(evil-mode 1)

;; This is an endless debate and is just a matter of convention
;; I prefer to stay on the original character when leaving insert mode
;; (initiated with 'i').
(setq evil-move-cursor-back nil)


