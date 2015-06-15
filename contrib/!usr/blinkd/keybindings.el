(global-set-key (kbd "C-x x") 'call-last-kbd-macro)

(cond ((system-is-linux)
       (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
       (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease))
      ((system-is-mswindows)
       (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
       (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)))
