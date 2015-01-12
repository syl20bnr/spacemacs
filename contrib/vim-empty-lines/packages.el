(defvar vim-empty-lines-packages
  '(vim-empty-lines-mode))

(defvar vim-empty-lines-mode-excluded-packages
  '(vi-tilde-fringe)
  "This contrib layer is meant to be an alternative/direct replacement to `vi-tilde-fringe'.")

(defun vim-empty-lines/init-vim-empty-lines-mode ()
  (use-package vim-empty-lines-mode
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'vim-empty-lines-mode) 
      (add-hook 'text-mode-hook 'vim-empty-lines-mode))
    ))
