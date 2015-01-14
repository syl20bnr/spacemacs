(defvar vim-empty-lines-packages
  '(vim-empty-lines-mode))

(defvar vim-empty-lines-excluded-packages
  '(vi-tilde-fringe))

(defun vim-empty-lines/init-vim-empty-lines-mode ()
  (use-package vim-empty-lines-mode
    :defer t
    :init
    (add-to-hooks 'vim-empty-lines-mode '(prog-mode-hook
                                          erlang-mode-hook
                                          text-mode-hook))
    :config
    (spacemacs|hide-lighter vim-empty-lines-mode)))
