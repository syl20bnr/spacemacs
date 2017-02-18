(setq vim-empty-lines-packages
      '(
        (vim-empty-lines-mode :location local)
        (vi-tilde-fringe :excluded t)
        ))

(defun vim-empty-lines/init-vim-empty-lines-mode ()
  (use-package vim-empty-lines-mode
    :init
    (spacemacs/add-to-hooks (lambda () (vim-empty-lines-mode -1))
                            '(comint-mode-hook
                              eshell-mode-hook
                              eww-mode-hook
                              shell-mode-hook
                              term-mode-hook))
    :config
    (progn
      (spacemacs|hide-lighter vim-empty-lines-mode)
      (global-vim-empty-lines-mode)
      (spacemacs|add-toggle vim-empty-lines-mode
        :mode global-vim-empty-lines-mode
        :documentation
        "Display an overlay of ~ on empty lines."
        :evil-leader "t~")
      ;; Don't enable it where it is detrimental.
      (dolist (x (list spacemacs-buffer-name
                       "*Messages*"))
        (with-current-buffer x (vim-empty-lines-mode -1)))
      (add-hook 'which-key-init-buffer-hook (lambda () (vim-empty-lines-mode -1)))
      ;; after a major mode is loaded, check if the buffer is read only
      ;; if so, disable vim-empty-lines-mode
      (add-hook 'after-change-major-mode-hook (lambda ()
                                                (when buffer-read-only
                                                  (vim-empty-lines-mode -1)))))))
