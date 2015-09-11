(require 'dired-x)
(define-key evil-normal-state-map (kbd "-") 'dired-jump)

(add-hook 'dired-mode-hook 'vinegar/dired-setup)

(eval-after-load "dired-mode"
  (evilify dired-mode dired-mode-map
           "j"         'vinegar/move-down
           "k"         'vinegar/move-up
           "-"         'vinegar/up-directory
           "0"         'dired-back-to-start-of-files
           "="         'vinegar/dired-diff
           (kbd "C-j") 'dired-next-subdir
           (kbd "C-k") 'dired-prev-subdir
           "I"         'vinegar/dotfiles-toggle
           (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
           (kbd "RET") (if vinegar-reuse-dired-buffer
                           'dired-find-alternate-file
                         'dired-find-file)
           "f"         'helm-find-files
           "J"         'dired-goto-file
           (kbd "C-f") 'find-name-dired
           "H"         'diredp-dired-recent-dirs
           "T"         'dired-tree-down
           "K"         'dired-do-kill-lines
           "r"         'revert-buffer
           "C-r"       'dired-do-redisplay
           "gg"        'vinegar/back-to-top
           "G"         'vinegar/jump-to-bottom))
