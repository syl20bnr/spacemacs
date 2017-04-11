;; Key Bindings for habitrpg.el
(spacemacs/set-leader-keys "aH" 'habitrpg-status)
(add-hook 'org-after-todo-state-change-hook 'habitrpg-add 'append)
