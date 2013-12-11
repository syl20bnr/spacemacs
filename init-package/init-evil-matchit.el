(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items
    nil 'evilmi-select-items
    nil 'evilmi-delete-items))

(use-package evil-matchit
  :init
  (progn (global-evil-matchit-mode)))
