(defun spacemacs/imenu-list-smart-focus ()
  "Focus the `imenu-list' buffer, creating as necessary.
If the imenu-list buffer is displayed in any window, focus it, otherwise create and focus.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
  (interactive)
  (if (get-buffer-window imenu-list-buffer-name t)
      (imenu-list-show)
    (imenu-list-smart-toggle)))
