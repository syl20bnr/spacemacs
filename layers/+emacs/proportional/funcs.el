(defun proportional/use-monospace ()
  (interactive)
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family proportional-monospace-font)))
