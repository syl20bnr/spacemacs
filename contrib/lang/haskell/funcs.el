(defun spacemacs/haskell-show-hi2-guides ()
  (when (and (boundp 'hi2-mode) hi2-mode)
    (hi2-enable-show-indentations)))

(defun spacemacs/haskell-hide-hi2-guides ()
  (when (and (boundp 'hi2-mode) hi2-mode)
    (hi2-disable-show-indentations)))
