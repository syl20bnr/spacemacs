(setq erc-post-extensions
  '(
    erc-tex
    erc-yank
    ))

(defun erc/init-erc-tex ()
  (use-package erc-tex
    :init
    ))

(defun erc/init-erc-yank ()
  (use-package erc-yank
    :init
    (bind-key "C-y" 'erc-yank erc-mode-map)))
