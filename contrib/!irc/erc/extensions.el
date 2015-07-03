(setq erc-post-extensions
  '(
    erc-tex
    erc-yank
    ))

(defun erc/init-erc-tex ()
  (require 'erc-tex))

(defun erc/init-erc-yank ()
  (use-package erc-yank
    :if (configuration-layer/package-usedp 'gist)
    :init
    (evil-define-key 'normal erc-mode-map
      "p" 'erc-yank)))
