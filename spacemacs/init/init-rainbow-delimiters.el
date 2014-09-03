(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (defun turn-on-rainbow-delimiters-mode ()
      (interactive)
      (rainbow-delimiters-mode 1))

    (setq-default frame-background-mode 'dark)
    (add-to-hooks
     'turn-on-rainbow-delimiters-mode '(prog-mode-hook
                                        erlang-mode-hook
                                        ))
    ))
