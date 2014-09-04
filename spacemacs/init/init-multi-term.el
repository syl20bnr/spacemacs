(use-package multi-term
  :defer t
  :init
  (evil-leader/set-key "ast" 'multi-term)
  :config
  (progn
    (setq multi-term-program "/bin/zsh")

    (defun term-send-tab ()
      "Send tab in term mode."
      (interactive)
      (term-send-raw-string "\t"))

    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))))

