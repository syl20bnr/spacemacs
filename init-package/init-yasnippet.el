(use-package helm-c-yasnippet
  :commands helm-c-yas-complete
  :config
  (progn
      (setq helm-c-yas-space-match-any-greedy t)
      (use-package yasnippet
        :config
        (progn
          (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
          (yas-global-mode 1)
          ))
      )
  )
