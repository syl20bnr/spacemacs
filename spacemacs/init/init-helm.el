(use-package helm
  :defer t
  :init
  (add-to-hooks 'helm-mode '(erlang-mode-hook
                             markdown-mode-hook
                             org-mode-hook
                             prog-mode-hook
                             )))
