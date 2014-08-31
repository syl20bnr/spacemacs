(use-package smartparens-config
  :commands smartparens-mode
  :init
  (add-to-hooks 'smartparens-mode '(erlang-mode-hook
                                    markdown-mode-hook
                                    prog-mode-hook
                                    )))
