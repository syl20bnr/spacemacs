(use-package smartparens-config
  :commands smartparens-mode
  :init
  (add-to-hooks 'smartparens-mode '(prog-mode-hook
                                    erlang-mode-hook
                                    )))
