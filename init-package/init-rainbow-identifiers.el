(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode
  :init
  (add-to-hooks 'rainbow-identifiers-mode '(prog-mode-hook
                                            erlang-mode-hook)))
