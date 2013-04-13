(require 'flymake-elixir)

(eval-after-load 'elixir-mode
  (add-hook 'elixir-mode-hook 'flymake-elixir-load))

