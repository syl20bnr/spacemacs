(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(setq erlang-root-dir "/usr/local/lib/erlang/erts-5.9.2")
(add-to-list 'exec-path "/usr/local/lib/erlang/erts-5.9.2/bin")
(setq erlang-man-root-dir "/usr/local/lib/erlang/erts-5.9.2/man")

