(use-package erlang
  :mode (("\\.erl?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.spec?$" . erlang-mode))
  :config
  (progn
    (setq erlang-root-dir "/usr/lib/erlang/erts-5.10.3")
    (add-to-list 'exec-path "/usr/lib/erlang/erts-5.10.3/bin")
    (setq erlang-man-root-dir "/usr/lib/erlang/erts-5.10.3/man")
    (setq erlang-compile-extra-opts '(debug_info))
    (require 'erlang-start)
    (add-hook 'erlang-mode-hook
              (lambda ()
                ;; when starting an Erlang shell in Emacs, with a custom node name
                (setq inferior-erlang-machine-options '("-sname" "syl20bnr"))
                ))
    (require 'edts-start)
    ;; (setq edts-log-level 'debug)
    ;; (setq edts-face-inhibit-mode-line-updates t)
    ))

;; not needed using EDTS
;; (require 'erlang-flymake)
;; (erlang-flymake-only-on-save)
