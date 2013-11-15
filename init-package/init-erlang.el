(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.spec?$" . erlang-mode))
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

;; not needed using EDTS
;; (require 'erlang-flymake)
;; (erlang-flymake-only-on-save)
