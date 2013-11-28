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
    ;; Key Bindings
    (evil-leader/set-key "Mec" 'edts-who-calls)
    (evil-leader/set-key "Med" 'edts-find-doc)
    (evil-leader/set-key "Mef" 'edts-find-source-under-point)
    (evil-leader/set-key "Meg" 'edts-find-global-function)
    (evil-leader/set-key "Meh" 'edts-find-header-source)
    (evil-leader/set-key "Mel" 'edts-find-local-function)
    (evil-leader/set-key "Mem" 'edts-find-macro-source)
    (evil-leader/set-key "Men" 'edts-code-next-issue)
    (evil-leader/set-key "Mer" 'edts-find-record-source)
    (evil-leader/set-key "Mex" 'edts-refactor-extract-function)))

;; not needed using EDTS
;; (require 'erlang-flymake)
;; (erlang-flymake-only-on-save)
