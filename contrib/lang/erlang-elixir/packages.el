(defvar erlang-elixir-packages
  '(
    auto-complete
    auto-highlight-symbol
    edts
    elixir-mode
    erlang
    flycheck
    git-gutter-fringe
    rainbow-delimiters
    ruby-end
    smartparens
    yasnippet
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar erlang-elixir-excluded-packages '()
  "List of packages to exclude.")

(defun erlang-elixir/init-auto-complete ()
  (add-hook 'erlang-mode-hook 'auto-complete-mode))

(defun erlang-elixir/init-auto-highlight-symbol ()
  (add-hook 'erlang-mode-hook 'auto-highlight-symbol-mode))

(defun erlang-elixir/init-elixir-mode ()
  (use-package elixir-mode
    :defer t
    :config
    (progn
      (require 'ruby-end)
      (add-to-list 'elixir-mode-hook
                   (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                     (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                          "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                     (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                     (ruby-end-mode +1))))))

(defun erlang-elixir/init-erlang ()
  (use-package erlang
    :mode (("\\.erl?$" . erlang-mode)
           ("\\.hrl?$" . erlang-mode)
           ("\\.spec?$" . erlang-mode))
    :defer t
    :config
    (progn
      (setq erlang-root-dir "/usr/lib/erlang/erts-5.10.3")
      (add-to-list 'exec-path "/usr/lib/erlang/erts-5.10.3/bin")
      (setq erlang-man-root-dir "/usr/lib/erlang/erts-5.10.3/man")
      (setq erlang-compile-extra-opts '(debug_info))
      (require 'erlang-start)
      (add-hook 'erlang-mode-hook
                (lambda ()
                  (setq mode-name "Erlang")
                  ;; when starting an Erlang shell in Emacs, with a custom node name
                  (setq inferior-erlang-machine-options '("-sname" "syl20bnr"))
                  ))
      (unless (eq window-system 'w32)
          (require 'edts-start))
      ;; (setq edts-log-level 'debug)
      ;; (setq edts-face-inhibit-mode-line-updates t)
      (evil-leader/set-key-for-mode 'erlang-mode
        "md" 'edts-find-doc
        "me" 'edts-code-next-issue
        "mG" 'edts-find-global-function
        "mg" 'edts-find-source-under-point
        "mh" 'edts-find-header-source
        "ml" 'edts-find-local-function
        "mm" 'edts-find-macro-source
        "mr" 'edts-find-record-source))))

(defun erlang-elixir/init-flycheck ()
  (add-hook 'elixir-mode-hook 'flycheck-mode))

(defun erlang-elixir/init-git-gutter-fringe ()
  (add-hook 'erlang-mode-hook 'git-gutter-mode))

(defun erlang-elixir/init-rainbow-delimiters ()
  (add-hook 'erlang-mode-hook 'turn-on-rainbow-delimiters-mode))

(defun erlang-elixir/init-ruby-end ()
  (use-package ruby-end
    :defer t
    :config (spacemacs//hide-lighter ruby-end-mode)))

(defun erlang-elixir/init-smartparens ()
  (add-hook 'erlang-mode-hook 'smartparens-mode))

(defun erlang-elixir/init-yasnippet ()
  (add-hook 'erlang-mode-hook 'spacemacs/load-yasnippet))
