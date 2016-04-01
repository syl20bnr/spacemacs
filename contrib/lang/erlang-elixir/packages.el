;;; packages.el --- Erlang and Elixir Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar erlang-elixir-packages
  '(
    auto-complete
    auto-highlight-symbol
    edts
    elixir-mode
    erlang
    flycheck
    git-gutter-fringe
    evil-iedit-state
    rainbow-delimiters
    ruby-end
    smartparens
    yasnippet
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun erlang-elixir/init-auto-complete ()
  (add-hook 'erlang-mode-hook 'auto-complete-mode))

(defun erlang-elixir/init-auto-highlight-symbol ()
  (add-hook 'erlang-mode-hook 'auto-highlight-symbol-mode))

(defun erlang-elixir/init-edts ()

  (defun erlang-elixir//load-edts ()
    "Return non-nil if EDTS can be loaded."
    (and spacemacs-erlang-elixir-use-edts
         (not (eq window-system 'w32))))

  (defun erlang-elixir//edts-start ()
    "Starts EDTS."
    (if (erlang-elixir//load-edts)
        (require 'edts-start))))

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
      (if (and (fboundp 'erlang-elixir//load-edts)
               (erlang-elixir//load-edts))
          (erlang-elixir//edts-start))
      ;; (setq edts-log-level 'debug)
      ;; (setq edts-face-inhibit-mode-line-updates t)
      (evil-leader/set-key-for-mode 'erlang-mode
        "me"  'edts-code-next-issue
        "mGg" 'edts-find-global-function
        "mGh" 'edts-find-header-source
        "mGl"  'edts-find-local-function
        "mGr" 'edts-find-record-source
        "mg"  'edts-find-source-under-point
        "mhd" 'edts-find-doc
        "mm"  'edts-find-macro-source))))

(defun erlang-elixir/init-flycheck ()
  (add-hook 'elixir-mode-hook 'flycheck-mode)
  (unless spacemacs-erlang-elixir-use-edts
    (add-hook 'erlang-mode-hook 'flycheck-mode)))

(defun erlang-elixir/init-git-gutter-fringe ()
  (add-hook 'erlang-mode-hook 'git-gutter-mode))

(defun erlang-elixir/init-evil-iedit-state ()
  (add-hook 'erlang-mode-hook 'spacemacs/evil-state-lazy-loading))


(defun erlang-elixir/init-rainbow-delimiters ()
  (add-hook 'erlang-mode-hook 'turn-on-rainbow-delimiters-mode))

(defun erlang-elixir/init-ruby-end ()
  (use-package ruby-end
    :defer t
    :config (spacemacs|hide-lighter ruby-end-mode)))

(defun erlang-elixir/init-smartparens ()
  (add-hook 'erlang-mode-hook
            (if dotspacemacs-smartparens-strict-mode
                'smartparens-strict-mode
              'smartparens-mode)))

(defun erlang-elixir/init-yasnippet ()
  (add-hook 'erlang-mode-hook 'spacemacs/load-yasnippet))
