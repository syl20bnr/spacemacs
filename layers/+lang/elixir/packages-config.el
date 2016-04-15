;;; packages-config.el --- Elixir Layer packages configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun elixir/init-alchemist ()
  (use-package alchemist
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'alchemist 'alchemist-iex-run "alchemist")
      (add-hook 'elixir-mode-hook 'alchemist-mode)
      (setq alchemist-project-compile-when-needed t)
      (push 'alchemist-company company-backends-elixir-mode)
      (push 'alchemist-company company-backends-alchemist-iex-mode))
    :config
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "me" "eval")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mp" "project")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mt" "test")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "ms" "iex")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mm" "mix")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mx" "execute")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mg" "goto")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "el" 'alchemist-eval-current-line
      "eL" 'alchemist-eval-print-current-line
      "er" 'alchemist-eval-region
      "eR" 'alchemist-eval-print-region
      "eb" 'alchemist-eval-buffer
      "eB" 'alchemist-eval-print-buffer
      "ej" 'alchemist-eval-quoted-current-line
      "eJ" 'alchemist-eval-print-quoted-current-line
      "eu" 'alchemist-eval-quoted-region
      "eU" 'alchemist-eval-print-quoted-region
      "ev" 'alchemist-eval-quoted-buffer
      "eV" 'alchemist-eval-print-quoted-buffer

      "pt" 'alchemist-project-find-test
      "gt" 'alchemist-project-toggle-file-and-tests
      "gT" 'alchemist-project-toggle-file-and-tests-other-window

      "h:" 'alchemist-help
      "hH" 'alchemist-help-history
      "hh" 'alchemist-help-search-at-point
      "hr" 'alchemist-help-search-marked-region

      "m:" 'alchemist-mix
      "mc" 'alchemist-mix-compile
      "mx" 'alchemist-mix-run
      "mh" 'alchemist-mix-help

      "'"  'alchemist-iex-run
      "sc" 'alchemist-iex-compile-this-buffer
      "si" 'alchemist-iex-run
      "sI" 'alchemist-iex-project-run
      "sl" 'alchemist-iex-send-current-line
      "sL" 'alchemist-iex-send-current-line-and-go
      "sm" 'alchemist-iex-reload-module
      "sr" 'alchemist-iex-send-region
      "sR" 'alchemist-iex-send-region-and-go

      "ta" 'alchemist-mix-test
      "tb" 'alchemist-mix-test-this-buffer
      "tt" 'alchemist-mix-test-at-point
      "tf" 'alchemist-test-file
      "tn" 'alchemist-test-jump-to-next-test
      "tp" 'alchemist-test-jump-to-previous-test
      "tr" 'alchemist-mix-rerun-last-test

      "xb" 'alchemist-execute-this-buffer
      "xf" 'alchemist-execute-file
      "x:" 'alchemist-execute

      "cb" 'alchemist-compile-this-buffer
      "cf" 'alchemist-compile-file
      "c:" 'alchemist-compile

      "gg" 'alchemist-goto-definition-at-point
      "," 'alchemist-goto-jump-back)

    (dolist (mode (list alchemist-compile-mode-map
                        alchemist-eval-mode-map
                        alchemist-execute-mode-map
                        alchemist-message-mode-map
                        alchemist-help-minor-mode-map
                        alchemist-mix-mode-map
                        alchemist-macroexpand-mode-map
                        alchemist-refcard-mode-map
                        alchemist-test-report-mode-map))
      (evil-define-key 'normal mode
        (kbd "q") 'quit-window))))

(defun elixir/post-init-company ()
  (spacemacs|add-company-hook elixir-mode)
  (spacemacs|add-company-hook alchemist-iex-mode))

(defun elixir/post-init-smartparens ()
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "->" "end"
                   :when '(("RET"))
                   :post-handlers '(:add spacemacs//elixir-do-end-close-action)
                   :actions '(insert)))

  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(:add spacemacs//elixir-do-end-close-action)
                   :actions '(insert))))

(defun elixir/init-elixir-mode ()
  (use-package elixir-mode
    :defer t))

(defun elixir/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*mix*" :tail t :noselect t) popwin:special-display-config)))
