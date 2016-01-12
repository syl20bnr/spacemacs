;;; packages.el --- Elixir Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq elixir-packages
  '(
    alchemist
    company
    elixir-mode
    popwin
    ruby-end
    ))

(defun elixir/init-alchemist ()
  (use-package alchemist
    :defer t
    :init
    (progn
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

(defun elixir/init-elixir-mode ()
  (use-package elixir-mode
    :defer t))

(defun elixir/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*mix*" :tail t :noselect t) popwin:special-display-config)))

(defun elixir/init-ruby-end ()
  (use-package ruby-end
    :defer t
    :init
    (progn
      (defun spacemacs//ruby-end ()
        (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
             "\\(?:^\\|\\s-+\\)\\(?:do\\)")
        (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers)
             nil)
        (ruby-end-mode +1))
      (add-hook 'elixir-mode-hook 'spacemacs//ruby-end)
      ;; hack to remove the autoloaded `add-hook' in `ruby-end'
      ;; since they are inserted as an autoload, they have to be removed both
      ;; before and after loading
      (remove-hook 'ruby-mode-hook 'ruby-end-mode)
      (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode))
    :config
    (progn
      (spacemacs|hide-lighter ruby-end-mode)
      ;; see comment in `:init' block
      (remove-hook 'ruby-mode-hook 'ruby-end-mode)
      (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode))))
