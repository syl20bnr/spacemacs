;;; packages.el --- Elixir Layer packages File for Spacemacs
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

(setq elixir-packages
  '(
    alchemist
    company
    elixir-mode
    flycheck
    ruby-end
    ))

(defun elixir/init-alchemist ()
  (use-package alchemist
    :defer t
    :init
    (progn
      (add-hook 'elixir-mode-hook 'alchemist-mode)
      (setq alchemist-project-compile-when-needed t)
      (push 'alchemist-company company-backends-elixir-mode))
    :config
    (evil-leader/set-key-for-mode 'elixir-mode
      "mel" 'alchemist-eval-current-line
      "meL" 'alchemist-eval-print-current-line
      "mer" 'alchemist-eval-region
      "meR" 'alchemist-eval-print-region
      "meb" 'alchemist-eval-buffer
      "meB" 'alchemist-eval-print-buffer
      "mej" 'alchemist-eval-quoted-current-line
      "meJ" 'alchemist-eval-print-quoted-current-line
      "meu" 'alchemist-eval-quoted-region
      "meU" 'alchemist-eval-print-quoted-region
      "mev" 'alchemist-eval-quoted-buffer
      "meV" 'alchemist-eval-print-quoted-buffer

      "mpt" 'alchemist-project-find-test
      "mgt" 'alchemist-project-toggle-file-and-tests
      "mgT" 'alchemist-project-toggle-file-and-tests-other-window

      "mh:" 'alchemist-help
      "mhH" 'alchemist-help-history
      "mhh" 'alchemist-help-search-at-point
      "mhr" 'alchemist-help-search-marked-region

      "mm:" 'alchemist-mix
      "mmc" 'alchemist-mix-compile
      "mmx" 'alchemist-mix-run
      "mmh" 'alchemist-mix-help

      "msi" 'alchemist-iex-run
      "msI" 'alchemist-iex-project-run
      "msl" 'alchemist-iex-send-current-line
      "msL" 'alchemist-iex-send-current-line-and-go
      "msr" 'alchemist-iex-send-region
      "msR" 'alchemist-iex-send-region-and-go
      "msc" 'alchemist-iex-compile-this-buffer

      "mta" 'alchemist-mix-test
      "mtb" 'alchemist-mix-test-this-buffer
      "mtt" 'alchemist-mix-test-at-point
      "mtf" 'alchemist-test-file
      "mtn" 'alchemist-test-jump-to-next-test
      "mtp" 'alchemist-test-jump-to-previous-test

      "mxb" 'alchemist-execute-this-buffer
      "mxf" 'alchemist-execute-file
      "mx:" 'alchemist-execute

      "mcb" 'alchemist-compile-this-buffer
      "mcf" 'alchemist-compile-file
      "mc:" 'alchemist-compile

      "mgg" 'alchemist-goto-definition-at-point
      "m," 'alchemist-goto-jump-back)))

(defun elixir/post-init-company ()
  (spacemacs|add-company-hook elixir-mode))

(defun elixir/init-elixir-mode ()
  (use-package elixir-mode
    :defer t))

(defun elixir/post-init-flycheck ()
  (add-hook 'elixir-mode-hook 'flycheck-mode))

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
      (add-hook 'elixir-mode-hook 'spacemacs//ruby-end))
    :config
    (progn
      (spacemacs|hide-lighter ruby-end-mode)
      ;; hack to remove the autoloaded `add-hook' in `ruby-end'
      (remove-hook 'ruby-mode-hook 'ruby-end-mode)
      (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode))))
