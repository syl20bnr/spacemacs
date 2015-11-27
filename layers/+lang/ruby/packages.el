;;; packages.el --- Ruby Layer packages File for Spacemacs
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

(setq ruby-packages
      '(
        bundler
        company
        evil-matchit
        flycheck
        robe
        ruby-test-mode
        rspec-mode
        ruby-tools
        rubocop
        ))

(if ruby-enable-enh-ruby-mode
    (add-to-list 'ruby-packages 'enh-ruby-mode)
  (add-to-list 'ruby-packages 'ruby-mode))

(when ruby-version-manager
  (add-to-list 'ruby-packages ruby-version-manager))

(defun ruby/init-rbenv ()
  "Initialize RBENV mode"
  (use-package rbenv
    :defer t
    :init (global-rbenv-mode)
    :config (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
              (add-hook hook (lambda () (rbenv-use-corresponding))))))

(defun ruby/init-rvm ()
  "Initialize RVM mode"
  (use-package rvm
    :defer t
    :init (rvm-use-default)
    :config
    (progn
      (setq rspec-use-rvm t)
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
              (add-hook hook
                        (lambda () (rvm-activate-corresponding-ruby)))))))

(defun ruby/init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
        "'" 'ruby-toggle-string-quotes
        "{" 'ruby-toggle-block)
      (sp-with-modes 'ruby-mode
        (sp-local-pair "{" "}"
                       :pre-handlers '(sp-ruby-pre-handler)
                       :post-handlers '(sp-ruby-post-handler (spacemacs/smartparens-pair-newline-and-indent "RET"))
                       :suffix "")))))

(defun ruby/init-enh-ruby-mode ()
  "Initialize Ruby Mode"
  (use-package enh-ruby-mode
    :mode (("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
    :interpreter "ruby"
    :config
    (progn
      (setq enh-ruby-deep-indent-paren nil
            enh-ruby-hanging-paren-deep-indent-level 2)
      (sp-with-modes 'enh-ruby-mode
        (sp-local-pair "{" "}"
                       :pre-handlers '(sp-ruby-pre-handler)
                       :post-handlers '(sp-ruby-post-handler (spacemacs/smartparens-pair-newline-and-indent "RET"))
                       :suffix "")))))

(defun ruby/post-init-evil-matchit ()
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook `turn-on-evil-matchit-mode)))

(defun ruby/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'ruby-mode)
  (spacemacs/add-flycheck-hook 'enh-ruby-mode))

(defun ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init
    (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
      (add-hook hook 'ruby-tools-mode))
    :config
    (progn
      (spacemacs|hide-lighter ruby-tools-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mx" "ruby/text")
        (spacemacs/set-leader-keys-for-major-mode mode
          "x\'" 'ruby-tools-to-single-quote-string
          "x\"" 'ruby-tools-to-double-quote-string
          "x:" 'ruby-tools-to-symbol)))))

(defun ruby/init-bundler ()
  (use-package bundler
    :defer t
    :init
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/declare-prefix-for-mode mode "mb" "ruby/bundle")
      (spacemacs/set-leader-keys-for-major-mode mode
        "bc" 'bundle-check
        "bi" 'bundle-install
        "bs" 'bundle-console
        "bu" 'bundle-update
        "bx" 'bundle-exec))))

(defun ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init
    (progn
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (add-hook hook 'robe-mode))
      (when (configuration-layer/layer-usedp 'auto-completion)
        (push 'company-robe company-backends-enh-ruby-mode)
        (push 'company-robe company-backends-ruby-mode)))
    :config
    (progn
      (spacemacs|hide-lighter robe-mode)

      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mg" "ruby/goto")
        (spacemacs/declare-prefix-for-mode mode "mh" "ruby/docs")
        (spacemacs/declare-prefix-for-mode mode "ms" "ruby/repl")
        (spacemacs/set-leader-keys-for-major-mode mode
          ;; robe mode specific
          "gg" 'robe-jump
          "hd" 'robe-doc
          "rsr" 'robe-rails-refresh
          ;; inf-enh-ruby-mode
          "sf" 'ruby-send-definition
          "sF" 'ruby-send-definition-and-go
          "si" 'robe-start
          "sr" 'ruby-send-region
          "sR" 'ruby-send-region-and-go
          "ss" 'ruby-switch-to-inf)))))

(defun ruby/init-rspec-mode ()
  "Define keybindings for rspec mode"
  (use-package rspec-mode
    :defer t
    :init
    (progn
      (defun spacemacs//ruby-enable-rspec-mode ()
        "Conditionally enable `rspec-mode'"
        (when (eq 'rspec ruby-test-runner)
          (rspec-mode)))
      (spacemacs/add-to-hooks
       'spacemacs//ruby-enable-rspec-mode '(ruby-mode-hook
                                            enh-ruby-mode-hook)))
    :config
    (progn
      (spacemacs|hide-lighter rspec-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "ta" 'rspec-verify-all
          "tc" 'rspec-verify-matching
          "tl" 'rspec-run-last-failed
          "tr" 'rspec-rerun
          "tt" 'rspec-verify-single)))))

(defun ruby/init-rubocop ()
  (use-package rubocop
    :defer t
    :init (spacemacs/add-to-hooks 'rubocop-mode '(ruby-mode-hook
                                                  enh-ruby-mode-hook))
    :config
    (progn
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mrr" "ruby/RuboCop")
        (spacemacs/set-leader-keys-for-major-mode mode
          "rrd" 'rubocop-check-directory
          "rrD" 'rubocop-autocorrect-directory
          "rrf" 'rubocop-check-current-file
          "rrF" 'rubocop-autocorrect-current-file
          "rrp" 'rubocop-check-project
          "rrP" 'rubocop-autocorrect-project)))))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode)
    :defer t
    :init
    (progn
      (defun spacemacs//ruby-enable-ruby-test-mode ()
        "Conditionally enable `ruby-test-mode'"
        (when (eq 'ruby-test ruby-test-runner)
          (ruby-test-mode)))
      (spacemacs/add-to-hooks
       'spacemacs//ruby-enable-ruby-test-mode '(ruby-mode-hook
                                                enh-ruby-mode-hook)))
    :config
    (progn
      (spacemacs|hide-lighter ruby-test-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "tb" 'ruby-test-run
          "tt" 'ruby-test-run-at-point))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun ruby/post-init-company ()
    (spacemacs|add-company-hook ruby-mode)
    (spacemacs|add-company-hook enh-ruby-mode)

    (with-eval-after-load 'company-dabbrev-code
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (push mode company-dabbrev-code-modes)))))
