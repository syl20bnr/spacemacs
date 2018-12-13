;;; packages.el --- Ruby Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
        chruby
        company
        (enh-ruby-mode :toggle ruby-enable-enh-ruby-mode)
        evil-matchit
        flycheck
        ggtags
        counsel-gtags
        helm-gtags
        minitest
        org
        popwin
        rbenv
        robe
        rspec-mode
        rubocop
        ruby-hash-syntax
        (ruby-mode :location built-in :toggle (not ruby-enable-enh-ruby-mode))
        ruby-refactor
        ruby-test-mode
        ruby-tools
        rvm
        seeing-is-believing
        smartparens
        rake
        ))

(defun ruby/init-bundler ()
  (use-package bundler
    :defer t
    :init (dolist (mode '(ruby-mode enh-ruby-mode))
            (spacemacs/declare-prefix-for-mode mode "mb" "bundle")
            (spacemacs/set-leader-keys-for-major-mode mode
              "bc" 'bundle-check
              "bi" 'bundle-install
              "bs" 'bundle-console
              "bu" 'bundle-update
              "bx" 'bundle-exec
              "bo" 'bundle-open))))

(defun ruby/post-init-company ()
  (when (configuration-layer/package-used-p 'robe)
    (spacemacs|add-company-backends
      :backends company-robe
      :modes ruby-mode enh-ruby-mode))
  (with-eval-after-load 'company-dabbrev-code
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (add-to-list 'company-dabbrev-code-modes mode))))

(defun ruby/init-chruby ()
  (use-package chruby
    :if (equal ruby-version-manager 'chruby)
    :commands chruby-use-corresponding
    :defer t
    :init (spacemacs/add-to-hooks 'chruby-use-corresponding
                                  '(ruby-mode-hook enh-ruby-mode-hook))))

(defun ruby/init-enh-ruby-mode ()
  (use-package enh-ruby-mode
    :mode (("Appraisals\\'" . enh-ruby-mode)
           ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
    :interpreter "ruby"
    :init
    (progn
      (setq enh-ruby-deep-indent-paren nil
            enh-ruby-hanging-paren-deep-indent-level 2)
      (spacemacs/declare-prefix-for-mode 'enh-ruby-mode "mr" "refactor/RuboCop/robe")
      (spacemacs/declare-prefix-for-mode 'enh-ruby-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'enh-ruby-mode "mT" "toggle"))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'enh-ruby-mode
      "T{" 'enh-ruby-toggle-block)))

(defun ruby/post-init-evil-matchit ()
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook `turn-on-evil-matchit-mode)))

(defun ruby/post-init-flycheck ()
  (spacemacs/enable-flycheck 'ruby-mode)
  (spacemacs/enable-flycheck 'enh-ruby-mode))

(defun ruby/post-init-ggtags ()
  (spacemacs/add-to-hooks 'spacemacs/ggtags-mode-enable
                          '(ruby-mode-local-vars-hook
                            enh-ruby-mode-local-vars-hook)))

(defun ruby/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'ruby-mode))

(defun ruby/post-init-helm-gtags ()
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (spacemacs/helm-gtags-define-keys-for-mode mode)))

(defun ruby/init-minitest ()
  (use-package minitest
    :defer t
    :init
    (progn
      (spacemacs/add-to-hooks 'spacemacs//ruby-enable-minitest-mode
                              '(ruby-mode-local-vars-hook
                                enh-ruby-mode-local-vars-hook))
      ;; remove hooks added by minitest mode
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (remove-hook hook 'minitest-enable-appropriate-mode)))
    :config
    (progn
      (spacemacs|hide-lighter minitest-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "ta" 'minitest-verify-all
          "tb" 'minitest-verify
          "tr" 'minitest-rerun
          "ts" 'minitest-verify-single)))))

(defun ruby/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(ruby . t))))

(defun ruby/post-init-popwin ()
  (push '("*Bundler*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  (push '("*projectile-rails-compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  (push '("*projectile-rails-generate*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  (push '("*rake-compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  (push '("*rspec-compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  (push '("^\\*RuboCop.+\\*$" :regexp t :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config))

(defun ruby/init-rbenv ()
  (use-package rbenv
    :if (equal ruby-version-manager 'rbenv)
    :defer t
    :init (spacemacs/add-to-hooks 'spacemacs//enable-rbenv
                                  '(ruby-mode-hook enh-ruby-mode-hook))))

(defun ruby/init-robe ()
  (use-package robe
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'robe 'robe-start "robe")
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (add-hook hook 'robe-mode))
      (spacemacs/add-to-hooks 'robe-jump
                       '(spacemacs-jump-handlers-ruby-mode
                         spacemacs-jump-handlers-enh-ruby-mode)))
    :config
    (progn
      (spacemacs|hide-lighter robe-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mg" "goto")
        (spacemacs/declare-prefix-for-mode mode "mh" "docs")
        (spacemacs/declare-prefix-for-mode mode "mrs" "robe")
        (spacemacs/declare-prefix-for-mode mode "ms" "repl")
        (spacemacs/set-leader-keys-for-major-mode mode
          "'" 'robe-start
          ;; robe mode specific
          "hh" 'robe-doc
          "rsr" 'robe-rails-refresh
          ;; inf-enh-ruby-mode
          "sb" 'ruby-send-buffer
          "sB" 'ruby-send-buffer-and-go
          "sf" 'ruby-send-definition
          "sF" 'ruby-send-definition-and-go
          "si" 'robe-start
          "sl" 'ruby-send-line
          "sL" 'ruby-send-line-and-go
          "sr" 'ruby-send-region
          "sR" 'ruby-send-region-and-go
          "ss" 'ruby-switch-to-inf)))))

(defun ruby/init-rspec-mode ()
  (use-package rspec-mode
    :defer t
    :init
    (progn
      (spacemacs/add-to-hooks 'spacemacs//ruby-enable-rspec-mode
                              '(ruby-mode-local-vars-hook
                                enh-ruby-mode-local-vars-hook))
      ;; remove hooks automatically added by rspec via autoload
      ;; because we want to be able to control when rspec-mode is
      ;; loaded based on the layer variable `ruby-test-runner'
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (remove-hook hook 'rspec-enable-appropriate-mode)))
    :config
    (progn
      (add-hook 'rspec-compilation-mode-hook 'spacemacs//inf-ruby-auto-enter)
      (spacemacs|hide-lighter rspec-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "ta"    'rspec-verify-all
          "tb"    'rspec-verify
          "tc"    'rspec-verify-continue
          "td"    'ruby/rspec-verify-directory
          "te"    'rspec-toggle-example-pendingness
          "tf"    'rspec-verify-method
          "tl"    'rspec-run-last-failed
          "tm"    'rspec-verify-matching
          "tr"    'rspec-rerun
          "tt"    'rspec-verify-single
          "t~"    'rspec-toggle-spec-and-target-find-example
          "t TAB" 'rspec-toggle-spec-and-target)))))

(defun ruby/init-rubocop ()
  (use-package rubocop
    :defer t
    :init (spacemacs/add-to-hooks 'rubocop-mode '(ruby-mode-hook
                                                  enh-ruby-mode-hook))
    :config (dolist (mode '(ruby-mode enh-ruby-mode))
              (spacemacs/declare-prefix-for-mode mode "mrr" "RuboCop")
              (spacemacs/set-leader-keys-for-major-mode mode
                "rrd" 'rubocop-check-directory
                "rrD" 'rubocop-autocorrect-directory
                "rrf" 'rubocop-check-current-file
                "rrF" 'rubocop-autocorrect-current-file
                "rrp" 'rubocop-check-project
                "rrP" 'rubocop-autocorrect-project))))

(defun ruby/init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :mode (("Appraisals\\'" . ruby-mode)
           ("Puppetfile" . ruby-mode))
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'ruby-mode "mr" "refactor/RuboCop/robe")
      (spacemacs/declare-prefix-for-mode 'ruby-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'ruby-mode "mT" "toggle")
      (spacemacs/add-to-hooks
       'spacemacs/ruby-maybe-highlight-debugger-keywords
       '(ruby-mode-local-vars-hook enh-ruby-mode-local-vars-hook)))
    :config (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
              "T'" 'ruby-toggle-string-quotes
              "T{" 'ruby-toggle-block)))

(defun ruby/init-ruby-hash-syntax ()
  (use-package ruby-hash-syntax
    :defer t
    :init
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "xh" 'ruby-hash-syntax-toggle))))

(defun ruby/init-ruby-refactor ()
  (use-package ruby-refactor
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-refactor-mode-launch))
    :config
    (progn
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mrR" "refactor")
        (spacemacs/set-leader-keys-for-major-mode mode
          "rRm" 'ruby-refactor-extract-to-method
          "rRv" 'ruby-refactor-extract-local-variable
          "rRc" 'ruby-refactor-extract-constant
          "rRl" 'ruby-refactor-extract-to-let)))))

(defun ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-tools-mode))
    :config
    (progn
      (spacemacs|hide-lighter ruby-tools-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mx" "text")
        (spacemacs/set-leader-keys-for-major-mode mode
          "x\'" 'ruby-tools-to-single-quote-string
          "x\"" 'ruby-tools-to-double-quote-string
          "x:" 'ruby-tools-to-symbol)))))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode)
    :defer t
    :init (spacemacs/add-to-hooks 'spacemacs//ruby-enable-ruby-test-mode
                                  '(ruby-mode-local-vars-hook
                                    enh-ruby-mode-local-vars-hook))
    :config
    (progn
      ;; `ruby-test-mode' adds a hook to enable itself, this hack
      ;; removes it to be sure that we control the loading of the
      ;; mode
      (remove-hook 'ruby-mode-hook 'ruby-test-enable)
      (spacemacs|hide-lighter ruby-test-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "tb" 'ruby-test-run
          "tt" 'ruby-test-run-at-point))))

(defun ruby/init-rvm ()
  (use-package rvm
    :if (equal ruby-version-manager 'rvm)
    :defer t
    :init
    (progn
      (setq rspec-use-rvm t)
      (spacemacs/add-to-hooks 'rvm-activate-corresponding-ruby
                              '(ruby-mode-hook enh-ruby-mode-hook)))))

(defun ruby/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (sp-with-modes (if ruby-enable-enh-ruby-mode 'enh-ruby-mode 'ruby-mode)
      (sp-local-pair
       "{" "}"
       :pre-handlers '(sp-ruby-pre-handler)
       :post-handlers '(sp-ruby-post-handler
                        (spacemacs/smartparens-pair-newline-and-indent "RET"))
       :suffix ""))))

(defun ruby/init-rake ()
  (use-package rake
    :defer t
    :init (setq rake-cache-file (concat spacemacs-cache-directory "rake.cache"))
    :config (dolist (mode '(ruby-mode enh-ruby-mode))
              (spacemacs/declare-prefix-for-mode mode "mk" "rake")
              (spacemacs/set-leader-keys-for-major-mode mode
                "kk"    'rake
                "kr"    'rake-rerun
                "kR"    'rake-regenerate-cache
                "kf"    'rake-find-task))))

(defun ruby/init-seeing-is-believing ()
  (use-package seeing-is-believing
    :defer t
    :commands (seeing-is-believing seeing-is-believing-run seeing-is-believing-clear)
    :if (executable-find "seeing_is_believing")
    :init
    (progn
      (spacemacs|diminish seeing-is-believing " 👁" " @")
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (add-hook hook 'seeing-is-believing))
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "@@" 'seeing-is-believing-run
          "@c" 'seeing-is-believing-clear)))))
