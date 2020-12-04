;;; packages.el --- Ruby Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst ruby-packages
  '(
    bundler
    chruby
    company
    counsel-gtags
    dap-mode
    (enh-ruby-mode :toggle ruby-enable-enh-ruby-mode)
    evil-matchit
    flycheck
    ggtags
    helm-gtags
    minitest
    org
    popwin
    rake
    rbenv
    robe
    rspec-mode
    rubocop
    rubocopfmt
    ruby-hash-syntax
    (ruby-mode :location built-in :toggle (not ruby-enable-enh-ruby-mode))
    ruby-refactor
    ruby-test-mode
    ruby-tools
    rvm
    seeing-is-believing
    smartparens))

(defun ruby/init-bundler ()
  (use-package bundler
    :defer t
    :init (dolist (mode '(ruby-mode enh-ruby-mode))
            (space-macs/declare-prefix-for-mode mode "mb"
              (if (eq (space-macs//ruby-backend) 'lsp) "build/bundle" "bundle"))
            (space-macs/set-leader-keys-for-major-mode mode
              "bc" 'bundle-check
              "bi" 'bundle-install
              "bs" 'bundle-console
              "bu" 'bundle-update
              "bx" 'bundle-exec
              "bo" 'bundle-open))))

(defun ruby/init-chruby ()
  (use-package chruby
    :if (equal ruby-version-manager 'chruby)
    :commands chruby-use-corresponding
    :defer t
    :init (space-macs/add-to-hooks 'chruby-use-corresponding
                                  '(ruby-mode-hook enh-ruby-mode-hook))))

(defun ruby/post-init-company ()
  (add-hook 'ruby-mode-local-vars-hook #'space-macs//ruby-setup-company))

(defun ruby/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'ruby-mode)
  (space-macs/counsel-gtags-define-keys-for-mode 'enh-ruby-mode))

(defun ruby/pre-init-dap-mode ()
  (pcase (space-macs//ruby-backend)
    (`lsp (add-to-list 'space-macs--dap-supported-modes 'ruby-mode)
          (add-to-list 'space-macs--dap-supported-modes 'enh-ruby-mode)))
  (space-macs/add-to-hooks #'space-macs//ruby-setup-dap
                          '(ruby-mode-local-vars-hook
                            enh-ruby-mode-local-vars-hook)))

(defun ruby/init-enh-ruby-mode ()
  (use-package enh-ruby-mode
    :mode (("Appraisals\\'" . enh-ruby-mode)
           ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\|pryrc\\)\\'" . enh-ruby-mode))
    :interpreter "ruby"
    :init
    (progn
      (setq enh-ruby-deep-indent-paren nil
            enh-ruby-hanging-paren-deep-indent-level 2)
      (space-macs/declare-prefix-for-mode 'enh-ruby-mode "mi" "insert")
      (space-macs/declare-prefix-for-mode 'enh-ruby-mode "mt" "test")

      (add-hook 'enh-ruby-mode-hook #'space-macs//ruby-setup-backend)
      (add-hook 'enh-ruby-mode-local-vars-hook
                #'space-macs/ruby-maybe-highlight-debugger-keywords))
    :config
    (space-macs/set-leader-keys-for-major-mode 'enh-ruby-mode
      "if"  'space-macs/ruby-insert-frozen-string-literal-comment
      "is"  'space-macs/ruby-insert-shebang
      "r{" 'enh-ruby-toggle-block
      "r}" 'enh-ruby-toggle-block)))

(defun ruby/post-init-evil-matchit ()
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook `turn-on-evil-matchit-mode)))

(defun ruby/post-init-flycheck ()
  (space-macs/enable-flycheck 'ruby-mode)
  (space-macs/enable-flycheck 'enh-ruby-mode))

(defun ruby/post-init-ggtags ()
  (space-macs/add-to-hooks 'space-macs/ggtags-mode-enable
                          '(ruby-mode-local-vars-hook
                            enh-ruby-mode-local-vars-hook)))

(defun ruby/post-init-helm-gtags ()
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (space-macs/helm-gtags-define-keys-for-mode mode)))

(defun ruby/init-minitest ()
  (use-package minitest
    :defer t
    :init
    (progn
      (space-macs/add-to-hooks 'space-macs//ruby-enable-minitest-mode
                              '(ruby-mode-local-vars-hook
                                enh-ruby-mode-local-vars-hook))
      ;; remove hooks added by minitest mode
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (remove-hook hook 'minitest-enable-appropriate-mode)))
    :config
    (progn
      (space-macs|hide-lighter minitest-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (space-macs/set-leader-keys-for-major-mode mode
          "ta" 'minitest-verify-all
          "tb" 'minitest-verify
          "tr" 'minitest-rerun
          "ts" 'minitest-verify-single)))))

(defun ruby/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(ruby . t))))

(defun ruby/pre-init-popwin ()
  (space-macs|use-package-add-hook popwin
    :post-config
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
          popwin:special-display-config)))

(defun ruby/init-rake ()
  (use-package rake
    :defer t
    :init (setq rake-cache-file (concat space-macs-cache-directory "rake.cache"))
    :config (dolist (mode '(ruby-mode enh-ruby-mode))
              (space-macs/declare-prefix-for-mode mode "mk" "rake")
              (space-macs/set-leader-keys-for-major-mode mode
                "kk"    'rake
                "kr"    'rake-rerun
                "kR"    'rake-regenerate-cache
                "kf"    'rake-find-task))))

(defun ruby/init-rbenv ()
  (use-package rbenv
    :if (equal ruby-version-manager 'rbenv)
    :defer t))

(defun ruby/init-robe ()
  (use-package robe
    :if (eq ruby-backend 'robe)
    :defer t
    :init
    (progn
      (space-macs/register-repl 'robe 'robe-start "robe")
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (add-hook hook 'robe-mode))
      (space-macs/add-to-hooks 'robe-jump
                              '(space-macs-jump-handlers-ruby-mode
                                space-macs-jump-handlers-enh-ruby-mode)))
    :config
    (progn
      (space-macs|hide-lighter robe-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (space-macs/declare-prefix-for-mode mode "mg" "goto")
        (space-macs/declare-prefix-for-mode mode "mh" "docs")
        (space-macs/declare-prefix-for-mode mode "mr" "refactor/robe")
        (space-macs/declare-prefix-for-mode mode "mrs" "robe")
        (space-macs/declare-prefix-for-mode mode "ms" "repl")
        (space-macs/set-leader-keys-for-major-mode mode
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
      (space-macs/add-to-hooks 'space-macs//ruby-enable-rspec-mode
                              '(ruby-mode-local-vars-hook
                                enh-ruby-mode-local-vars-hook))
      ;; remove hooks automatically added by rspec via autoload
      ;; because we want to be able to control when rspec-mode is
      ;; loaded based on the layer variable `ruby-test-runner'
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (remove-hook hook 'rspec-enable-appropriate-mode)))
    :config
    (progn
      (add-hook 'rspec-compilation-mode-hook 'space-macs//inf-ruby-auto-enter)
      (space-macs|hide-lighter rspec-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (space-macs/set-leader-keys-for-major-mode mode
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
    :init (space-macs/add-to-hooks 'rubocop-mode '(ruby-mode-hook
                                                  enh-ruby-mode-hook))
    :config (dolist (mode '(ruby-mode enh-ruby-mode))
              (space-macs/declare-prefix-for-mode mode "mR" "RuboCop")
              (space-macs/set-leader-keys-for-major-mode mode
                "Rd" 'rubocop-check-directory
                "RD" 'rubocop-autocorrect-directory
                "Rf" 'rubocop-check-current-file
                "Rp" 'rubocop-check-project
                "RP" 'rubocop-autocorrect-project))))

(defun ruby/init-rubocopfmt ()
  (use-package rubocopfmt
    :defer t
    :init
    (progn
      (setq-default rubocopfmt-disabled-cops '())

      (dolist (mode '(ruby-mode enh-ruby-mode))
        (space-macs/declare-prefix-for-mode mode "m=" "format")
        (space-macs/set-leader-keys-for-major-mode mode
          "=r" #'rubocopfmt)))))

(defun ruby/init-ruby-hash-syntax ()
  (use-package ruby-hash-syntax
    :defer t
    :init
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (space-macs/set-leader-keys-for-major-mode mode
        "xh" 'ruby-hash-syntax-toggle))))

(defun ruby/init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :mode (("Appraisals\\'" . ruby-mode)
            ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . ruby-mode)
            ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\|pryrc\\)\\'" . ruby-mode))
    :init
    (progn
      (space-macs/declare-prefix-for-mode 'ruby-mode "mi" "insert")
      (space-macs/declare-prefix-for-mode 'ruby-mode "mt" "test")
      (space-macs/declare-prefix-for-mode 'ruby-mode "mT" "toggle")

      ;; setup version manager which is necessary to find gems in path for backend
      (space-macs//ruby-setup-version-manager)
      (add-hook 'ruby-mode-hook #'space-macs//ruby-setup-backend)
      (add-hook 'ruby-mode-local-vars-hook
                #'space-macs/ruby-maybe-highlight-debugger-keywords))
    :config
    (progn
      ;; This might have been important 10 years ago but now it's frustrating.
      (setq ruby-insert-encoding-magic-comment nil)

      (space-macs/set-leader-keys-for-major-mode 'ruby-mode
        "if"  'space-macs/ruby-insert-frozen-string-literal-comment
        "is"  'space-macs/ruby-insert-shebang
        "r'"  'ruby-toggle-string-quotes
        "r\"" 'ruby-toggle-string-quotes
        "r{"  'ruby-toggle-block
        "r}"  'ruby-toggle-block))))

(defun ruby/init-ruby-refactor ()
  (use-package ruby-refactor
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-refactor-mode-launch))
    :config
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (space-macs/declare-prefix-for-mode mode "mre" "extract")
      (space-macs/set-leader-keys-for-major-mode mode
        "rem" 'ruby-refactor-extract-to-method
        "rev" 'ruby-refactor-extract-local-variable
        "rec" 'ruby-refactor-extract-constant
        "rel" 'ruby-refactor-extract-to-let))))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode
    :defer t
    :init (space-macs/add-to-hooks 'space-macs//ruby-enable-ruby-test-mode
                                  '(ruby-mode-local-vars-hook
                                    enh-ruby-mode-local-vars-hook))
    :config
    (progn
      ;; `ruby-test-mode' adds a hook to enable itself, this hack
      ;; removes it to be sure that we control the loading of the
      ;; mode
      (remove-hook 'ruby-mode-hook 'ruby-test-enable)
      (space-macs|hide-lighter ruby-test-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (space-macs/set-leader-keys-for-major-mode mode
          "tb" 'ruby-test-run
          "tt" 'ruby-test-run-at-point)))))

(defun ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-tools-mode))
    :config
    (progn
      (space-macs|hide-lighter ruby-tools-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (space-macs/declare-prefix-for-mode mode "mx" "text")
        (space-macs/set-leader-keys-for-major-mode mode
          "x\'" 'ruby-tools-to-single-quote-string
          "x\"" 'ruby-tools-to-double-quote-string
          "x:" 'ruby-tools-to-symbol)))))

(defun ruby/init-rvm ()
  (use-package rvm
    :if (equal ruby-version-manager 'rvm)
    :defer t
    :init
    (progn
      (setq rspec-use-rvm t)
      (space-macs/add-to-hooks 'rvm-activate-corresponding-ruby
                              '(ruby-mode-hook enh-ruby-mode-hook)))))

(defun ruby/init-seeing-is-believing ()
  (use-package seeing-is-believing
    :defer t
    :commands (seeing-is-believing seeing-is-believing-run seeing-is-believing-clear)
    :if (executable-find "seeing_is_believing")
    :init
    (progn
      (space-macs|diminish seeing-is-believing " ðŸ‘" " @")
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (add-hook hook 'seeing-is-believing))
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (space-macs/declare-prefix-for-mode mode "m@" "seeing-is-believing")
        (space-macs/set-leader-keys-for-major-mode mode
          "@@" 'seeing-is-believing-run
          "@c" 'seeing-is-believing-clear)))))

(defun ruby/pre-init-smartparens ()
  (space-macs|use-package-add-hook smartparens
    :post-config
    (sp-with-modes (if ruby-enable-enh-ruby-mode 'enh-ruby-mode 'ruby-mode)
      (sp-local-pair
       "{" "}"
       :pre-handlers '(sp-ruby-pre-handler)
       :post-handlers '(sp-ruby-post-handler
                        (space-macs/smartparens-pair-newline-and-indent "RET"))
       :suffix ""))))


