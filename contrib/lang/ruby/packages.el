(setq ruby-packages
  '(
    bundler
    company
    enh-ruby-mode
    flycheck
    robe
    ruby-test-mode
    ruby-tools
    yaml-mode))

(when ruby-version-manager
  (add-to-list 'ruby-packages ruby-version-manager))

(when ruby-enable-ruby-on-rails-support
  (add-to-list 'ruby-packages 'haml-mode)
  (add-to-list 'ruby-packages 'feature-mode)
  (add-to-list 'ruby-packages 'projectile-rails))

(defun ruby/init-rbenv ()
  "Initialize RBENV mode"
  (use-package rbenv
    :defer t
    :init (global-rbenv-mode)
    :config (add-hook 'enh-ruby-mode-hook
                      (lambda () (rbenv-use-corresponding)))))

(defun ruby/init-rvm ()
  "Initialize RVM mode"
  (use-package rvm
    :defer t
    :init (rvm-use-default)
    :config (add-hook 'enh-ruby-mode-hook
                      (lambda () (rvm-activate-corresponding-ruby)))))

(defun ruby/init-enh-ruby-mode ()
  "Initialize Ruby Mode"
  (use-package enh-ruby-mode
    :mode (("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))))

(defun ruby/post-init-flycheck ()
  (add-hook 'enh-ruby-mode-hook 'flycheck-mode))

(defun ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init
    (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
    :config
    (spacemacs|hide-lighter ruby-tools-mode)))

(defun ruby/init-bundler ()
  (use-package bundler
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbc" 'bundle-check)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbi" 'bundle-install)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbs" 'bundle-console)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbu" 'bundle-update)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mbx" 'bundle-exec))))

(defun ruby/init-projectile-rails ()
  (use-package projectile-rails
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'projectile-rails-on))
    :config
    (progn
      (spacemacs|diminish projectile-rails-mode " ⇋" " RoR")

      ;; Find files
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfa" 'projectile-rails-find-locale)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfc" 'projectile-rails-find-controller)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfe" 'projectile-rails-find-environment)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrff" 'projectile-rails-find-feature)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfh" 'projectile-rails-find-helper)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfi" 'projectile-rails-find-initializer)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfj" 'projectile-rails-find-javascript)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfl" 'projectile-rails-find-lib)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfm" 'projectile-rails-find-model)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfn" 'projectile-rails-find-migration)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfo" 'projectile-rails-find-log)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfp" 'projectile-rails-find-spec)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfr" 'projectile-rails-find-rake-task)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfs" 'projectile-rails-find-stylesheet)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrft" 'projectile-rails-find-test)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfu" 'projectile-rails-find-fixture)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfv" 'projectile-rails-find-view)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrfy" 'projectile-rails-find-layout)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrf@" 'projectile-rails-find-mailer)
      ;; Goto file
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgc" 'projectile-rails-find-current-controller)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgd" 'projectile-rails-goto-schema)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrge" 'projectile-rails-goto-seeds)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgh" 'projectile-rails-find-current-helper)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgj" 'projectile-rails-find-current-javascript)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgg" 'projectile-rails-goto-gemfile)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgm" 'projectile-rails-find-current-model)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgn" 'projectile-rails-find-current-migration)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgp" 'projectile-rails-find-current-spec)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgr" 'projectile-rails-goto-routes)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgs" 'projectile-rails-find-current-stylesheet)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgt" 'projectile-rails-find-current-test)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgu" 'projectile-rails-find-current-fixture)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgv" 'projectile-rails-find-current-view)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrgz" 'projectile-rails-goto-spec-helper)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrg." 'projectile-rails-goto-file-at-point)
      ;; Rails external commands
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrcc" 'projectile-rails-generate)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mri" 'projectile-rails-console)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrr:" 'projectile-rails-rake)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrxs" 'projectile-rails-server)
      ;; Refactoring 'projectile-rails-mode
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrRx" 'projectile-rails-extract-region)
      ;; Ex-commands
      (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test))))

(defun ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'robe-mode)
      (when (configuration-layer/layer-usedp 'auto-completion)
        (push 'company-robe company-backends-enh-ruby-mode)))
    :config
    (progn
      (spacemacs|hide-lighter robe-mode)
      ;; robe mode specific
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mgg" 'robe-jump)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mhd" 'robe-doc)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mrsr" 'robe-rails-refresh)
      ;; inf-enh-ruby-mode
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msf" 'ruby-send-definition)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msF" 'ruby-send-definition-and-go)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msi" 'robe-start)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msr" 'ruby-send-region)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "msR" 'ruby-send-region-and-go)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mss" 'ruby-switch-to-inf))))

(defun ruby/init-yaml-mode ()
  "Initialize YAML mode"
  (use-package yaml-mode
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode))
    :config (add-hook 'yaml-mode-hook
                      '(lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

(defun ruby/init-feature-mode ()
  "Initialize Cucumber feature mode"
  (use-package feature-mode
    :mode (("\\.feature\\'" . feature-mode))))

(defun ruby/init-haml-mode ()
  (use-package haml-mode
    :defer t))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode
    :defer t
    :init (add-hook 'enh-ruby-mode-hook 'ruby-test-mode)
    :config
    (progn
      (spacemacs|hide-lighter ruby-test-mode)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtb" 'ruby-test-run)
      (evil-leader/set-key-for-mode 'enh-ruby-mode "mtt" 'ruby-test-run-at-point))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun ruby/post-init-company ()
    (spacemacs|add-company-hook enh-ruby-mode)))
