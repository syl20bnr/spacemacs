(defvar ruby-packages
  '(
    bundler
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
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
    :config
    (progn
      ;; work arround for a bug with wrong number of argument
      ;; https://github.com/zenspider/enhanced-ruby-mode/blob/master/test/enh-ruby-mode-test.el#L4
      (defun erm-darken-color (name)
        (let ((attr (face-attribute name :foreground)))
          (unless (equal attr 'unspecified)
            (color-darken-name attr 20) "#000000"))))))

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
      (evil-leader/set-key "mrfa" 'projectile-rails-find-locale)
      (evil-leader/set-key "mrfc" 'projectile-rails-find-controller)
      (evil-leader/set-key "mrfe" 'projectile-rails-find-environment)
      (evil-leader/set-key "mrff" 'projectile-rails-find-feature)
      (evil-leader/set-key "mrfh" 'projectile-rails-find-helper)
      (evil-leader/set-key "mrfi" 'projectile-rails-find-initializer)
      (evil-leader/set-key "mrfj" 'projectile-rails-find-javascript)
      (evil-leader/set-key "mrfl" 'projectile-rails-find-lib)
      (evil-leader/set-key "mrfm" 'projectile-rails-find-model)
      (evil-leader/set-key "mrfn" 'projectile-rails-find-migration)
      (evil-leader/set-key "mrfo" 'projectile-rails-find-log)
      (evil-leader/set-key "mrfp" 'projectile-rails-find-spec)
      (evil-leader/set-key "mrfr" 'projectile-rails-find-rake-task)
      (evil-leader/set-key "mrfs" 'projectile-rails-find-stylesheet)
      (evil-leader/set-key "mrft" 'projectile-rails-find-test)
      (evil-leader/set-key "mrfu" 'projectile-rails-find-fixture)
      (evil-leader/set-key "mrfv" 'projectile-rails-find-view)
      (evil-leader/set-key "mrfy" 'projectile-rails-find-layout)
      (evil-leader/set-key "mrf@" 'projectile-rails-find-mailer)
      ;; Goto file
      (evil-leader/set-key "mrgc" 'projectile-rails-find-current-controller)
      (evil-leader/set-key "mrgd" 'projectile-rails-goto-schema)
      (evil-leader/set-key "mrge" 'projectile-rails-goto-seeds)
      (evil-leader/set-key "mrgh" 'projectile-rails-find-current-helper)
      (evil-leader/set-key "mrgj" 'projectile-rails-find-current-javascript)
      (evil-leader/set-key "mrgg" 'projectile-rails-goto-gemfile)
      (evil-leader/set-key "mrgm" 'projectile-rails-find-current-model)
      (evil-leader/set-key "mrgn" 'projectile-rails-find-current-migration)
      (evil-leader/set-key "mrgp" 'projectile-rails-find-current-spec)
      (evil-leader/set-key "mrgr" 'projectile-rails-goto-routes)
      (evil-leader/set-key "mrgs" 'projectile-rails-find-current-stylesheet)
      (evil-leader/set-key "mrgt" 'projectile-rails-find-current-test)
      (evil-leader/set-key "mrgu" 'projectile-rails-find-current-fixture)
      (evil-leader/set-key "mrgv" 'projectile-rails-find-current-view)
      (evil-leader/set-key "mrgz" 'projectile-rails-goto-spec-helper)
      (evil-leader/set-key "mrg." 'projectile-rails-goto-file-at-point)
      ;; Rails external commands
      (evil-leader/set-key "mrcc" 'projectile-rails-generate)
      (evil-leader/set-key "mri" 'projectile-rails-console)
      (evil-leader/set-key "mrr:" 'projectile-rails-rake)
      (evil-leader/set-key "mrxs" 'projectile-rails-server)
      ;; Refactoring
      (evil-leader/set-key "mrRx" 'projectile-rails-extract-region))))

(defun ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init
    (progn
      (add-hook 'enh-ruby-mode-hook 'robe-mode)
      (when (configuration-layer/layer-usedp 'auto-completion)
        (push '(company-robe :with company-yasnippet) company-backends-enh-ruby-mode)))
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
    :init (add-hook 'ruby-mode-hook 'ruby-test-mode)
    :config
    (progn
      (spacemacs|hide-lighter ruby-test-mode)
      (evil-leader/set-key
        "mtb" 'ruby-test-run
        "mtt" 'ruby-test-run-at-point))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun ruby/post-init-company ()
    (spacemacs|add-company-hook enh-ruby-mode)))
