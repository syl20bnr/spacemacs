(defvar ruby-packages
  '(
    ;; package rubys go here
    ruby-mode
    ruby-tools
    flycheck
    ruby-test-mode
    robe
    bundler
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
    :config (add-hook 'ruby-mode-hook
                      (lambda () (rbenv-use-corresponding)))))

(defun ruby/init-rvm ()
  "Initialize RVM mode"
  (use-package rvm
    :defer t
    :init (rvm-use-default)
    :config (add-hook 'ruby-mode-hook
                      (lambda () (rvm-activate-corresponding-ruby)))))

(defun ruby/init-ruby-mode ()
  "Initialize Ruby Mode"
  (use-package ruby-mode
    :defer t
    :mode (("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\|berks\\|pod\\|puppet\\)file\\'" . ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . ruby-mode))))

(defun ruby/init-flycheck ()
  (add-hook 'ruby-mode-hook 'flycheck-mode))

(defun ruby/init-ruby-tools ()
  (add-hook 'ruby-mode-hook 'ruby-tools-mode))

(defun ruby/init-bundler ()
  (use-package bundler
    :defer t
    :init (progn
              (evil-leader/set-key-for-mode 'ruby-mode "mbc" 'bundle-check)
              (evil-leader/set-key-for-mode 'ruby-mode "mbi" 'bundle-install)
              (evil-leader/set-key-for-mode 'ruby-mode "mbs" 'bundle-console)
              (evil-leader/set-key-for-mode 'ruby-mode "mbu" 'bundle-update)
              (evil-leader/set-key-for-mode 'ruby-mode "mbx" 'bundle-exec))))

(defun ruby/init-projectile-rails ()
  (use-package projectile-rails
    :defer t
    :init (progn
            (add-hook 'projectile-mode-hook 'projectile-rails-on))
    :config (progn
              (spacemacs|diminish projectile-rails-mode " ⇋" " R")
              ; Code navigation
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
    :init (add-hook 'ruby-mode-hook 'robe-mode)
    :config (progn
              (spacemacs|diminish robe-mode " ♦" " r")
              ;; robe mode specific
              (evil-leader/set-key-for-mode 'ruby-mode "mgg" 'robe-jump)
              (evil-leader/set-key-for-mode 'ruby-mode "mhd" 'robe-doc)
              (evil-leader/set-key-for-mode 'ruby-mode "mrsr" 'robe-rails-refresh)
              ;; inf-ruby-mode
              (evil-leader/set-key-for-mode 'ruby-mode "msf" 'ruby-send-definition)
              (evil-leader/set-key-for-mode 'ruby-mode "msF" 'ruby-send-definition-and-go)
              (evil-leader/set-key-for-mode 'ruby-mode "msi" 'robe-start)
              (evil-leader/set-key-for-mode 'ruby-mode "msr" 'ruby-send-region)
              (evil-leader/set-key-for-mode 'ruby-mode "msR" 'ruby-send-region-and-go)
              (evil-leader/set-key-for-mode 'ruby-mode "mss" 'ruby-switch-to-inf))))

(defun ruby/init-yaml-mode ()
  "Initialize YAML mode"
  (use-package yaml-mode
    :defer t
    :config (add-hook 'yaml-mode-hook
                      '(lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode))))

(defun ruby/init-feature-mode ()
  "Initialize Cucumber feature mode"
  (use-package feature-mode
    :defer t
    :mode (("\\.feature\\'" . feature-mode))))


(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode
    :defer t
    :init (add-hook 'ruby-mode-hook 'ruby-test-mode)
    :config (progn (evil-leader/set-key "mtb" 'ruby-test-run)
                   (evil-leader/set-key "mtt" 'ruby-test-run-at-point))))
