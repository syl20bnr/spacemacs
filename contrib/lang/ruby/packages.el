(defvar ruby-packages
  '(
    ;; package rubys go here
    enh-ruby-mode
    flycheck
    ruby-test-mode
    robe
    yaml-mode))

(when ruby-version-manager
  (add-to-list 'ruby-packages ruby-version-manager))

(when ruby-enable-ruby-on-rails-support
  (add-to-list 'ruby-packages 'haml-mode)
  (add-to-list 'ruby-packages 'feature-mode)
  (add-to-list 'ruby-packages 'projectile-rails))

(defvar ruby-excluded-packages '(ruby-mode))

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
  "Initialize Enhanced Ruby Mode"
  (use-package enh-ruby-mode
    :defer t
    :mode (("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . enh-ruby-mode))))

(defun ruby/init-flycheck ()
  (add-hook 'enh-ruby-mode-hook 'flycheck-mode))

(defun ruby/init-projectile-rails ()
  (use-package projectile-rails
    :defer t
    :init (progn
            (add-hook 'projectile-mode-hook 'projectile-rails-on))
    :config (progn
              (spacemacs|diminish projectile-rails-mode " ⇋" " R")
              ;; Code navigation
              (evil-leader/set-key "mgA" 'projectile-rails-find-locale)
              (evil-leader/set-key "mgC" 'projectile-rails-find-controller)
              (evil-leader/set-key "mgc" 'projectile-rails-find-current-controller)
              (evil-leader/set-key "mgd" 'projectile-rails-goto-schema)
              (evil-leader/set-key "mge" 'projectile-rails-goto-seeds)
              (evil-leader/set-key "mgE" 'projectile-rails-find-environment)
              (evil-leader/set-key "mgF" 'projectile-rails-find-feature)
              (evil-leader/set-key "mgH" 'projectile-rails-find-helper)
              (evil-leader/set-key "mgh" 'projectile-rails-find-current-helper)
              (evil-leader/set-key "mgI" 'projectile-rails-find-initializer)
              (evil-leader/set-key "mgJ" 'projectile-rails-find-javascript)
              (evil-leader/set-key "mgj" 'projectile-rails-find-current-javascript)
              (evil-leader/set-key "mgK" 'projectile-rails-find-rake-task)
              (evil-leader/set-key "mgL" 'projectile-rails-find-lib)
              (evil-leader/set-key "mgl" 'projectile-rails-goto-gemfile)
              (evil-leader/set-key "mgM" 'projectile-rails-find-model)
              (evil-leader/set-key "mgm" 'projectile-rails-find-current-model)
              (evil-leader/set-key "mgN" 'projectile-rails-find-migration)
              (evil-leader/set-key "mgn" 'projectile-rails-find-current-migration)
              (evil-leader/set-key "mgO" 'projectile-rails-find-log)
              (evil-leader/set-key "mgP" 'projectile-rails-find-spec)
              (evil-leader/set-key "mgp" 'projectile-rails-find-current-spec)
              (evil-leader/set-key "mgr" 'projectile-rails-goto-routes)
              (evil-leader/set-key "mgS" 'projectile-rails-find-stylesheet)
              (evil-leader/set-key "mgs" 'projectile-rails-find-current-stylesheet)
              (evil-leader/set-key "mgT" 'projectile-rails-find-test)
              (evil-leader/set-key "mgt" 'projectile-rails-find-current-test)
              (evil-leader/set-key "mgU" 'projectile-rails-find-fixture)
              (evil-leader/set-key "mgu" 'projectile-rails-find-current-fixture)
              (evil-leader/set-key "mgV" 'projectile-rails-find-view)
              (evil-leader/set-key "mgv" 'projectile-rails-find-current-view)
              (evil-leader/set-key "mgx" 'projectile-rails-goto-spec-helper)
              (evil-leader/set-key "mgY" 'projectile-rails-find-layout)
              (evil-leader/set-key "mg@" 'projectile-rails-find-mailer)
              (evil-leader/set-key "mg." 'projectile-rails-goto-file-at-point)
              ;; Rails external commands
              (evil-leader/set-key "mRc" 'projectile-rails-console)
              (evil-leader/set-key "mRs" 'projectile-rails-server)
              (evil-leader/set-key "mRk" 'projectile-rails-rake)
              (evil-leader/set-key "mRg" 'projectile-rails-generate)
              ;; Refactoring
              (evil-leader/set-key "mrx" 'projectile-rails-extract-region))))

(defun ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init (add-hook 'enh-ruby-mode-hook 'robe-mode)
    :config (progn
              (spacemacs|diminish robe-mode " ♦" " r")
              ;; robe mode specific
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mgg" 'robe-jump)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mhd" 'robe-doc)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mRR" 'robe-rails-refresh)
              ;; inf-ruby-mode
              (evil-leader/set-key-for-mode 'enh-ruby-mode "msf" 'ruby-send-definition)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "msF" 'ruby-send-definition-and-go)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "msi" 'robe-start)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "msr" 'ruby-send-region)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "msR" 'ruby-send-region-and-go)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mss" 'ruby-switch-to-inf))))

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
    :config (progn (evil-leader/set-key "mtb" 'ruby-test-run)
                   (evil-leader/set-key "mtt" 'ruby-test-run-at-point))))
