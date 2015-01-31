(defvar ruby-packages
  '(
    ;; package rubys go here
    enh-ruby-mode
    flycheck
    ruby-test-mode
    robe
    yaml-mode

    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(when ruby-version-manager
  (add-to-list 'ruby-packages ruby-version-manager))

(when ruby-on-rails-support
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
              ;; GOTO commands
              (evil-leader/set-key "mrf" 'projectile-rails-goto-file-at-point)
              (evil-leader/set-key "mrg" 'projectile-rails-goto-gemfile)
              (evil-leader/set-key "mrr" 'projectile-rails-goto-routes)
              (evil-leader/set-key "mrd" 'projectile-rails-goto-schema)
              (evil-leader/set-key "mrs" 'projectile-rails-goto-seeds)
              (evil-leader/set-key "mrh" 'projectile-rails-goto-spec-helper)
              ;; Find commands
              (evil-leader/set-key "mrm" 'projectile-rails-find-model)
              (evil-leader/set-key "mrM" 'projectile-rails-find-current-model)

              (evil-leader/set-key "mrc" 'projectile-rails-find-controller)
              (evil-leader/set-key "mrC" 'projectile-rails-find-current-controller)

              (evil-leader/set-key "mrv" 'projectile-rails-find-view)
              (evil-leader/set-key "mrV" 'projectile-rails-find-current-view)

              (evil-leader/set-key "mrj" 'projectile-rails-find-javascript)
              (evil-leader/set-key "mrJ" 'projectile-rails-find-current-javascript)

              (evil-leader/set-key "mrs" 'projectile-rails-find-stylesheet)
              (evil-leader/set-key "mrS" 'projectile-rails-find-current-stylesheet)

              (evil-leader/set-key "mrh" 'projectile-rails-find-helper)
              (evil-leader/set-key "mrH" 'projectile-rails-find-current-helper)

              (evil-leader/set-key "mrp" 'projectile-rails-find-spec)
              (evil-leader/set-key "mrP" 'projectile-rails-find-current-spec)

              (evil-leader/set-key "mrt" 'projectile-rails-find-test)
              (evil-leader/set-key "mrT" 'projectile-rails-find-current-test)

              (evil-leader/set-key "mrn" 'projectile-rails-find-migration)
              (evil-leader/set-key "mrN" 'projectile-rails-find-current-migration)

              (evil-leader/set-key "mru" 'projectile-rails-find-fixture)
              (evil-leader/set-key "mrU" 'projectile-rails-find-current-fixture)

              (evil-leader/set-key "mrl" 'projectile-rails-find-lib)
              (evil-leader/set-key "mrf" 'projectile-rails-find-feature)
              (evil-leader/set-key "mri" 'projectile-rails-find-initializer)
              (evil-leader/set-key "mro" 'projectile-rails-find-log)
              (evil-leader/set-key "mre" 'projectile-rails-find-environment)
              (evil-leader/set-key "mra" 'projectile-rails-find-locale)
              (evil-leader/set-key "mr@" 'projectile-rails-find-mailer)
              (evil-leader/set-key "mry" 'projectile-rails-find-layout)
              (evil-leader/set-key "mrk" 'projectile-rails-find-rake-task)

              (evil-leader/set-key "mrx" 'projectile-rails-extract-region)

              ;; RUN commands
              (evil-leader/set-key "mrc" 'projectile-rails-console)
              (evil-leader/set-key "mrs" 'projectile-rails-server)
              (evil-leader/set-key "mrr" 'projectile-rails-rake)
              (evil-leader/set-key "mrg" 'projectile-rails-generate)
   )
  ))

(defun ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init (add-hook 'enh-ruby-mode-hook 'robe-mode)
    :config (progn
              (spacemacs|diminish robe-mode " ♦" " r")
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mgg" 'robe-jump)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mhd" 'robe-doc)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mR" 'robe-rails-refresh)
              (evil-leader/set-key-for-mode 'enh-ruby-mode "mi" 'robe-start))))

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
