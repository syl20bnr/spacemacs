(defvar ruby-packages
  '(
    ;; package rubys go here
    rbenv
    enh-ruby-mode
    ruby-test-mode
    robe
    yaml-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ruby-excluded-packages '(ruby-mode))

(defun ruby/init-rbenv ()
  "Initialize RBENV mode"
  (use-package rbenv
    :init (global-rbenv-mode)
    :defer t
    :config (add-hook 'enh-ruby-mode-hook
          (lambda () (rbenv-use-corresponding)))))

(defun ruby/init-enh-ruby-mode ()
  "Initialize Enhanced Ruby Mode"
  (use-package enh-ruby-mode
    :defer t
    :mode (("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . enh-ruby-mode))))

(defun ruby/init-robe ()
  "Initialize Robe mode"
  (use-package robe
    :defer t
    :init (add-hook 'enh-ruby-mode-hook 'robe-mode)
    :config (progn (evil-leader/set-key-for-mode 'enh-ruby-mode "mg" 'robe-jump)
                   (evil-leader/set-key-for-mode 'enh-ruby-mode "md" 'robe-doc)
                   (evil-leader/set-key-for-mode 'enh-ruby-mode "mR" 'robe-rails-refresh)
                   (evil-leader/set-key-for-mode 'enh-ruby-mode "mi" 'robe-start))))

(defun ruby/init-yaml-mode ()
  "Initialize YAML mode"
  (use-package yaml-mode
    :config (add-hook 'yaml-mode-hook
                      '(lambda ()
                         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
    :defer t
    :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
           ("Procfile\\'" . yaml-mode))))



(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode
    :config (progn (evil-leader/set-key "mtb" 'ruby-test-run)
                   (evil-leader/set-key "mtt" 'ruby-test-run-at-point))))
