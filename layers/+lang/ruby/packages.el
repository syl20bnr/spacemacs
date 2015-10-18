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
        ruby-tools))

(when ruby-version-manager
  (add-to-list 'ruby-packages ruby-version-manager))

(if ruby-use-built-in-ruby-mode
    (add-to-list 'ruby-packages 'ruby-mode)
  (add-to-list 'ruby-packages 'enh-ruby-mode))

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
    :config (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
              (add-hook hook
                        (lambda () (rvm-activate-corresponding-ruby))))))

(defun ruby/init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :config
    (progn
      (evil-leader/set-key-for-mode 'ruby-mode
        "m'" 'ruby-toggle-string-quotes
        "m{" 'ruby-toggle-block)
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
        (evil-leader/set-key-for-mode mode
          "mx\'" 'ruby-tools-to-single-quote-string
          "mx\"" 'ruby-tools-to-double-quote-string
          "mx:" 'ruby-tools-to-symbol)))))

(defun ruby/init-bundler ()
  (use-package bundler
    :defer t
    :init
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (evil-leader/set-key-for-mode mode
        "mbc" 'bundle-check
        "mbi" 'bundle-install
        "mbs" 'bundle-console
        "mbu" 'bundle-update
        "mbx" 'bundle-exec))))

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
        (evil-leader/set-key-for-mode mode
          ;; robe mode specific
          "mgg" 'robe-jump
          "mhd" 'robe-doc
          "mrsr" 'robe-rails-refresh
          ;; inf-enh-ruby-mode
          "msf" 'ruby-send-definition
          "msF" 'ruby-send-definition-and-go
          "msi" 'robe-start
          "msr" 'ruby-send-region
          "msR" 'ruby-send-region-and-go
          "mss" 'ruby-switch-to-inf)))))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-test-mode))
    :config
    (progn
      (spacemacs|hide-lighter ruby-test-mode)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (evil-leader/set-key-for-mode mode "mtb" 'ruby-test-run)
        (evil-leader/set-key-for-mode mode "mtt" 'ruby-test-run-at-point)))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun ruby/post-init-company ()
    (spacemacs|add-company-hook ruby-mode)
    (spacemacs|add-company-hook enh-ruby-mode)

    (with-eval-after-load 'company-dabbrev-code
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (push mode company-dabbrev-code-modes)))))
