;;; packages.el --- Ruby Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst ruby-packages
  '(
    add-node-modules-path
    bundler
    chruby
    company
    counsel-gtags
    dap-mode
    (enh-ruby-mode :toggle ruby-enable-enh-ruby-mode)
    evil-matchit
    flycheck
    ggtags
    minitest
    org
    popwin
    prettier-js
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
            (spacemacs/declare-prefix-for-mode mode "mb"
              (if (eq ruby-backend 'lsp) "build/bundle" "bundle"))
            (spacemacs/set-leader-keys-for-major-mode mode
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
    :init (spacemacs/add-to-hooks 'chruby-use-corresponding
                                  '(ruby-mode-hook enh-ruby-mode-hook))))

(defun ruby/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(ruby-mode-hook)))

(defun ruby/post-init-company ()
  (add-hook 'ruby-mode-local-vars-hook #'spacemacs//ruby-setup-company))

(defun ruby/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'ruby-mode)
  (spacemacs/counsel-gtags-define-keys-for-mode 'enh-ruby-mode))

(defun ruby/pre-init-dap-mode ()
  (when (eq ruby-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'ruby-mode)
    (add-to-list 'spacemacs--dap-supported-modes 'enh-ruby-mode))
  (spacemacs/add-to-hooks #'spacemacs//ruby-setup-dap
                          '(ruby-mode-local-vars-hook
                            enh-ruby-mode-local-vars-hook)))

(defun ruby/init-enh-ruby-mode ()
  (use-package enh-ruby-mode
    :mode (("Appraisals\\'" . enh-ruby-mode)
           ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\|pryrc\\)\\'" . enh-ruby-mode))
    :interpreter "ruby"
    :init
    (setq enh-ruby-deep-indent-paren nil
          enh-ruby-hanging-paren-deep-indent-level 2)
    (spacemacs/declare-prefix-for-mode 'enh-ruby-mode "mi" "insert")
    (spacemacs/declare-prefix-for-mode 'enh-ruby-mode "mt" "test")

    (add-hook 'enh-ruby-mode-hook #'spacemacs//ruby-setup-backend)
    (add-hook 'enh-ruby-mode-local-vars-hook
              #'spacemacs/ruby-maybe-highlight-debugger-keywords)
    :config
    (spacemacs/set-leader-keys-for-major-mode 'enh-ruby-mode
      "if"  'spacemacs/ruby-insert-frozen-string-literal-comment
      "is"  'spacemacs/ruby-insert-shebang
      "r{" 'enh-ruby-toggle-block
      "r}" 'enh-ruby-toggle-block)))

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

(defun ruby/init-minitest ()
  (use-package minitest
    :defer t
    :init
    (spacemacs/add-to-hooks 'spacemacs//ruby-enable-minitest-mode
                            '(ruby-mode-local-vars-hook
                              enh-ruby-mode-local-vars-hook))
    ;; remove hooks added by minitest mode
    (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
      (remove-hook hook 'minitest-enable-appropriate-mode))
    :config
    (spacemacs|hide-lighter minitest-mode)
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "ta" 'minitest-verify-all
        "tb" 'minitest-verify
        "tr" 'minitest-rerun
        "ts" 'minitest-verify-single))))

(defun ruby/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(ruby . t))))

(defun ruby/pre-init-prettier-js ()
  (add-to-list 'spacemacs--prettier-modes 'ruby-mode)
  (add-to-list 'spacemacs--prettier-modes 'enh-ruby-mode))

(defun ruby/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
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
    :init (setq rake-cache-file (concat spacemacs-cache-directory "rake.cache"))
    :config (dolist (mode '(ruby-mode enh-ruby-mode))
              (spacemacs/declare-prefix-for-mode mode "mk" "rake")
              (spacemacs/set-leader-keys-for-major-mode mode
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
    (spacemacs/register-repl 'robe 'robe-start "robe")
    (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
      (add-hook hook 'robe-mode))
    (spacemacs/add-to-hooks 'robe-jump
                            '(spacemacs-jump-handlers-ruby-mode
                              spacemacs-jump-handlers-enh-ruby-mode))
    :config
    (spacemacs|hide-lighter robe-mode)
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/declare-prefix-for-mode mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode mode "mh" "docs")
      (spacemacs/declare-prefix-for-mode mode "mr" "refactor/robe")
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
        "ss" 'ruby-switch-to-inf))))

(defun ruby/init-rspec-mode ()
  (use-package rspec-mode
    :defer t
    :init
    (spacemacs/add-to-hooks 'spacemacs//ruby-enable-rspec-mode
                            '(ruby-mode-local-vars-hook
                              enh-ruby-mode-local-vars-hook))
    ;; remove hooks automatically added by rspec via autoload
    ;; because we want to be able to control when rspec-mode is
    ;; loaded based on the layer variable `ruby-test-runner'
    (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
      (remove-hook hook 'rspec-enable-appropriate-mode))
    :config
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
        "t TAB" 'rspec-toggle-spec-and-target))))

(defun ruby/init-rubocop ()
  (use-package rubocop
    :defer t
    :init (spacemacs/add-to-hooks 'rubocop-mode '(ruby-mode-hook
                                                  enh-ruby-mode-hook))
    :config (dolist (mode '(ruby-mode enh-ruby-mode))
              (spacemacs/declare-prefix-for-mode mode "mR" "RuboCop")
              (spacemacs/set-leader-keys-for-major-mode mode
                "Rd" 'rubocop-check-directory
                "RD" 'rubocop-autocorrect-directory
                "Rf" 'rubocop-check-current-file
                "RF" 'rubocop-autocorrect-current-file
                "Rp" 'rubocop-check-project
                "RP" 'rubocop-autocorrect-project))))

(defun ruby/init-rubocopfmt ()
  (use-package rubocopfmt
    :defer t
    :init
    (setq-default rubocopfmt-disabled-cops '())

    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/declare-prefix-for-mode mode "m=" "format")
      (spacemacs/set-leader-keys-for-major-mode mode
        "=r" #'rubocopfmt))))

(defun ruby/init-ruby-hash-syntax ()
  (use-package ruby-hash-syntax
    :defer t
    :init
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "xh" 'ruby-hash-syntax-toggle))))

(defun ruby/init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :mode (("Appraisals\\'" . ruby-mode)
           ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\|pryrc\\)\\'" . ruby-mode))
    :init
    (spacemacs/declare-prefix-for-mode 'ruby-mode "mi" "insert")
    (spacemacs/declare-prefix-for-mode 'ruby-mode "mt" "test")
    (spacemacs/declare-prefix-for-mode 'ruby-mode "mT" "toggle")

    ;; setup version manager which is necessary to find gems in path for backend
    (spacemacs//ruby-setup-version-manager)
    (add-hook 'ruby-mode-hook #'spacemacs//ruby-setup-backend)
    (add-hook 'ruby-mode-local-vars-hook
              #'spacemacs/ruby-maybe-highlight-debugger-keywords)
    :config
    ;; This might have been important 10 years ago but now it's frustrating.
    (setq ruby-insert-encoding-magic-comment nil)

    (when ruby-prettier-on-save
      (add-hook 'ruby-mode-hook 'spacemacs/ruby-fmt-before-save-hook))
    (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
      "if"  'spacemacs/ruby-insert-frozen-string-literal-comment
      "is"  'spacemacs/ruby-insert-shebang
      "r'"  'ruby-toggle-string-quotes
      "r\"" 'ruby-toggle-string-quotes
      "r{"  'ruby-toggle-block
      "r}"  'ruby-toggle-block)))

(defun ruby/init-ruby-refactor ()
  (use-package ruby-refactor
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-refactor-mode-launch))
    :config
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/declare-prefix-for-mode mode "mre" "extract")
      (spacemacs/set-leader-keys-for-major-mode mode
        "rem" 'ruby-refactor-extract-to-method
        "rev" 'ruby-refactor-extract-local-variable
        "rec" 'ruby-refactor-extract-constant
        "rel" 'ruby-refactor-extract-to-let))))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode"
  (use-package ruby-test-mode
    :defer t
    :init (spacemacs/add-to-hooks 'spacemacs//ruby-enable-ruby-test-mode
                                  '(ruby-mode-local-vars-hook
                                    enh-ruby-mode-local-vars-hook))
    :config
    ;; `ruby-test-mode' adds a hook to enable itself, this hack
    ;; removes it to be sure that we control the loading of the
    ;; mode
    (remove-hook 'ruby-mode-hook 'ruby-test-enable)
    (spacemacs|hide-lighter ruby-test-mode)
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "tb" 'ruby-test-run
        "tt" 'ruby-test-run-at-point))))

(defun ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-tools-mode))
    :config
    (spacemacs|hide-lighter ruby-tools-mode)
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/declare-prefix-for-mode mode "mx" "text")
      (spacemacs/set-leader-keys-for-major-mode mode
        "x\'" 'ruby-tools-to-single-quote-string
        "x\"" 'ruby-tools-to-double-quote-string
        "x:" 'ruby-tools-to-symbol))))

(defun ruby/init-rvm ()
  (use-package rvm
    :if (equal ruby-version-manager 'rvm)
    :defer t
    :init
    (setq rspec-use-rvm t)
    (spacemacs/add-to-hooks 'rvm-activate-corresponding-ruby
                            '(ruby-mode-hook enh-ruby-mode-hook))))

(defun ruby/init-seeing-is-believing ()
  (use-package seeing-is-believing
    :defer t
    :commands (seeing-is-believing seeing-is-believing-run seeing-is-believing-clear)
    :if (executable-find "seeing_is_believing")
    :init
    (spacemacs|diminish seeing-is-believing " 👁" " @")
    (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
      (add-hook hook 'seeing-is-believing))
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/declare-prefix-for-mode mode "m@" "seeing-is-believing")
      (spacemacs/set-leader-keys-for-major-mode mode
        "@@" 'seeing-is-believing-run
        "@c" 'seeing-is-believing-clear))))

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
