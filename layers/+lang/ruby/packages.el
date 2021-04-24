;;; packages.el --- Ruby Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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
    :init (spacemacs|eval-for-enabled-ruby-mode
            (spacemacs|spacebind
             :major
             (mode
              "Bundler key bindings"
              ("b" "backend/bundle"
               ("c" bundle-check "Run bundle check")
               ("i" bundle-install "Run bundle install")
               ("s" bundle-console "Run bundle console")
               ("u" bundle-update "Run bundle update")
               ("x" bundle-exec "Run bundle exec")
               ("o" bundle-open "Run bundle open")))))))

(defun ruby/init-chruby ()
  (use-package chruby
    :defer t
    ;; initialized via `spacemacs//ruby-setup-version-manager'
    ))

(defun ruby/post-init-add-node-modules-path ()
  (spacemacs|eval-for-enabled-ruby-mode
    (add-hook 'hook #'add-node-modules-path)))

(defun ruby/post-init-company ()
  (spacemacs|eval-for-enabled-ruby-mode
    (add-hook 'local-vars-hook
              #'spacemacs//ruby-setup-company)))

(defun ruby/post-init-counsel-gtags ()
  (spacemacs|eval-for-enabled-ruby-mode
    (spacemacs/counsel-gtags-define-keys-for-mode 'mode)))

(defun ruby/pre-init-dap-mode ()
  (spacemacs|eval-for-enabled-ruby-mode
    (add-hook 'hook #'spacemacs//ruby-setup-dap)))

(defun ruby/init-enh-ruby-mode ()
  (use-package enh-ruby-mode
    :mode (("Appraisals\\'" . enh-ruby-mode)
           ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\|pryrc\\)\\'" . enh-ruby-mode))
    :interpreter "ruby"
    :hook ((enh-ruby-mode-local-vars . spacemacs//ruby-setup-backend)
           (enh-ruby-mode-local-vars . spacemacs//ruby-setup-test-runner)
           (enh-ruby-mode-local-vars . spacemacs//ruby-setup-version-manager)
           (enh-ruby-mode . spacemacs/ruby-maybe-highlight-debugger-keywords))
    :init
    (setq enh-ruby-deep-indent-paren nil
          enh-ruby-hanging-paren-deep-indent-level 2)
    :config
    (spacemacs|spacebind
     :major
     (enh-ruby-mode
      "Enhanced Ruby Mode key bindings"
      ("i" "Insert"
       ("f" spacemacs/ruby-insert-frozen-string-literal-comment "Insert frozen_string_literal")
       ("s" spacemacs/ruby-insert-shebang "Insert shebang"))
      ("r" "Refactor"
       ("{" enh-ruby-toggle-block "Toggle do-end/braces")
       ("}" enh-ruby-toggle-block "Toggle do-end/braces"))))))

(defun ruby/post-init-evil-matchit ()
  (spacemacs|eval-for-enabled-ruby-mode
    (add-hook 'hook `turn-on-evil-matchit-mode)))

(defun ruby/post-init-flycheck ()
  (spacemacs|eval-for-enabled-ruby-mode
    (spacemacs/enable-flycheck 'mode)))

(defun ruby/post-init-ggtags ()
  (spacemacs|eval-for-enabled-ruby-mode
    (add-hook 'local-vars-hook 'spacemacs/ggtags-mode-enable)))

(defun ruby/init-minitest ()
  (use-package minitest
    :defer t
    ;; initialized via `spacemacs//ruby-setup-version-manager'
    :init (spacemacs|eval-for-enabled-ruby-mode
            ;; remove hooks automatically added by minitest via autoload
            ;; because we want to be able to control when `minitest-mode' is
            ;; loaded based on the layer variable `ruby-test-runner'
            (remove-hook 'hook 'minitest-enable-appropriate-mode))
    :config
    (spacemacs|hide-lighter minitest-mode)
    (spacemacs|eval-for-enabled-ruby-mode
     (spacemacs|spacebind
      :major
      (mode
       "Ruby minitest key bindings"
       ("t" "Tests"
        ("a" minitest-verify-all "Run all tests")
        ("b" minitest-verify "Run tests in buffer")
        ("l" minitest-rerun "Run the last test command")
        ("t" minitest-verify-single "Run test around point")))))))

(defun ruby/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(ruby . t))))

(defun ruby/pre-init-prettier-js ()
  (spacemacs|eval-for-enabled-ruby-mode
    (add-to-list 'spacemacs--prettier-modes 'mode))
  (when ruby-prettier-on-save
    (spacemacs|eval-for-enabled-ruby-mode
      (add-hook 'hook 'spacemacs//ruby-add-prettier-js-before-save-hook))))

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
    :config (spacemacs|eval-for-enabled-ruby-mode
              (spacemacs|spacebind
               :major
               (mode
                "Rake key bindings"
                ("k" "Rake"
                 ("f" rake-find-task "Find rake task")
                 ("k" rake "Run rake")
                 ("r" rake-rerun "Execute last rake command")
                 ("R" rake-regenerate-cache "Regenerates cache")))))))

(defun ruby/init-rbenv ()
  (use-package rbenv
    :defer t
    ;; initialized via `spacemacs//ruby-setup-version-manager'
    ))

(defun ruby/init-robe ()
  (use-package robe
    :defer t
    ;; initialize via `spacemacs//ruby-setup-backend'
    :config
    (spacemacs|hide-lighter robe-mode)
    (spacemacs|eval-for-enabled-ruby-mode
     (spacemacs|spacebind
      :major
      (mode
       "Robe key bindings"
       ("'" robe-start "Start Robe server")
       ("h" "Help/Docs"
        ("h" robe-doc "Method docstring"))
       ("r" "Refactor/Robe"
        ("s" "Robe"
         ("r" robe-rails-refresh "Refresh rails")))
       ("s" "REPL"
        ("b" ruby-send-buffer "Send buffer")
        ("B" ruby-send-buffer-and-go "Send buffer and focus")
        ("f" ruby-send-definition "Send function")
        ("F" ruby-send-definition-and-go "Send function and focus")
        ("i" robe-start "Start")
        ("l" ruby-send-line "Send line")
        ("L" ruby-send-line-and-go "Send line and focus")
        ("r" ruby-send-region "Send region")
        ("R" ruby-send-region-and-go "Send region and focus")
        ("s" ruby-switch-to-inf "Switch to REPL")))))))

(defun ruby/init-rspec-mode ()
  (use-package rspec-mode
    :defer t
    ;; initialized via `spacemacs//ruby-setup-version-manager'
    :init (spacemacs|eval-for-enabled-ruby-mode
            ;; remove hooks automatically added by rspec via autoload
            ;; because we want to be able to control when `rspec-mode' is
            ;; loaded based on the layer variable `ruby-test-runner'
            (remove-hook 'hook 'rspec-enable-appropriate-mode))
    :config
    (spacemacs|hide-lighter rspec-mode)
    (spacemacs|eval-for-enabled-ruby-mode
     (spacemacs|spacebind
      :major
      (mode
       "Ruby rspec key bindings"
       ("t" "Tests"
        ("a" rspec-verify-all "Run spec rake task")
        ("b" rspec-verify "Run spec of buffer")
        ("c" rspec-verify-continue "Run spec files")
        ("d" spacemacs/rspec-verify-directory "Run spec in directory...")
        ("e" rspec-toggle-example-pendingness "Toogle active/pending examples")
        ("f" rspec-verify-method "Run method spec")
        ("l" rspec-run-last-failed "Run last failed spec")
        ("m" rspec-verify-matching "Run spec matching with buffer")
        ("r" rspec-rerun "Execute last rspec command")
        ("t" rspec-verify-single "Run spec")
        ("~" rspec-toggle-spec-and-target-find-example "Switch to method examples")
        (" TAB" rspec-toggle-spec-and-target "Switch to spec for buffer")))))))

(defun ruby/init-rubocop ()
  (use-package rubocop
    :defer t
    :init (spacemacs|eval-for-enabled-ruby-mode
            (add-hook 'hook 'rubocop-mode))
    :config
    (progn
      (spacemacs|hide-lighter rubocop-mode)
      (spacemacs|eval-for-enabled-ruby-mode
        (spacemacs|spacebind
         :major
         (mode
          "Rubucop key bindings"
          ("R" "Rubucop"
           ("b" rubocop-check-current-file "Run check in buffer")
           ("B" rubocop-autocorrect-current-file "Run autocorrect in buffer")
           ("d" rubocop-check-directory "Run check on directory...")
           ("D" rubocop-autocorrect-directory "Run autocorrect on directory...")
           ("p" rubocop-check-project "Run check on project")
           ("P" rubocop-autocorrect-project "Run autocorrect on project"))))))))

(defun ruby/init-rubocopfmt ()
  (use-package rubocopfmt
    :defer t
    :init
    (setq-default rubocopfmt-disabled-cops '())
    (spacemacs|eval-for-enabled-ruby-mode
     (add-hook 'hook 'rubocopfmt-mode))
    :config
    (spacemacs|hide-lighter rubocopfmt-mode)
    (spacemacs|eval-for-enabled-ruby-mode
     (spacemacs|spacebind
      :major
      (mode
       "Rubucop key bindings"
       ("=" "Format"
        ("r" rubocopfmt "Format buffer with Rubucop")))))))

(defun ruby/init-ruby-hash-syntax ()
  (use-package ruby-hash-syntax
    :defer t
    :init
    (spacemacs|eval-for-enabled-ruby-mode
      (spacemacs|spacebind
       :major
       (mode
        "Ruby key bindings"
        ("x" "Text"
         ("h" ruby-hash-syntax-toggle "Toggle 1.8/1.9 hash literal syntax")))))))

(defun ruby/init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :mode (("Appraisals\\'" . ruby-mode)
           ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\|pryrc\\)\\'" . ruby-mode))
    :hook ((ruby-mode-local-vars . spacemacs//ruby-setup-backend)
           (ruby-mode-local-vars . spacemacs//ruby-setup-test-runner)
           (ruby-mode-local-vars . spacemacs//ruby-setup-version-manager)
           (ruby-mode . spacemacs/ruby-maybe-highlight-debugger-keywords))
    :init
    ;; This might have been important 10 years ago but now it's frustrating.
    (setq ruby-insert-encoding-magic-comment nil)
    :config
    (spacemacs|spacebind
     :major
     (ruby-mode
      "Ruby Mode key bindings"
      ("i" "Insert"
       ("f" spacemacs/ruby-insert-frozen-string-literal-comment "Insert frozen_string_literal")
       ("s" spacemacs/ruby-insert-shebang "Insert shebang"))
      ("r" "Refactor"
       ("'" ruby-toggle-string-quotes "Toggle single/double quotes")
       ("\"" ruby-toggle-string-quotes "Toggle single/double quotes")
       ("{" ruby-toggle-block "Toggle do-end/braces")
       ("}" ruby-toggle-block "Toggle do-end/braces"))))))

(defun ruby/init-ruby-refactor ()
  (use-package ruby-refactor
    :defer t
    :init (spacemacs|eval-for-enabled-ruby-mode
            (add-hook 'hook 'ruby-refactor-mode-launch))
    :config
    (progn
      (spacemacs|hide-lighter ruby-refactor-mode)
      (spacemacs|eval-for-enabled-ruby-mode
        (spacemacs|spacebind
         :major
         (mode
          "Ruby key bindings"
          ("r" "Refactor"
           ("e" "Extract"
            ("m" ruby-refactor-extract-to-method "Extract region to method")
            ("v" ruby-refactor-extract-local-variable "Extract selected text to local variable")
            ("c" ruby-refactor-extract-constant "Extract selected text to constant")
            ("l" ruby-refactor-extract-to-let "Convert line to 'let'")))))))))

(defun ruby/init-ruby-test-mode ()
  "Define keybindings for ruby test mode."
  (use-package ruby-test-mode
    :defer t
    ;; initialized via `spacemacs//ruby-setup-version-manager'
    :config
    ;; `ruby-test-mode' adds a hook to enable itself, this hack
    ;; removes it to be sure that we control the loading of the
    ;; mode
    (remove-hook 'ruby-mode-hook 'ruby-test-enable)
    (spacemacs|hide-lighter ruby-test-mode)
    (spacemacs|eval-for-enabled-ruby-mode
     (spacemacs|spacebind
      :major
      (mode
       "Ruby-test key bindings"
       ("t" "Tests"
        ("b" ruby-test-run "Run tests in buffer")
        ("t" ruby-test-run-at-point "Run test around point")))))))

(defun ruby/init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init (spacemacs|eval-for-enabled-ruby-mode
            (add-hook 'hook 'ruby-tools-mode))
    :config
    (spacemacs|hide-lighter ruby-tools-mode)
    (spacemacs|eval-for-enabled-ruby-mode
     (spacemacs|spacebind
      :major
      (mode
       "Ruby key bindings"
       ("x" "Text"
        ("\'" ruby-tools-to-single-quote-string "Replace with single quotes")
        ("\"" ruby-tools-to-double-quote-string "Replace with double quotes")
        (":" ruby-tools-to-symbol "Convert string to symbol")))))))

(defun ruby/init-rvm ()
  (use-package rvm
    :defer t
    ;; initialized via `spacemacs//ruby-setup-version-manager'
    ))

(defun ruby/init-seeing-is-believing ()
  (use-package seeing-is-believing
    :if (executable-find "seeing_is_believing")
    :defer t
    :commands (seeing-is-believing
               seeing-is-believing-run
               seeing-is-believing-clear)
    :init (spacemacs|eval-for-enabled-ruby-mode
            (add-hook 'hook 'seeing-is-believing))
    :config
    (spacemacs|diminish seeing-is-believing " 👀" " @@")
    (spacemacs|eval-for-enabled-ruby-mode
     (spacemacs|spacebind
      :major
      (mode
       "Ruby key bindings"
       ("@" "Seeing is Believing"
        ("@" seeing-is-believing-run "Start")
        ("c" seeing-is-believing-clear "Clear")))))))

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
