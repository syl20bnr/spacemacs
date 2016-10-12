;;; packages.el --- Ruby on Rails Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ruby-on-rails-packages
  '(
    feature-mode
    projectile-rails
    which-key
    ))

(defun ruby-on-rails/init-projectile-rails ()
  (use-package projectile-rails
    :defer t
    :init
    (progn
      (add-hook 'projectile-mode-hook 'projectile-rails-on))
    :config
    (progn
      (spacemacs|diminish projectile-rails-mode " ⇋" " RoR")

      ;; Find files
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/set-leader-keys-for-major-mode mode
          "rfa" 'projectile-rails-find-locale
          "rfc" 'projectile-rails-find-controller
          "rfe" 'projectile-rails-find-environment
          "rff" 'projectile-rails-find-feature
          "rfh" 'projectile-rails-find-helper
          "rfi" 'projectile-rails-find-initializer
          "rfj" 'projectile-rails-find-javascript
          "rfl" 'projectile-rails-find-lib
          "rfm" 'projectile-rails-find-model
          "rfn" 'projectile-rails-find-migration
          "rfo" 'projectile-rails-find-log
          "rfp" 'projectile-rails-find-spec
          "rfr" 'projectile-rails-find-rake-task
          "rfs" 'projectile-rails-find-stylesheet
          "rft" 'projectile-rails-find-test
          "rfu" 'projectile-rails-find-fixture
          "rfv" 'projectile-rails-find-view
          "rfy" 'projectile-rails-find-layout
          "rf@" 'projectile-rails-find-mailer
          ;; Goto file
          "rgc" 'projectile-rails-find-current-controller
          "rgd" 'projectile-rails-goto-schema
          "rge" 'projectile-rails-goto-seeds
          "rgh" 'projectile-rails-find-current-helper
          "rgj" 'projectile-rails-find-current-javascript
          "rgg" 'projectile-rails-goto-gemfile
          "rgm" 'projectile-rails-find-current-model
          "rgn" 'projectile-rails-find-current-migration
          "rgp" 'projectile-rails-find-current-spec
          "rgr" 'projectile-rails-goto-routes
          "rgs" 'projectile-rails-find-current-stylesheet
          "rgt" 'projectile-rails-find-current-test
          "rgu" 'projectile-rails-find-current-fixture
          "rgv" 'projectile-rails-find-current-view
          "rgz" 'projectile-rails-goto-spec-helper
          "rg." 'projectile-rails-goto-file-at-point
          ;; Rails external commands
          "r:" 'projectile-rails-rake
          "rcc" 'projectile-rails-generate
          "ri" 'projectile-rails-console
          "rxs" 'projectile-rails-server
          ;; Refactoring 'projectile-rails-mode
          "rRx" 'projectile-rails-extract-region)
        (spacemacs/declare-prefix-for-mode mode "mr" "rails/rubocop")
        (spacemacs/declare-prefix-for-mode mode "mrf" "file")
        (spacemacs/declare-prefix-for-mode mode "mrg" "goto"))

      ;; Ex-commands
      (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test))))

(defun ruby-on-rails/init-feature-mode ()
  "Initialize Cucumber feature mode"
  (use-package feature-mode
    :mode (("\\.feature\\'" . feature-mode))))

(defun ruby-on-rails/post-init-which-key ()
  (push '("projectile-rails-\\(.+\\)" . "\\1")
        which-key-description-replacement-alist))
