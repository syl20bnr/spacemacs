;;; packages.el --- Ruby on Rails Layer packages File for Spacemacs
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


(setq ruby-on-rails-packages
      '(
        feature-mode
        projectile-rails
        which-key
        ))

(defun ruby-on-rails/init-projectile-rails ()
  (use-package projectile-rails
    :config
    (progn
      (projectile-rails-global-mode)
      (spacemacs|diminish projectile-rails-mode " ⇋" " RoR")

      ;; Find files
      (spacemacs/set-leader-keys-for-minor-mode 'projectile-rails-mode
        "ffa" 'projectile-rails-find-locale
        "ffb" 'projectile-rails-find-job
        "ffc" 'projectile-rails-find-controller
        "ffe" 'projectile-rails-find-environment
        "fff" 'projectile-rails-find-feature
        "ffh" 'projectile-rails-find-helper
        "ffi" 'projectile-rails-find-initializer
        "ffj" 'projectile-rails-find-javascript
        "ffl" 'projectile-rails-find-lib
        "ffm" 'projectile-rails-find-model
        "ffn" 'projectile-rails-find-migration
        "ffo" 'projectile-rails-find-log
        "ffp" 'projectile-rails-find-spec
        "ffr" 'projectile-rails-find-rake-task
        "ffs" 'projectile-rails-find-stylesheet
        "fft" 'projectile-rails-find-test
        "ffu" 'projectile-rails-find-fixture
        "ffv" 'projectile-rails-find-view
        "ffw" 'projectile-rails-find-webpack
        "ffy" 'projectile-rails-find-layout
        "ff@" 'projectile-rails-find-mailer
        ;f Goto file
        "fgc" 'projectile-rails-find-current-controller
        "fgd" 'projectile-rails-goto-schema
        "fge" 'projectile-rails-goto-seeds
        "fgh" 'projectile-rails-find-current-helper
        "fgj" 'projectile-rails-find-current-javascript
        "fgg" 'projectile-rails-goto-gemfile
        "fgm" 'projectile-rails-find-current-model
        "fgn" 'projectile-rails-find-current-migration
        "fgp" 'projectile-rails-find-current-spec
        "fgr" 'projectile-rails-goto-routes
        "fgs" 'projectile-rails-find-current-stylesheet
        "fgt" 'projectile-rails-find-current-test
        "fgu" 'projectile-rails-find-current-fixture
        "fgv" 'projectile-rails-find-current-view
        "fgz" 'projectile-rails-goto-spec-helper
        "fg." 'projectile-rails-goto-file-at-point
        ;; Rails external commands
        "f:" 'projectile-rails-rake
        "fcc" 'projectile-rails-generate
        "fcd" 'projectile-rails-destroy
        "fi" 'projectile-rails-console
        "fxs" 'projectile-rails-server
        ;; Refactoring 'projectile-rails-mode
        "fRx" 'projectile-rails-extract-region)

      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mf" "rails")
        (spacemacs/declare-prefix-for-mode mode "mfc" "generate/destroy")
        (spacemacs/declare-prefix-for-mode mode "mfR" "extract")
        (spacemacs/declare-prefix-for-mode mode "mfx" "server")
        (spacemacs/declare-prefix-for-mode mode "mff" "file")
        (spacemacs/declare-prefix-for-mode mode "mfg" "goto"))

      ;; Ex-commands
      (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test))))

(defun ruby-on-rails/init-feature-mode ()
  "Initialize Cucumber feature mode"
  (use-package feature-mode
    :mode (("\\.feature\\'" . feature-mode))))

(defun ruby-on-rails/post-init-which-key ()
  (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist))
