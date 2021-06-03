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
    :defer t
    :init
    (progn
      (spacemacs/add-local-var-hook
       #'spacemacs//ruby-on-rails-setup-projectile-rails
       :project-type 'ruby-on-rails))
    :config
    (spacemacs|diminish projectile-rails-mode " ⇋" " RoR")
    (spacemacs|spacebind
     :project-minor
     (projectile-rails-mode
      "Ruby on Rails bindings"
      ("r" "Rails"
       ("i" projectile-rails-console "Start console")
       ("c" "Compile/Execute"
        ("c" projectile-rails-generate "generate")
        ("d" projectile-rails-destroy "destroy")
        ("r" projectile-rails-rake "Rake task...")
        ("s" projectile-rails-server "server"))
       ("f" "Files"
        ("@" projectile-rails-find-mailer "Find mailer")
        ("a" projectile-rails-find-locale "Find locale file")
        ("b" projectile-rails-find-job "Find job file")
        ("c" projectile-rails-find-controller "Find controller file")
        ("e" projectile-rails-find-environment "Find environment file")
        ("f" projectile-rails-find-feature "Find feature file")
        ("h" projectile-rails-find-helper "Find helper")
        ("i" projectile-rails-find-initializer "Find initializer file")
        ("j" projectile-rails-find-javascript "Find javascript file")
        ("l" projectile-rails-find-lib "Find file within lib directory")
        ("m" projectile-rails-find-model "Find model")
        ("M" projectile-rails-find-migration "Find migration")
        ("o" projectile-rails-find-log "Find log file")
        ("p" projectile-rails-find-spec "Find spec")
        ("r" projectile-rails-find-rake-task "Find rake task file")
        ("s" projectile-rails-find-stylesheet "Find stylesheet file")
        ("t" projectile-rails-find-test "Find test")
        ("u" projectile-rails-find-fixture "Find fixture file")
        ("v" projectile-rails-find-view "Find template or partial")
        ("w" projectile-rails-find-webpack "Find webpack configuration")
        ("y" projectile-rails-find-layout "Find layout file"))
       ("g" "Goto"
        ("c" projectile-rails-find-current-controller "Go to controller")
        ("d" projectile-rails-goto-schema "Open db/schema.rb")
        ("e" projectile-rails-goto-seeds "Open db/seed.rb")
        ("g" projectile-rails-goto-file-at-point "Go to file")
        ("h" projectile-rails-find-current-helper "Go to helper")
        ("j" projectile-rails-find-current-javascript "Go to javascript")
        ("g" projectile-rails-goto-gemfile "Open Gemfile")
        ("m" projectile-rails-find-current-model "Go to model")
        ("n" projectile-rails-find-current-migration "Go to migration")
        ("p" projectile-rails-find-current-spec "Go to spec")
        ("r" projectile-rails-goto-routes "Open config/routes.rb")
        ("s" projectile-rails-find-current-stylesheet "Go to stylesheet")
        ("t" projectile-rails-find-current-test "Go to test")
        ("u" projectile-rails-find-current-fixture "Go to fixture")
        ("v" projectile-rails-find-current-view "Go to template")
        ("z" projectile-rails-goto-spec-helper "Open spec/spec_helper.rb"))
       ("r" "Refactor"
        ("x" projectile-rails-extract-region "Extract region to partial...")))))
    ;; Ex-commands
    (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)))

(defun ruby-on-rails/init-feature-mode ()
  "Initialize Cucumber feature mode"
  (use-package feature-mode
    :mode (("\\.feature\\'" . feature-mode))))

(defun ruby-on-rails/post-init-which-key ()
  (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist))
