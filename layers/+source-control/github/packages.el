;;; packages.el --- Github Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq github-packages
      '(
        forge
        gist
        github-clone
        github-search
        magithub
        ;; this package does not exits, we need it to wrap
        ;; the call to spacemacs/declare-prefix.
        (spacemacs-github :location built-in)
        ))

(defun github/init-forge ()
  (use-package forge
    :after magit
    :init (setq forge-database-file (concat spacemacs-cache-directory
                                            "forge-database.sqlite"))))

(defun github/init-gist ()
  (use-package gist
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "gg" "github gist")
      (spacemacs/set-leader-keys
        "ggb" 'gist-buffer
        "ggB" 'gist-buffer-private
        "ggl" 'gist-list
        "ggr" 'gist-region
        "ggR" 'gist-region-private))
    :config
    (progn
      (evilified-state-evilify-map gist-list-menu-mode-map
        :mode gist-list-mode
        :bindings
        "f" 'gist-fetch-current
        "K" 'gist-kill-current
        "o" 'gist-browse-current-url)
      (evilified-state-evilify-map gist-list-mode-map
        :mode gist-list-mode
        :bindings
        (kbd "gr") 'gist-list-reload))))

(defun github/init-github-clone ()
  (use-package github-clone
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "ghc" "clone")
      (spacemacs/set-leader-keys
        "ghcc" 'github-clone
        "ghcr" 'github-clone-add-existing-remote
        "ghcf" 'github-clone-fork-remote
        "ghcu" 'github-clone-add-source-remote))))

(defun github/init-github-search ()
  (use-package github-search
    :commands (github-search-clone-repo github-search-user-clone-repo)
    :init (spacemacs/set-leader-keys "ghc/" 'github-search-clone-repo)))


(defun github/init-magithub ()
  (use-package magithub
    :after magit
    :init
    (setq magithub-dir (concat spacemacs-cache-directory "magithub/"))
    :config
    (progn
      ;; Configure Magithub to be offline by default because loading data from
      ;; projects with many pull requests or issues can be exorbitantly slow.
      ;; See <https://github.com/syl20bnr/spacemacs/issues/11176>.
      (when (null (magit-get "--global" "magithub.online"))
        (magit-set "false" "--global" "magithub.online")
        (magit-set "false" "--global" "magithub.status.includeStatusHeader")
        (magit-set "false" "--global" "magithub.status.includePullRequestsSection")
        (magit-set "false" "--global" "magithub.status.includeIssuesSection"))
      (magithub-feature-autoinject `(,@(when (not (package-installed-p 'forge))
                                         '(issues-section
                                           pull-requests-section))
                                     completion
                                     status-checks-header
                                     commit-browse
                                     pull-request-merge))
      (define-key magit-status-mode-map "@" #'magithub-dispatch-popup))))

(defun github/init-spacemacs-github ()
  (spacemacs/declare-prefix "gh" "github"))
