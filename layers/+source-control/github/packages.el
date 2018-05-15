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
        gist
        github-clone
        github-search
        magit-gh-pulls
        magithub
        ;; this package does not exits, we need it to wrap
        ;; the call to spacemacs/declare-prefix.
        (spacemacs-github :location built-in)
        ))

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

;; magit-gh-pulls has to be loaded via a pre-config hook because the source code
;; makes assumptions about the status of the magit-mode keymaps that are
;; incompatible with the spacemacs' evilification feature.
;; To avoid errors, magit-gh-pulls must be loaded after magit, but before magit
;; is configured by spacemacs.

(defun github/pre-init-magit-gh-pulls ()
  (spacemacs|use-package-add-hook magit
    :pre-config
    (progn
      (use-package magit-gh-pulls
        :config
        (define-key magit-mode-map "#" 'spacemacs/load-gh-pulls-mode)
        (spacemacs|diminish magit-gh-pulls-mode "Github-PR")))))
(defun github/init-magit-gh-pulls ())

(defun github/init-magithub ()
  (use-package magithub
    :defer t
    :after magit
    :init
    (setq magithub-dir (concat spacemacs-cache-directory "magithub/"))
    :config
    (progn
      (magithub-feature-autoinject t)
      (define-key magit-status-mode-map "@" #'magithub-dispatch-popup))))

(defun github/init-spacemacs-github ()
  (spacemacs/declare-prefix "gh" "github"))
