;;; packages.el --- Github Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq github-packages
      '(
        ;; forge requires a C compiler on Windows so we disable
        ;; it by default on Windows.
        (forge :toggle (not (spacemacs/system-is-mswindows)))
        gist
        github-clone
        github-search
        ;; this package does not exits, we need it to wrap
        ;; the call to spacemacs/declare-prefix.
        (spacemacs-github :location built-in)
        ))

(defun github/init-forge ()
  (use-package forge
    :after magit
    :init
    (progn
      (setq forge-database-file (concat spacemacs-cache-directory
                                        "forge-database.sqlite"))
      (spacemacs/set-leader-keys-for-major-mode 'forge-topic-mode
        "c" 'forge-create-post
        "e" 'forge-edit-post)
      (spacemacs/set-leader-keys-for-major-mode 'forge-post-mode
        dotspacemacs-major-mode-leader-key 'forge-post-submit
        "c" 'forge-post-submit
        "k" 'forge-post-cancel
        "a" 'forge-post-cancel))))

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

(defun github/init-spacemacs-github ()
  (spacemacs/declare-prefix "gh" "github"))
