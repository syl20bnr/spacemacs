;;; packages.el --- Github Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
        github-browse-file
        github-clone
        github-search
        magit-gh-pulls
        ;; this package does not exits, we need it to wrap
        ;; the call to spacemacs/declare-prefix.
        (spacemacs-github :location built-in)
        ))

(defun github/init-gist ()
  (use-package gist
    :defer t
    :init
    (progn
      (evilified-state-evilify gist-list-mode gist-list-menu-mode-map
        "f" 'gist-fetch-current
        "K" 'gist-kill-current
        "o" 'gist-browse-current-url)
      (spacemacs/declare-prefix "gg" "github gist")
      (spacemacs/set-leader-keys
        "ggb" 'gist-buffer
        "ggB" 'gist-buffer-private
        "ggl" 'gist-list
        "ggr" 'gist-region
        "ggR" 'gist-region-private))))

(defun github/init-github-browse-file ()
  (use-package github-browse-file
    :defer t
    :init (spacemacs/set-leader-keys "gho" 'github-browse-file)))

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

(defun github/init-magit-gh-pulls ()
  (spacemacs|use-package-add-hook magit
    :pre-config
    (progn
      (use-package magit-gh-pulls
        :init
        (progn
          (defun spacemacs/load-gh-pulls-mode ()
            "Start `magit-gh-pulls-mode' only after a manual request."
            (interactive)
            (magit-gh-pulls-mode)
            (magit-gh-pulls-popup))

          (define-key magit-mode-map "#" 'spacemacs/load-gh-pulls-mode))
        :config
        (spacemacs|diminish magit-gh-pulls-mode "Github-PR")))))

(defun github/init-spacemacs-github ()
  (spacemacs/declare-prefix "gh" "github"))
