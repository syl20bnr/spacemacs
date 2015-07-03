;;; packages.el --- Github Layer packages File for Spacemacs
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

(setq github-packages
      '(
        gist
        git-link
        github-browse-file
        ;; not up to date
        ;; helm-gist
        ))

(defun github/init-gist ()
  (use-package gist
    :defer t
    :init
    (progn
      (evilify gist-list-menu-mode gist-list-menu-mode-map
               "f" 'gist-fetch-current
               "K" 'gist-kill-current
               "o" 'gist-browse-current-url)

      (evil-leader/set-key
        "ggb" 'gist-buffer
        "ggB" 'gist-buffer-private
        "ggl" 'gist-list
        "ggr" 'gist-region
        "ggR" 'gist-region-private))))

;; this mode is not up to date
;; any contributor to make it up to date is welcome:
;; https://github.com/emacs-helm/helm-gist
;;
;; (defun github/init-helm-gist ()
;;   (use-package helm-gist
;;     :commands egist-mode
;;     :init
;;     (progn
;;       (defun spacemacs/helm-gist-list ()
;;         "List the gists using helm, ensure thath elgist-mode is enabled."
;;         (interactive)
;;         (egist-mode)
;;         (helm-for-gist))

;;       (evil-leader/set-key "ggh" 'spacemacs/helm-gist-list))
;;     ))

(defun github/init-github-browse-file ()
  (use-package github-browse-file
    :defer t
    :init
    (evil-leader/set-key
      "gfb" 'github-browse-file)))

(defun github/init-git-link ()
  (use-package git-link
    :defer t
    :init
    (progn

      (defun spacemacs/git-link-copy-url-only ()
        "Only copy the generated link to the kill ring."
        (interactive)
        (let (git-link-open-in-browser)
          (call-interactively 'git-link)))

      (defun spacemacs/git-link-commit-copy-url-only ()
        "Only copy the generated link to the kill ring."
        (interactive)
        (let (git-link-open-in-browser)
          (call-interactively 'git-link-commit)))

      (evil-leader/set-key
        "gfl" 'git-link
        "gfL" 'spacemacs/git-link-copy-url-only
        "gfc" 'git-link-commit
        "gfC" 'spacemacs/git-link-commit-copy-url-only)
      ;; default is to open the generated link
      (setq git-link-open-in-browser t))))
