;;; packages.el --- Spacemacs Project Management Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(setq spacemacs-project-packages
      '(
        projectile
        ))


(defun spacemacs-project/init-projectile ()
  (use-package projectile
    :commands (projectile-ack
               projectile-ag
               projectile-compile-project
               projectile-dired
               projectile-find-dir
               projectile-find-file
               projectile-find-tag
               projectile-test-project
               projectile-grep
               projectile-invalidate-cache
               projectile-kill-buffers
               projectile-multi-occur
               projectile-project-p
               projectile-project-root
               projectile-recentf
               projectile-regenerate-tags
               projectile-replace
               projectile-replace-regexp
               projectile-run-async-shell-command-in-root
               projectile-run-shell-command-in-root
               projectile-switch-project
               projectile-switch-to-buffer
               projectile-vc)
    :init
    (progn
      ;; note for Windows: GNU find or Cygwin find must be in path to enable
      ;; fast indexing. Also, because windows ships with a program called
      ;; c:\windows\system32\find.exe that is very much not findutils find
      ;; we ignore that specific executable
      (when (and (spacemacs/system-is-mswindows) (executable-find "find")
                 (not (file-in-directory-p
                       (executable-find "find") "C:\\Windows")))
        (setq  projectile-indexing-method 'alien
               projectile-generic-command "find . -type f"))
      (setq projectile-sort-order 'recentf
            projectile-cache-file (concat spacemacs-cache-directory
                                          "projectile.cache")
            projectile-known-projects-file (concat spacemacs-cache-directory
                                                   "projectile-bookmarks.eld"))
      (spacemacs/set-leader-keys
        ;; File path
        "fyC" 'spacemacs/projectile-copy-file-path-with-line-column
        "fyD" 'spacemacs/projectile-copy-directory-path
        "fyL" 'spacemacs/projectile-copy-file-path-with-line
        "fyY" 'spacemacs/projectile-copy-file-path
        ;; Project
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "p%" 'projectile-replace-regexp
        "pa" 'projectile-toggle-between-implementation-and-test
        "pb" 'projectile-switch-to-buffer
        "pc" 'projectile-compile-project
        "pd" 'projectile-find-dir
        "pD" 'projectile-dired
        "pe" 'projectile-edit-dir-locals
        "pf" 'projectile-find-file
        "pF" 'projectile-find-file-dwim
        "pg" 'projectile-find-tag
        "pG" 'projectile-regenerate-tags
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "pp" 'projectile-switch-project
        "pr" 'projectile-recentf
        "pR" 'projectile-replace
        "pT" 'projectile-test-project
        "pv" 'projectile-vc))
    :config
    (progn
      (projectile-mode)
      (spacemacs|hide-lighter projectile-mode))))
