;;; extensions.el --- Git Layer Extensions File for Spacemacs
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

(setq git-post-extensions '())

(when git-use-magit-next
  (setq git-post-extensions '(magit-next)))

(defun git/init-magit-next ()
  (use-package magit
    :if git-use-magit-next
    :commands (magit-status
               magit-blame-mode
               magit-log
               magit-commit)
    :init
    (progn
      (add-to-list 'load-path (format "%smagit-next/lisp/"
                                      (configuration-layer/get-layer-property
                                       'git :ext-dir)))
      (setq magit-completing-read-function 'magit-builtin-completing-read)
      (add-hook 'git-commit-mode-hook 'fci-mode)
      ;; On Windows, we must use Git GUI to enter username and password
      ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
      (when (eq window-system 'w32)
        (setenv "GIT_ASKPASS" "git-gui--askpass"))

      (defun spacemacs/magit-diff-head ()
        "Execute `magit-diff' against current HEAD."
        (interactive)
        (magit-diff "HEAD"))

      (evil-leader/set-key
        "gb" 'magit-blame-mode
        "gl" 'magit-log
        "gL" 'magit-log-buffer-file
        "gs" 'magit-status
        "gd" 'spacemacs/magit-diff-head
        "gC" 'magit-commit))
    :config
    (progn
      ;; mode maps
      (spacemacs|evilify-map magit-mode-map)
      (spacemacs|evilify-map magit-status-mode-map
                             :mode magit-status-mode)
      (spacemacs|evilify-map magit-refs-mode-map
                             :mode magit-refs-mode)
      (spacemacs|evilify-map magit-blame-mode-map
                             :mode magit-blame-mode)
      (spacemacs|evilify-map magit-diff-mode-map
                             :mode magit-diff-mode)
      (spacemacs|evilify-map magit-log-read-revs-map
                             :mode magit-log-read-revs)
      (spacemacs|evilify-map magit-log-mode-map
                             :mode magit-log-mode)
      (spacemacs|evilify-map magit-log-select-mode-map
                             :mode magit-log-select-mode)
      (spacemacs|evilify-map magit-cherry-mode-map
                             :mode magit-cherry-mode)
      (spacemacs|evilify-map magit-reflog-mode-map
                             :mode magit-reflog-mode)
      (spacemacs|evilify-map magit-process-mode-map
                             :mode magit-process-mode)
      (spacemacs|evilify-map git-rebase-mode-map
                             :mode git-rebase-mode
                             :bindings
                             "J" 'git-rebase-move-line-down
                             "K" 'git-rebase-move-line-up
                             "u" 'git-rebase-undo
                             "y" 'git-rebase-insert)
      ;; default state for additional modes
      (dolist (mode '(magit-popup-mode
                      magit-popup-sequence-mode))
        (add-to-list 'evil-emacs-state-modes mode))
      (spacemacs/evilify-configure-default-state 'magit-revision-mode)
      ;; section maps
      (spacemacs|evilify-map magit-tag-section-map)
      (spacemacs|evilify-map magit-untracked-section-map)
      (spacemacs|evilify-map magit-branch-section-map)
      (spacemacs|evilify-map magit-remote-section-map)
      (spacemacs|evilify-map magit-file-section-map)
      (spacemacs|evilify-map magit-hunk-section-map)
      (spacemacs|evilify-map magit-unstaged-section-map)
      (spacemacs|evilify-map magit-staged-section-map)
      (spacemacs|evilify-map magit-commit-section-map)
      (spacemacs|evilify-map magit-module-commit-section-map)
      (spacemacs|evilify-map magit-unpulled-section-map)
      (spacemacs|evilify-map magit-unpushed-section-map)
      (spacemacs|evilify-map magit-stashes-section-map)
      (spacemacs|evilify-map magit-stash-section-map)

      ;; full screen magit-status
      (when git-magit-status-fullscreen
        (setq magit-restore-window-configuration t)
        (setq magit-status-buffer-switch-function
              (lambda (buffer)
                (pop-to-buffer buffer)
                (delete-other-windows))))

      ;; rebase mode
      (evil-leader/set-key-for-mode 'git-rebase-mode
        "mcc" 'git-rebase-server-edit
        "mk" 'git-rebase-abort)
      ;; commit mode
      (evil-leader/set-key-for-mode 'git-commit-mode
        "mcc" 'git-commit-commit
        "mk" 'git-commit-abort)

      ;; whitespace
      (defun magit-toggle-whitespace ()
        (interactive)
        (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
			     magit-refresh-args
			   magit-diff-section-arguments))
            (magit-dont-ignore-whitespace)
          (magit-ignore-whitespace)))
      (defun magit-ignore-whitespace ()
        (interactive)
        (add-to-list (if (derived-mode-p 'magit-diff-mode)
			 'magit-refresh-args 'magit-diff-section-arguments) "-w")
        (magit-refresh))
      (defun magit-dont-ignore-whitespace ()
        (interactive)
        (setq magit-diff-options
              (remove "-w"
                      (if (derived-mode-p 'magit-diff-mode)
                          magit-refresh-args
                        magit-diff-section-arguments))) (magit-refresh))
      (define-key magit-status-mode-map (kbd "C-S-w")
        'magit-toggle-whitespace))))
