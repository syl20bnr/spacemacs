;;; packages.el --- Git Layer packages File for Spacemacs
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

(setq git-packages
      '(
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        git-messenger
        git-timemachine
        helm-gitignore
        magit
        ;; not compatible with magit 2.1 at the time of release
        ;; magit-gitflow
        ;; not compatible with magit 2.1 at the time of release
        ;; magit-svn
        smeargle
        ))

(defun git/init-helm-gitignore ()
  (use-package helm-gitignore
    :defer t
    :init (evil-leader/set-key "gI" 'helm-gitignore)))

(defun git/init-git-messenger ()
  (use-package git-messenger
    :defer t
    :init
    (evil-leader/set-key
      "gm" 'git-messenger:popup-message)))

(defun git/init-git-timemachine ()
  (use-package git-timemachine
    :defer t
    :commands spacemacs/time-machine-micro-state
    :init
    (evil-leader/set-key
      "gt" 'spacemacs/time-machine-micro-state)

    :config
    (progn

      (defun spacemacs//time-machine-ms-on-enter ()
        "Initiate git-timemachine properly with goden-ratio support."
        (let ((golden-ratio (when (boundp 'golden-ratio-mode)
                              golden-ratio-mode)))
          (when (bound-and-true-p golden-ratio-mode) (golden-ratio-mode -1))
          (git-timemachine)
          (when golden-ratio (golden-ratio-mode))))

      (spacemacs|define-micro-state time-machine
        :doc "[p] [N] previous [n] next [c] current [Y] copy hash [q] quit"
        :on-enter (spacemacs//time-machine-ms-on-enter)
        :on-exit (git-timemachine-quit)
        :persistent t
        :bindings
        ("c" git-timemachine-show-current-revision)
        ("p" git-timemachine-show-previous-revision)
        ("n" git-timemachine-show-next-revision)
        ("N" git-timemachine-show-previous-revision)
        ("Y" git-timemachine-kill-revision)
        ("q" nil :exit t)))))

(defun git/init-gitattributes-mode ()
  (use-package gitattributes-mode
    :defer t))

(defun git/init-gitconfig-mode ()
  (use-package gitconfig-mode
    :defer t))

(defun git/init-gitignore-mode ()
  (use-package gitignore-mode
    :defer t))

(defun git/init-magit ()
  (use-package magit
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
        "gb" 'magit-blame
        "gl" 'magit-log-all
        "gL" 'magit-log-buffer-file
        "gs" 'magit-status
        "gd" 'spacemacs/magit-diff-head
        "gC" 'magit-commit))
    :config
    (progn
      ;; seems to be necessary at the time of release
      (require 'git-rebase)
      ;; mode maps
      (spacemacs|evilify-map magit-mode-map)
      (spacemacs|evilify-map magit-status-mode-map
        :mode magit-status-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-refs-mode-map
        :mode magit-refs-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-blame-mode-map
        :mode magit-blame-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-diff-mode-map
        :mode magit-diff-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-log-read-revs-map
        :mode magit-log-read-revs
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-log-mode-map
        :mode magit-log-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-log-select-mode-map
        :mode magit-log-select-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-cherry-mode-map
        :mode magit-cherry-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-reflog-mode-map
        :mode magit-reflog-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map magit-process-mode-map
        :mode magit-process-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward)
      (spacemacs|evilify-map git-rebase-mode-map
        :mode git-rebase-mode
        :bindings
        (kbd "C-S-j") 'magit-section-forward
        (kbd "C-S-k") 'magit-section-backward
        (kbd "C-n") 'magit-section-forward
        (kbd "C-p") 'magit-section-backward
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

(defun git/init-magit-gitflow ()
  (use-package magit-gitflow
    :commands turn-on-magit-gitflow
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
    :config (spacemacs|diminish magit-gitflow-mode "Flow")))

(defun git/init-magit-svn ()
  (use-package magit-svn
    :if git-enable-magit-svn-plugin
    :commands turn-on-magit-svn
    :init (add-hook 'magit-mode-hook 'turn-on-magit-svn)
    :config
    (progn
      (evil-define-key 'emacs magit-status-mode-map
        "N" 'magit-key-mode-popup-svn))))

(defun git/init-smeargle ()
  (use-package smeargle
    :defer t
    :init
    (evil-leader/set-key
      "ghc" 'smeargle-clear
      "ghh" 'smeargle-commits
      "ght" 'smeargle)))
