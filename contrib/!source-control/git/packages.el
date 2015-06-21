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
        smeargle))

(unless git-use-magit-next
  (push 'magit git-packages)
  ;; not compatible with magit-next
  (push 'magit-svn git-packages)
  (push 'git-commit-mode git-packages)
  (push 'git-rebase-mode git-packages)
  (push 'magit-gitflow git-packages))

(defun git/init-helm-gitignore ()
  (use-package helm-gitignore
    :defer t
    :init (evil-leader/set-key "gI" 'helm-gitignore)))

(defun git/init-git-commit-mode ()
  (use-package git-commit-mode
    :defer t
    :config
    (evil-leader/set-key-for-mode 'git-commit-mode
      "mcc" 'git-commit-commit
      "mk" 'git-commit-abort)))

(defun git/init-git-messenger ()
  (use-package git-messenger
    :defer t
    :init
    (evil-leader/set-key
      "gm" 'git-messenger:popup-message)))

(defun git/init-git-rebase-mode ()
  (use-package git-rebase-mode
    :defer t
    :config
    (progn
      (evilify git-rebase-mode git-rebase-mode-map
               "J" 'git-rebase-move-line-down
               "K" 'git-rebase-move-line-up
               "u" 'git-rebase-undo
               "y" 'git-rebase-insert)
      (evil-leader/set-key-for-mode 'git-rebase-mode
        "mcc" 'git-rebase-server-edit
        "mk" 'git-rebase-abort))))

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
    :if (null git-use-magit-next)
    :defer t
    :init
    (progn
      (setq magit-last-seen-setup-instructions "1.4.0"
            magit-completing-read-function 'magit-ido-completing-read)
      (add-hook 'git-commit-mode-hook 'fci-mode)
      ;; must enable auto-fill-mode again because somehow fci-mode disable it
      (add-hook 'git-commit-mode-hook 'auto-fill-mode)
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
        "gL" 'magit-file-log
        "gs" 'magit-status
        "gd" 'spacemacs/magit-diff-head
        "gC" 'magit-commit)
      (evilify magit-commit-mode magit-commit-mode-map
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-log-mode magit-log-mode-map
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-process-mode magit-process-mode-map
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-branch-manager-mode magit-branch-manager-mode-map
               "K" 'magit-discard-item
               "L" 'magit-key-mode-popup-logging
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-status-mode magit-status-mode-map
               "K" 'magit-discard-item
               "L" 'magit-key-mode-popup-logging
               "H" 'magit-key-mode-popup-diff-options
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-diff-mode magit-diff-mode-map
               "K" 'magit-discard-item
               "L" 'magit-key-mode-popup-logging
               "H" 'magit-key-mode-popup-diff-options
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item))
    :config
    (progn
      (spacemacs|hide-lighter magit-auto-revert-mode)
      ;; full screen magit-status
      (when git-magit-status-fullscreen
        (defadvice magit-status (around magit-fullscreen activate)
          (window-configuration-to-register :magit-fullscreen)
          ad-do-it
          (delete-other-windows))

        (defun magit-quit-session ()
          "Restores the previous window configuration and kills the magit buffer"
          (interactive)
          (kill-buffer)
          (jump-to-register :magit-fullscreen))
        (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

      (defun magit-toggle-whitespace ()
        (interactive)
        (if (member "-w" magit-diff-options)
            (magit-dont-ignore-whitespace)
          (magit-ignore-whitespace)))

      (defun magit-ignore-whitespace ()
        (interactive)
        (add-to-list 'magit-diff-options "-w")
        (magit-refresh))

      (defun magit-dont-ignore-whitespace ()
        (interactive)
        (setq magit-diff-options (remove "-w" magit-diff-options))
        (magit-refresh))
      (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))))

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
