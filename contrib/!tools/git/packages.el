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
        diff-mode
        diff-hl
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        git-commit-mode
        git-messenger
        git-rebase-mode
        git-timemachine
        gist
        github-browse-file
        git-link
        ;; not up to date
        ;; helm-gist
        helm-gitignore
        magit
        magit-gh-pulls
        magit-gitflow
        magit-svn
        smeargle))

(defun git/init-diff-mode ()
  (use-package diff-mode
    :defer t
    :config
    (evilify diff-mode diff-mode-map
             "j" 'diff-hunk-next
             "k" 'diff-hunk-prev)))

(defun git/init-diff-hl ()
  (use-package diff-hl
    :defer t
    :init
    (progn
      (setq diff-hl-side 'right)
      (global-diff-hl-mode)
      (unless (display-graphic-p)
        (setq diff-hl-side 'left)
        (diff-hl-margin-mode))
      (evil-leader/set-key
        "ghr" 'diff-hl-revert-hunk
        "ghN" 'diff-hl-previous-hunk
        "ghn" 'diff-hl-next-hunk
        "ghg" 'diff-hl-diff-goto-hunk))))

(defun git/init-gist ()
  (use-package gist
    :if git-enable-github-support
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

;; this mode is not up to date
;; any contributor to make it up to date is welcome:
;; https://github.com/emacs-helm/helm-gist
;;
;; (defun git/init-helm-gist ()
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

(defun git/init-magit ()
  (use-package magit
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
      (evil-leader/set-key
        "gb" 'magit-blame-mode
        "gl" 'magit-log
        "gs" 'magit-status
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

(defun git/init-magit-gh-pulls ()
  (use-package magit-gh-pulls
    :if git-enable-github-support
    :defer t
    :init
    (progn
      (defun spacemacs/load-gh-pulls-mode ()
        "Start `magit-gh-pulls-mode' only after a manual request."
        (interactive)
        (magit-gh-pulls-mode)
        (magit-gh-pulls-reload))
      (defun spacemacs/fetch-gh-pulls-mode ()
        "Start `magit-gh-pulls-mode' only after a manual request."
        (interactive)
        (magit-gh-pulls-mode)
        (magit-gh-pulls-fetch-commits))
      (eval-after-load 'magit
        '(progn
           (define-key magit-mode-map "#gg" 'spacemacs/load-gh-pulls-mode)
           (define-key magit-mode-map "#gf" 'spacemacs/fetch-gh-pulls-mode))))
    :config
    (spacemacs|diminish magit-gh-pulls-mode "Github-PR")))

(defun git/init-github-browse-file ()
  (use-package github-browse-file
    :if git-enable-github-support
    :defer t
    :init
    (evil-leader/set-key
      "gfb" 'github-browse-file)))

(defun git/init-git-link ()
  (use-package git-link
    :if git-enable-github-support
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
