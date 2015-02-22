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

(defvar git-packages
  '(
    fringe-helper
    git-messenger
    git-rebase-mode
    git-timemachine
    gist
    github-browse-file
    ;; not up to date
    ;; helm-gist
    magit
    magit-gh-pulls
    magit-gitflow
    magit-svn
    smeargle
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(if git-gutter-use-fringe
    (push 'git-gutter-fringe git-packages)
  (push 'git-gutter git-packages))

(defun git/init-gist ()
  (use-package gist
    :if git-enable-github-support
    :defer t
    :init
    (progn
      (add-to-list 'evil-emacs-state-modes 'gist-list-menu-mode)
      (spacemacs|evilify gist-list-menu-mode-map
                         "f" 'gist-fetch-current
                         "K" 'gist-kill-current
                         "o" 'gist-browse-current-url)

      (evil-leader/set-key
        "ggb" 'gist-buffer
        "ggB" 'gist-buffer-private
        "ggl" 'gist-list
        "ggr" 'gist-region
        "ggR" 'gist-region-private))
    :config
    (spacemacs/activate-evil-leader-for-map 'gist-list-menu-mode-map)
    ))

(defun init-git-gutter ()
  "Common initialization of git-gutter."
  (git-gutter-mode)
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (spacemacs|hide-lighter git-gutter-mode)
  (if (and (not git-gutter-use-fringe)
           global-linum-mode)
      (git-gutter:linum-setup)))

(defun git/init-git-gutter ()
  (use-package git-gutter
    :commands git-gutter-mode
    :init
    (progn
      (init-git-gutter)
      (add-to-hooks 'git-gutter-mode '(markdown-mode-hook
                                       org-mode-hook
                                       prog-mode-hook)))))

(defun git/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :init
    (progn
      (defun git/load-git-gutter ()
        "Lazy load git gutter and choose between fringe and no fringe."
        (when (display-graphic-p) (require 'git-gutter-fringe))
        (init-git-gutter))
      (setq git-gutter-fr:side 'right-fringe)
      (add-to-hooks 'git/load-git-gutter '(markdown-mode-hook
                                           org-mode-hook
                                           prog-mode-hook)))
    :config
    (progn
      (setq git-gutter:hide-gutter t)
      ;; Don't need log/message.
      (setq git-gutter:verbosity 0)
      (evil-leader/set-key
        "ghs" 'git-gutter:stage-hunk
        "ghr" 'git-gutter:revert-hunk
        "ghN" 'git-gutter:previous-hunk
        "ghn" 'git-gutter:next-hunk)
      ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
      ;; custom graphics that works nice with half-width fringes
      (fringe-helper-define 'git-gutter-fr:added nil
        "..X...."
        "..X...."
        "XXXXX.."
        "..X...."
        "..X...."
        )
      (fringe-helper-define 'git-gutter-fr:deleted nil
        "......."
        "......."
        "XXXXX.."
        "......."
        "......."
        )
      (fringe-helper-define 'git-gutter-fr:modified nil
        "..X...."
        ".XXX..."
        "XXXXX.."
        ".XXX..."
        "..X...."
        ))))

(defun git/init-git-messenger ()
  (use-package git-messenger
    :defer t
    :init
    (evil-leader/set-key
      "gm" 'git-messenger:popup-message)))

(defun git/init-git-rebase-mode ()
  (use-package git-rebase-mode
    :defer t
    :init
    (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
    :config
    (progn
      (spacemacs|evilify git-rebase-mode-map "y" 'git-rebase-insert)
      (spacemacs/activate-evil-leader-for-map 'git-rebase-mode-map))))

(defun git/init-git-timemachine ()
  (use-package git-timemachine
    :defer t
    :commands spacemacs/time-machine-micro-state
    :init
    (evil-leader/set-key
      "gt" 'spacemacs/time-machine-micro-state)

    :config
    (progn
      (spacemacs|define-micro-state time-machine
        :doc "[q] quit [p] previous [n] next [c] current"
        :on-enter (git-timemachine)
        :on-exit (git-timemachine-quit)
        :persistent t
        :bindings
        ("c" git-timemachine-show-current-revision)
        ("p" git-timemachine-show-previous-revision)
        ("n" git-timemachine-show-next-revision)
        ("q" nil :exit t)
        )
      )))

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
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (evil-leader/set-key
      "gb" 'magit-blame-mode
      "gl" 'magit-log
      "gs" 'magit-status
      "gC" 'magit-commit)
    :config
    (progn
      (spacemacs|hide-lighter magit-auto-revert-mode)

      ;; hjkl key bindings
      (spacemacs|evilify magit-commit-mode-map
        (kbd "C-j") 'magit-goto-next-section
        (kbd "C-k") 'magit-goto-previous-section
        (kbd "C-n") 'magit-goto-next-section
        (kbd "C-p") 'magit-goto-previous-section
        (kbd "C-v") 'magit-revert-item)
      (spacemacs|evilify magit-log-mode-map
        (kbd "C-j") 'magit-goto-next-section
        (kbd "C-k") 'magit-goto-previous-section
        (kbd "C-n") 'magit-goto-next-section
        (kbd "C-p") 'magit-goto-previous-section
        (kbd "C-v") 'magit-revert-item)
      (spacemacs|evilify magit-process-mode-map
        (kbd "C-j") 'magit-goto-next-section
        (kbd "C-k") 'magit-goto-previous-section
        (kbd "C-n") 'magit-goto-next-section
        (kbd "C-p") 'magit-goto-previous-section
        (kbd "C-v") 'magit-revert-item)
      (spacemacs|evilify magit-branch-manager-mode-map
        "K" 'magit-discard-item
        "L" 'magit-key-mode-popup-logging
        (kbd "C-j") 'magit-goto-next-section
        (kbd "C-k") 'magit-goto-previous-section
        (kbd "C-n") 'magit-goto-next-section
        (kbd "C-p") 'magit-goto-previous-section
        (kbd "C-v") 'magit-revert-item)
      (spacemacs|evilify magit-status-mode-map
        "K" 'magit-discard-item
        "L" 'magit-key-mode-popup-logging
        "H" 'magit-key-mode-popup-diff-options
        (kbd "C-j") 'magit-goto-next-section
        (kbd "C-k") 'magit-goto-previous-section
        (kbd "C-n") 'magit-goto-next-section
        (kbd "C-p") 'magit-goto-previous-section
        (kbd "C-v") 'magit-revert-item)
      ;; remove conflicts with evil leader
      (spacemacs/activate-evil-leader-for-maps '(magit-mode-map
                                                 magit-commit-mode-map
                                                 magit-diff-mode-map))

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
  (use-package magit-gh-pulls ()
    :if git-enable-github-support
    :defer t
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
    :config (spacemacs|diminish magit-gh-pulls-mode "Github-PR")))

(defun git/init-github-browse-file ()
  (use-package github-browse-file
    :if git-enable-github-support
    :defer t
    :init
    (evil-leader/set-key
      "gfb" 'github-browse-file)))

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
