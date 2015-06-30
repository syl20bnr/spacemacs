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
        "gs" 'magit-status
        "gd" 'spacemacs/magit-diff-head
        "gC" 'magit-commit))
    :config
    (progn
      ;; (spacemacs|hide-lighter magit-auto-revert-mode)


      ;; mode maps
      (spacemacs/evilify-map 'magit-mode-map)
      (spacemacs/evilify-map 'magit-status-mode-map 'magit-status-mode)
      (spacemacs/evilify-map 'magit-refs-mode-map 'magit-refs-mode)
      (spacemacs/evilify-map 'magit-blame-mode-map 'magit-blame-mode)
      (spacemacs/evilify-map 'magit-diff-mode-map 'magit-diff-mode)
      (spacemacs/evilify-map 'magit-log-read-revs-map 'magit-log-read-revs)
      (spacemacs/evilify-map 'magit-log-mode-map 'magit-log-mode)
      (spacemacs/evilify-map 'magit-log-select-mode-map 'magit-log-select-mode)
      (spacemacs/evilify-map 'magit-cherry-mode-map 'magit-cherry-mode)
      (spacemacs/evilify-map 'magit-reflog-mode-map 'magit-reflog-mode)
      (spacemacs/evilify-map 'magit-process-mode-map 'magit-process-mode)
      ;; default state for additional modes
      (dolist (mode '(magit-popup-mode
                      magit-popup-sequence-mode))
        (add-to-list 'evil-insert-state-modes mode))
      (spacemacs/evilify-configure-default-state 'magit-revision-mode)
      ;; section maps
      (spacemacs/evilify-map 'magit-tag-section-map)
      (spacemacs/evilify-map 'magit-untracked-section-map)
      (spacemacs/evilify-map 'magit-branch-section-map)
      (spacemacs/evilify-map 'magit-remote-section-map)
      (spacemacs/evilify-map 'magit-file-section-map)
      (spacemacs/evilify-map 'magit-hunk-section-map)
      (spacemacs/evilify-map 'magit-unstaged-section-map)
      (spacemacs/evilify-map 'magit-staged-section-map)
      (spacemacs/evilify-map 'magit-commit-section-map)
      (spacemacs/evilify-map 'magit-module-commit-section-map)
      (spacemacs/evilify-map 'magit-unpulled-section-map)
      (spacemacs/evilify-map 'magit-unpushed-section-map)
      (spacemacs/evilify-map 'magit-stashes-section-map)
      (spacemacs/evilify-map 'magit-stash-section-map)

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
        (magit-refresh)))
    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))
