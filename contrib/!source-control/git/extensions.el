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
      ;; (spacemacs|hide-lighter magit-auto-revert-mode)
      (define-key magit-staged-section-map "k" 'evil-previous-visual-line)
      (define-key magit-hunk-section-map "k" 'evil-previous-visual-line)
      (define-key magit-file-section-map "k" 'evil-previous-visual-line)
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
