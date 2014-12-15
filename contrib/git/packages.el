(defvar git-packages
  '(
    git-gutter-fringe
    git-messenger
    git-timemachine
    magit
    magit-gitflow
    smeargle
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(when git-enable-github-support
  (mapc (lambda (x) (push x git-packages))
        '(
          gist
          ;; not up to date
          ;; helm-gist
          magit-gh-pulls
          )))

(defun git/init-gist ()
  (use-package gist
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

(defun git/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :init
    (add-to-hooks 'git-gutter-mode '(markdown-mode-hook
                                     org-mode-hook
                                     prog-mode-hook
                                     ))
    :config
    (progn
      (setq git-gutter:hide-gutter t)
      ;; Don't need log/message.
      (setq git-gutter:verbosity 0)
      (setq git-gutter-fr:side 'right-fringe)
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
        )
      (spacemacs|hide-lighter git-gutter-mode))))

(defun git/init-git-messenger ()
  (use-package git-messenger
    :defer t
    :init
    (evil-leader/set-key
      "gm" 'git-messenger:popup-message)))

(defun git/init-git-timemachine ()
  (use-package git-timemachine
    :defer t
    :init
    (evil-leader/set-key
      "gt" 'git-timemachine)))

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
    (evil-leader/set-key "gs" 'magit-status)
    :config
    (progn
      (spacemacs|hide-lighter magit-auto-revert-mode)
      ;; full screen magit-status
      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))

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


      (defun magit-quit-session ()
        "Restores the previous window configuration and kills the magit buffer"
        (interactive)
        (kill-buffer)
        (jump-to-register :magit-fullscreen))
      (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

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
    :defer t
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
    :config (spacemacs|diminish magit-gh-pulls-mode "Github-PR")))

(defun git/init-magit-gitflow ()
  (use-package magit-gitflow
    :commands turn-on-magit-gitflow
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
    :config (spacemacs|diminish magit-gitflow-mode "Flow")))

(defun git/init-smeargle ()
  (use-package smeargle
    :defer t
    :init
    (evil-leader/set-key
      "ghc" 'smeargle-clear
      "ghh" 'smeargle-commits
      "ght" 'smeargle)))
