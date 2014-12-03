(defvar github-packages
  '(
    gist
    ;; not up to date
    ;; helm-gist
    magit-gh-pulls
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun github/init-gist ()
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

(defun github/init-magit-gh-pulls ()
  (use-package magit-gh-pulls ()
    :defer t
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
    :config (spacemacs//diminish magit-gh-pulls-mode "Github-PR")))
