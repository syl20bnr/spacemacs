(setq rcirc-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    flyspell
    (helm-rcirc :location local
                :requires helm)
    persp-mode
    rcirc
    rcirc-color
    rcirc-notify
    ))

(defun rcirc/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes rcirc-mode))

(defun rcirc/post-init-company-emoji ()
  (spacemacs|add-company-backends :backends company-emoji :modes rcirc-mode))

(defun rcirc/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'rcirc-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun rcirc/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'rcirc-mode-hook))

(defun rcirc/init-helm-rcirc ()
  (use-package helm-rcirc
    :commands helm-rcirc-auto-join-channels
    :init
    (spacemacs/set-leader-keys "irc" 'helm-rcirc-auto-join-channels)))

(defun rcirc/post-init-persp-mode ()
  ;; do not save rcirc buffers
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'rcirc-mode)))
          persp-filter-save-buffers-functions))

  (spacemacs|define-custom-layout rcirc-spacemacs-layout-name
    :binding rcirc-spacemacs-layout-binding
    :body
    (progn
      (add-hook 'rcirc-mode-hook #'spacemacs-layouts/add-rcirc-buffer-to-persp)
      (call-interactively 'spacemacs/rcirc))))

(defun rcirc/init-rcirc ()
  (use-package rcirc
    :defer t
    :init
    (progn
      (spacemacs/add-to-hook 'rcirc-mode-hook '(rcirc-omit-mode
                                         rcirc-track-minor-mode))

      (spacemacs/set-leader-keys "air" 'spacemacs/rcirc)
      (evil-set-initial-state 'rcirc-mode 'insert))
    :config
    (progn
      ;; (set-input-method "latin-1-prefix")
      (set (make-local-variable 'scroll-conservatively) 8192)

      (setq rcirc-fill-column 80
            rcirc-buffer-maximum-lines 2048
            rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
            rcirc-time-format "%Y-%m-%d %H:%M "
            rcirc-omit-threshold 20)

      ;; Exclude rcirc properties when yanking, in order to be able to send mails
      ;; for example.
      (add-to-list 'yank-excluded-properties 'rcirc-text)

      ;; rcirc-reconnect
      (let ((dir (configuration-layer/get-layer-local-dir 'rcirc)))
        (require 'rcirc-reconnect
                 (concat dir "rcirc-reconnect/rcirc-reconnect.el")))

      ;; load this file from the dropbox location load-path
      ;; this is where you can store personal information
      (require 'pinit-rcirc nil 'noerror)

      (evil-define-key 'normal rcirc-mode-map
        (kbd "C-j") 'rcirc-insert-prev-input
        (kbd "C-k") 'rcirc-insert-next-input)

      ;; add a key for EMMS integration
      (when (boundp 'emms-track-description)
        (define-key rcirc-mode-map (kbd "C-c C-e") 'spacemacs/rcirc-insert-current-emms-track))

      ;; Minimal logging to `~/.emacs.d/.cache/rcirc-logs/'
      ;; by courtesy of Trent Buck.
      (setq rcirc-log-directory (concat spacemacs-cache-directory "/rcirc-logs/"))
      (setq rcirc-log-flag t)
      (add-hook 'rcirc-print-hooks 'spacemacs//rcirc-write-log)

      ;; dependencies
      ;; will autoload rcirc-notify
      (rcirc-notify-add-hooks)
      (require 'rcirc-color))))

(defun rcirc/init-rcirc-color ()
  (use-package rcirc-color :defer t))

(defun rcirc/init-rcirc-notify ()
  (use-package rcirc-notify
    :defer t
    :config
    (progn
      (add-hook 'rcirc-notify-page-me-hooks 'spacemacs/rcirc-notify-beep))))
