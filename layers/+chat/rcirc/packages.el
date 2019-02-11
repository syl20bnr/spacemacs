;;; config.el --- rcirc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
        (rcirc-late-fix :location local
                        :toggle rcirc-enable-late-fix)
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

(defun rcirc/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'spacemacs//rcirc-persp-filter-save-buffers-function)
      (spacemacs|define-custom-layout rcirc-spacemacs-layout-name
        :binding rcirc-spacemacs-layout-binding
        :body
        (progn
          (add-hook 'rcirc-mode-hook #'spacemacs//rcirc-buffer-to-persp)
          (call-interactively #'spacemacs/rcirc))))))

(defun rcirc/init-rcirc ()
  (use-package rcirc
    :defer t
    :init
    (progn
      (spacemacs/add-to-hook 'rcirc-mode-hook '(rcirc-omit-mode
                                         rcirc-track-minor-mode))

      (spacemacs/set-leader-keys "air" 'spacemacs/rcirc)
      (spacemacs/declare-prefix "ai"  "irc")
      (evil-set-initial-state 'rcirc-mode 'insert))
      (setq rcirc-fill-column 80
            rcirc-buffer-maximum-lines 2048
            rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
            rcirc-time-format "%Y-%m-%d %H:%M "
            rcirc-omit-threshold 20
            rcirc-log-directory (concat spacemacs-cache-directory "/rcirc-logs/")
            rcirc-log-flag t)
    :config
    (progn
      ;; (set-input-method "latin-1-prefix")
      (set (make-local-variable 'scroll-conservatively) 8192)

      ;; Exclude rcirc properties when yanking, in order to be able to send mails
      ;; for example.
      (add-to-list 'yank-excluded-properties 'rcirc-text)

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
      (add-hook 'rcirc-print-hooks 'spacemacs//rcirc-write-log)

      ;; dependencies
      ;; will autoload rcirc-notify
      (rcirc-notify-add-hooks)
      (require 'rcirc-color))))

(defun rcirc/init-rcirc-color ()
  (use-package rcirc-color :defer t))

(defun rcirc/init-rcirc-late-fix ()
  (spacemacs|use-package-add-hook rcirc
    :post-config
    (when rcirc-enable-late-fix
      (use-package rcirc-late-fix))))

(defun rcirc/init-rcirc-notify ()
  (use-package rcirc-notify
    :defer t
    :config
    (progn
      (add-hook 'rcirc-notify-page-me-hooks 'spacemacs/rcirc-notify-beep))))
