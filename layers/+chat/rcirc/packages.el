;;; packages.el --- rcirc Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst rcirc-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    emojify
    (erc-image :toggle rcirc-enable-erc-image)
    (erc-tweet :toggle rcirc-enable-erc-tweet)
    (erc-yt :toggle rcirc-enable-erc-yt)
    flyspell
    (helm-rcirc :location local
                :requires helm)
    persp-mode
    rcirc
    rcirc-color
    (rcirc-late-fix :location local
                    :toggle rcirc-enable-late-fix)
    rcirc-notify
    (rcirc-styles :toggle rcirc-enable-styles)
    window-purpose))

(defun rcirc/post-init-company ()
  (space-macs|add-company-backends :backends company-capf :modes rcirc-mode))

(defun rcirc/post-init-company-emoji ()
  (space-macs|add-company-backends :backends company-emoji :modes rcirc-mode))

(defun rcirc/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'rcirc-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun rcirc/post-init-emojify ()
  (space-macs|use-package-add-hook rcirc
    :post-config
    (use-package emojify
      :hook (rcirc-mode . emojify-mode)
      :if rcirc-enable-emojify)))

(defun rcirc/init-erc-image ()
  (space-macs|use-package-add-hook rcirc
    :post-config
    (use-package erc-image
      :if rcirc-enable-erc-image
      :init (with-eval-after-load 'rcirc
              (setq erc-image-images-path (concat space-macs-cache-directory
                                                  "erc-image/"))
              (make-directory erc-image-images-path t)
              (add-hook 'rcirc-markup-text-functions
                        #'space-macs//rcirc-image-show-url)))))

(defun rcirc/init-erc-tweet ()
  (space-macs|use-package-add-hook rcirc
    :post-config
    (use-package erc-tweet
      :if rcirc-enable-erc-tweet
      :init (with-eval-after-load 'rcirc
              (setq erc-tweet-cache-dir (concat space-macs-cache-directory
                                                "erc-tweet/"))
              (make-directory erc-tweet-cache-dir t)
              (add-hook 'rcirc-markup-text-functions
                        #'space-macs//rcirc-tweet-show-tweet)))))

(defun rcirc/init-erc-yt ()
  (space-macs|use-package-add-hook rcirc
    :post-config
    (use-package erc-yt
      :if rcirc-enable-erc-yt
      :init
      (with-eval-after-load 'rcirc
        (setq erc-yt-cache-dir (concat space-macs-cache-directory "erc-yt/"))
        (make-directory erc-yt-cache-dir t)
        (add-hook 'rcirc-markup-text-functions
                  #'space-macs//rcirc-youtube-show-info)))))

(defun rcirc/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'rcirc-mode-hook))

(defun rcirc/init-helm-rcirc ()
  (use-package helm-rcirc
    :commands helm-rcirc-auto-join-channels
    :init
    (space-macs/set-leader-keys "irc" 'helm-rcirc-auto-join-channels)))

(defun rcirc/pre-init-persp-mode ()
  (space-macs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'space-macs//rcirc-persp-filter-save-buffers-function)
      (space-macs|define-custom-layout rcirc-space-macs-layout-name
        :binding rcirc-space-macs-layout-binding
        :body
        (progn
          (add-hook 'rcirc-mode-hook #'space-macs//rcirc-buffer-to-persp)
          (call-interactively #'space-macs/rcirc))))))

(defun rcirc/init-rcirc ()
  (use-package rcirc
    :defer t
    :init
    (progn
      (space-macs/add-to-hook 'rcirc-mode-hook '(rcirc-omit-mode
                                                rcirc-track-minor-mode))

      (space-macs/set-leader-keys "acir" 'space-macs/rcirc)
      (space-macs/declare-prefix "aci"  "irc")
      (evil-set-initial-state 'rcirc-mode 'insert)
      (setq rcirc-fill-column 80
            rcirc-buffer-maximum-lines 2048
            rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
            rcirc-time-format "%Y-%m-%d %H:%M "
            rcirc-omit-threshold 20
            rcirc-log-directory (concat space-macs-cache-directory "/rcirc-logs/")
            rcirc-log-flag t))
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
        (define-key rcirc-mode-map (kbd "C-c C-e") 'space-macs/rcirc-insert-current-emms-track))

      ;; Minimal logging to `~/.e-macs.d/.cache/rcirc-logs/'
      ;; by courtesy of Trent Buck.
      (add-hook 'rcirc-print-hooks 'space-macs//rcirc-write-log)

      ;; dependencies
      ;; will autoload rcirc-notify
      (rcirc-notify-add-hooks)
      (require 'rcirc-color)
      (when rcirc-enable-styles
        (require 'rcirc-styles)
        (space-macs/declare-prefix-for-mode 'rcirc-mode "mi" "insert")
        (space-macs/set-leader-keys-for-major-mode 'rcirc-mode
          "ic" 'rcirc-styles-insert-color
          "ia" 'rcirc-styles-insert-attribute
          "ip" 'rcirc-styles-toggle-preview)))))

(defun rcirc/init-rcirc-color ()
  (use-package rcirc-color :defer t))

(defun rcirc/init-rcirc-late-fix ()
  (space-macs|use-package-add-hook rcirc
    :post-config
    (when rcirc-enable-late-fix
      (use-package rcirc-late-fix))))

(defun rcirc/init-rcirc-styles ()
  (use-package rcirc-styles))

(defun rcirc/init-rcirc-notify ()
  (use-package rcirc-notify
    :defer t
    :config
    (progn
      (add-hook 'rcirc-notify-page-me-hooks 'space-macs/rcirc-notify-beep))))

(defun rcirc/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :rcirc-layer
   (purpose-conf :mode-purposes '((rcirc-mode . chat)))))


