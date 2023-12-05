;;; packages.el --- rcirc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
    (rcirc-late-fix :location (recipe :fetcher url
                                      :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/rcirc-late-fix.el")
                    :toggle rcirc-enable-late-fix)
    rcirc-notify
    (rcirc-styles :toggle rcirc-enable-styles)
    window-purpose))

(defun rcirc/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes rcirc-mode))

(defun rcirc/post-init-company-emoji ()
  (spacemacs|add-company-backends :backends company-emoji :modes rcirc-mode))

(defun rcirc/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'rcirc-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun rcirc/post-init-emojify ()
  (spacemacs|use-package-add-hook rcirc
    :post-config
    (use-package emojify
      :hook (rcirc-mode . emojify-mode)
      :if rcirc-enable-emojify)))

(defun rcirc/init-erc-image ()
  (spacemacs|use-package-add-hook rcirc
    :post-config
    (use-package erc-image
      :if rcirc-enable-erc-image
      :init (with-eval-after-load 'rcirc
              (setq erc-image-images-path (concat spacemacs-cache-directory
                                                  "erc-image/"))
              (make-directory erc-image-images-path t)
              (add-hook 'rcirc-markup-text-functions
                        #'spacemacs//rcirc-image-show-url)))))

(defun rcirc/init-erc-tweet ()
  (spacemacs|use-package-add-hook rcirc
    :post-config
    (use-package erc-tweet
      :if rcirc-enable-erc-tweet
      :init (with-eval-after-load 'rcirc
              (setq erc-tweet-cache-dir (concat spacemacs-cache-directory
                                                "erc-tweet/"))
              (make-directory erc-tweet-cache-dir t)
              (add-hook 'rcirc-markup-text-functions
                        #'spacemacs//rcirc-tweet-show-tweet)))))

(defun rcirc/init-erc-yt ()
  (spacemacs|use-package-add-hook rcirc
    :post-config
    (use-package erc-yt
      :if rcirc-enable-erc-yt
      :init
      (with-eval-after-load 'rcirc
        (setq erc-yt-cache-dir (concat spacemacs-cache-directory "erc-yt/"))
        (make-directory erc-yt-cache-dir t)
        (add-hook 'rcirc-markup-text-functions
                  #'spacemacs//rcirc-youtube-show-info)))))

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
    (spacemacs/add-to-hook 'rcirc-mode-hook '(rcirc-omit-mode
                                              rcirc-track-minor-mode))

    (spacemacs/set-leader-keys "acir" 'spacemacs/rcirc)
    (spacemacs/declare-prefix "aci"  "irc")
    (evil-set-initial-state 'rcirc-mode 'insert)
    (setq rcirc-fill-column 80
          rcirc-buffer-maximum-lines 2048
          rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
          rcirc-time-format "%Y-%m-%d %H:%M "
          rcirc-omit-threshold 20
          rcirc-log-directory (concat spacemacs-cache-directory "/rcirc-logs/")
          rcirc-log-flag t)
    :config
    ;; (set-input-method "latin-1-prefix")
    (setq-local scroll-conservatively 8192)

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
    (require 'rcirc-color)
    (when rcirc-enable-styles
      (require 'rcirc-styles)
      (spacemacs/declare-prefix-for-mode 'rcirc-mode "mi" "insert")
      (spacemacs/set-leader-keys-for-major-mode 'rcirc-mode
        "ic" 'rcirc-styles-insert-color
        "ia" 'rcirc-styles-insert-attribute
        "ip" 'rcirc-styles-toggle-preview))))

(defun rcirc/init-rcirc-color ()
  (use-package rcirc-color :defer t))

(defun rcirc/init-rcirc-late-fix ()
  (spacemacs|use-package-add-hook rcirc
    :post-config
    (when rcirc-enable-late-fix
      (use-package rcirc-late-fix))))

(defun rcirc/init-rcirc-styles ()
  (use-package rcirc-styles))

(defun rcirc/init-rcirc-notify ()
  (use-package rcirc-notify
    :defer t
    :config
    (add-hook 'rcirc-notify-page-me-hooks 'spacemacs/rcirc-notify-beep)))

(defun rcirc/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :rcirc-layer
   (purpose-conf :mode-purposes '((rcirc-mode . chat)))))
