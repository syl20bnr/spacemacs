;;; packages.el --- emoji Layer Packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq emoji-packages
      '(
        emoji-cheat-sheet-plus
        emojify
        (company-emoji :requires company)
        ))

(defun emoji/init-emoji-cheat-sheet-plus ()
  (use-package emoji-cheat-sheet-plus
    :commands (emoji-cheat-sheet-plus-insert
               emoji-cheat-sheet-plus-buffer
               emoji-cheat-sheet-plus-display-mode)
    :init
    (progn
      (spacemacs/set-leader-keys "afe" 'emoji-cheat-sheet-plus-buffer)
      (spacemacs/set-leader-keys "ie" 'emoji-cheat-sheet-plus-insert)
      (evilified-state-evilify emoji-cheat-sheet-plus-buffer-mode
        emoji-cheat-sheet-plus-buffer-mode-map
        "<RET>" 'emoji-cheat-sheet-plus-echo-and-copy))
    :config
    (spacemacs|hide-lighter emoji-cheat-sheet-plus-display-mode)))

(defun emoji/init-emojify ()
  (use-package emojify
    :defer t
    :init
    (setq emojify-emojis-dir (concat spacemacs-cache-directory "emojify/"))))

(defun emoji/init-company-emoji ()
  (use-package company-emoji
    :defer t
    :init
    (progn
      ;; For when Emacs is started in GUI mode:
      (spacemacs//set-emoji-font nil)
      ;; Hook for when a frame is created with emacsclient
      (spacemacs|do-after-display-system-init
       (spacemacs//set-emoji-font-for-current-frame)))))
