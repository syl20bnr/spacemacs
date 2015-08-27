;;; packages.el --- emoji Layer Packages File for Spacemacs
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

(setq emoji-packages
      '(emoji-cheat-sheet-plus
        company-emoji))

(defun emoji/init-emoji-cheat-sheet-plus ()
  (use-package emoji-cheat-sheet-plus
    :commands (emoji-cheat-sheet-plus-insert
               emoji-cheat-sheet-plus-buffer
               emoji-cheat-sheet-plus-display-mode)
    :init
    (progn
      (evil-leader/set-key "aE" 'emoji-cheat-sheet-plus-buffer)
      (evil-leader/set-key "ie" 'emoji-cheat-sheet-plus-insert)
      (evilify emoji-cheat-sheet-plus-buffer-mode
               emoji-cheat-sheet-plus-buffer-mode-map
               "<RET>" 'emoji-cheat-sheet-plus-echo-and-copy)
      (defun spacemacs/delay-emoji-cheat-sheet-hook ()
        "Work-around for org buffers."
        ;; we need to wait for org buffer to be fully loaded before
        ;; calling the emoji mode.
        ;; If we directly call the emoji mode at hook runtime then some
        ;; text properties are not applied correctly.
        (run-at-time 0.1 nil 'emoji-cheat-sheet-plus-display-mode)))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun emoji/init-company-emoji ()
    (use-package company-emoji
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init (setq company-emoji-insert-unicode nil))))
