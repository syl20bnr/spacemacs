;;; packages.el --- emoji Layer Packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
      (spacemacs/set-leader-keys "aE" 'emoji-cheat-sheet-plus-buffer)
      (spacemacs/set-leader-keys "ie" 'emoji-cheat-sheet-plus-insert)
      (evilified-state-evilify emoji-cheat-sheet-plus-buffer-mode
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
      :init
      (progn
        (setq company-emoji-insert-unicode nil)
        ;; From https://github.com/dunn/company-emoji/README.md for Linux, or on Mac OS X and using the Cocoa version of Emacs
        (defun --set-emoji-font (frame)
          "Adjust the font settings of FRAME so Emacs can display emoji properly.Only works in GUI mode"
          (when (display-graphic-p)
            (cond
             ;; For NS/Cocoa
             ((spacemacs/system-is-mac)
              (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend))
             ;; For Linux
             ((spacemacs/system-is-linux)
              (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))))

        (--set-emoji-font nil)
        ;; Hook for when a frame is created with emacsclient
        ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
        (add-hook 'after-make-frame-functions '--set-emoji-font)
        ))))
