;;; packages.el --- Language Server Protocol functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//lsp-sync-peek-face ()
  "Synchronize the face used in `lsp-ui' peek window according to the theme."
  (set-face-attribute 'lsp-ui-peek-list nil
                      :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-peek nil
                      :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-selection nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'default :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-filename nil
                      :foreground (face-attribute 'font-lock-constant-face
                                                  :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-highlight nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'highlight :foreground nil t)
                      :distant-foreground (face-attribute 'highlight
                                                          :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-header nil
                      :background (face-attribute 'highlight :background nil t)
                      :foreground (face-attribute 'default :foreground nil t))
  )

(defun spacemacs//setup-lsp-jump-handler (&rest modes)
  "Set jump handler for LSP with the given MODE."
  (dolist (m modes)
    (add-to-list (intern (format "spacemacs-jump-handlers-%S" m))
                 '(lsp-ui-peek-find-definitions))))
