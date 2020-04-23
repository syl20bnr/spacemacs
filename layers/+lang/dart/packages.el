;;; packages.el --- dart layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Bruno Tavares <connect+spacemacs@bltavares.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst dart-packages
  '(
    dart-mode
    (dart-server :toggle (eq dart-backend 'analyzer))
    (flutter (eq dart-backend 'analyzer))
    flycheck
    (lsp-dart (eq dart-backend 'lsp))
    ))

(defun dart/show-buffer ()
  "Shows information at point in a new buffer"
  (interactive)
  (dart-server-show-hover t))

(defun dart/init-dart-mode ()
  (use-package dart-mode
    :defer t
    :mode "\\.dart\\'"
    :init
    (add-hook 'dart-mode-local-vars-hook
              #'spacemacs//dart-setup-backend)))

(defun dart/init-dart-server ()
  (use-package dart-server
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'dart-mode "mf" "find")
      (spacemacs/declare-prefix-for-mode 'dart-mode "mh" "help")
      (spacemacs/set-leader-keys-for-major-mode 'dart-mode
        "=" 'dart-server-format
        "?" 'dart-server-show-hover
        "g" 'dart-server-goto
        "hh" 'dart-server-show-hover
        "hb" 'dart/show-buffer
        ;; There's an upstream issue with this command:
        ;; dart-server-find-refs on int opens a dart buffer that keeps growing in size #11
        ;; https://github.com/bradyt/dart-server/issues/11
        ;; When/if it's fixed, add back the key binding:
        ;; ~SPC m f f~ Find reference at point.
        ;; to the readme.org key binding table.
        ;; "ff" 'dart-server-find-refs
        "fe" 'dart-server-find-member-decls
        "fr" 'dart-server-find-member-refs
        "fd" 'dart-server-find-top-level-decls)

      (add-to-list 'spacemacs-jump-handlers-dart-mode
                   '(dart-server-goto :async t))

      (evil-define-key 'insert dart-server-map
        (kbd "<tab>") 'dart-server-expand
        (kbd "C-<tab>") 'dart-server-expand-parameters)

      (evil-set-initial-state 'dart-server-popup-mode 'motion)
      (evil-define-key 'motion dart-server-popup-mode-map
        (kbd "gr") 'dart-server-do-it-again))))

(defun dart/init-flutter ()
  (use-package flutter
    :defer t
    :after dart-mode
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'dart-mode "mx" "flutter")
      (spacemacs/set-leader-keys-for-major-mode 'dart-mode
        "xx" 'flutter-run-or-hot-reload))))

(defun dart/init-lsp-dart ()
  (use-package lsp-dart
    :defer t))

(defun dart/post-init-flycheck ()
  (spacemacs/enable-flycheck 'dart-mode))
