;;; packages.el --- dart layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Bruno Tavares <connect+spacemacs@bltavares.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst dart-packages
  '(
    dart-mode
    dart-server
    company
    flutter
    flycheck
    ))

(defun dart/show-buffer ()
  "Shows information at point in a new buffer"
  (interactive)
  (dart-server-show-hover t))

(defun dart/init-dart-mode ())

(defun dart/init-dart-server ()
  (use-package dart-server
    :defer t
    :mode "\\.dart\\'"
    :init
    (progn
      (add-hook 'dart-mode-local-vars-hook
                #'spacemacs//dart-setup-backend-lsp)
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
        ;; "ff" 'dart-server-find-refs
        "fe" 'dart-server-find-member-decls
        "fr" 'dart-server-find-member-refs
        "fd" 'dart-server-find-top-level-decls)

      (add-to-list 'spacemacs-jump-handlers-dart-server
                   '(dart-server-goto :async t))

      (evil-define-key 'insert dart-server-map
        (kbd "<tab>") 'dart-server-expand
        (kbd "C-<tab>") 'dart-server-expand-parameters)

      (evil-set-initial-state 'dart-server-popup-mode 'motion)
      (evil-define-key 'motion dart-server-popup-mode-map
        (kbd "gr") 'dart-server-do-it-again))
    :config (dart-mode)))

(defun dart/init-flutter ()
  (use-package flutter
    :defer t
    :after dart-mode
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'dart-mode "mx" "flutter")
      (spacemacs/set-leader-keys-for-major-mode 'dart-mode
        "xx" 'flutter-run-or-hot-reload))))

(defun dart/post-init-company ()
  (add-hook 'dart-mode-local-vars-hook #'spacemacs//dart-setup-company-lsp))

(defun dart/post-init-flycheck ()
  (spacemacs/enable-flycheck 'dart-mode))

;;; packages.el ends here
