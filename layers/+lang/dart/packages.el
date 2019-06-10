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

;; TODO: offer lsp integration
(defconst dart-packages
  '(dart-mode
    flycheck))

(defun dart/show-buffer ()
  "Shows information at point on a new buffer"
  (interactive)
  (dart-show-hover 't))

(defun dart/init-dart-mode ()
  (use-package dart-mode
    :defer t
    :mode "\\.dart\\'"
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'dart-mode "mf" "find")
      (spacemacs/declare-prefix-for-mode 'dart-mode "mh" "help")
      (spacemacs/set-leader-keys-for-major-mode 'dart-mode
        "=" 'dart-format
        "?" 'dart-show-hover
        "g" 'dart-goto
        "hh" 'dart-show-hover
        "hb" 'dart/show-buffer
        "ff" 'dart-find-refs
        "fe" 'dart-find-member-decls
        "fr" 'dart-find-member-refs
        "fd" 'dart-find-top-level-decls)

      (add-to-list 'spacemacs-jump-handlers-dart-mode '(dart-goto :async t))

      (evil-define-key 'insert dart-mode-map
        (kbd "<tab>") 'dart-expand
        (kbd "C-<tab>") 'dart-expand-parameters)

      (evil-set-initial-state 'dart-popup-mode 'motion)
      (evil-define-key 'motion dart-popup-mode-map
        (kbd "gr") 'dart-do-it-again))))

(defun dart/post-init-flycheck ()
  (spacemacs/enable-flycheck 'dart-mode))

;;; packages.el ends here
