;;; packages.el --- react Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq react-packages
      '(
        company-tern
        emmet-mode
        evil-matchit
        flycheck
        js-doc
        smartparens
        tern
        web-beautify
        web-mode
        ))

(defun react/post-init-company-tern ()
  (spacemacs|add-company-backends :backends company-tern :modes react-mode))

(defun react/post-init-emmet-mode ()
  (add-hook 'react-mode-hook 'emmet-mode))

(defun react/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'react-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump)))))

(defun react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'react-mode)))
  (add-hook 'react-mode-hook #'spacemacs//react-use-eslint-from-node-modules)
  (spacemacs/enable-flycheck 'react-mode))

(defun react/post-init-js-doc ()
  (add-hook 'react-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'react-mode))

(defun react/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'react-mode-hook #'smartparens-strict-mode)
    (add-hook 'react-mode-hook #'smartparens-mode)))

(defun react/post-init-tern ()
  (add-hook 'react-mode-hook 'tern-mode)
  (spacemacs//set-tern-key-bindings 'react-mode))

(defun react/post-init-web-beautify ()
  (spacemacs/set-leader-keys-for-major-mode 'react-mode  "=" 'web-beautify-js))

(defun react/post-init-web-mode ()
  (define-derived-mode react-mode web-mode "react")
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . react-mode))
  (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . react-mode))
  (add-to-list 'magic-mode-alist '("^import React" . react-mode))
  (add-hook 'react-mode-hook 'spacemacs//setup-react-mode))
