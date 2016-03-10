;;; packages.el --- react Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq react-packages
      '(
        company
        company-tern
        evil-matchit
        flycheck
        js-doc
        js2-mode
        js2-refactor
        tern
        web-beautify
        web-mode
        ))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun react/post-init-company ()
    (spacemacs|add-company-hook react-mode))

  (defun react/post-init-company-tern ()
    (push 'company-tern company-backends-react-mode)))

(defun react/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'react-mode '((evilmi-simple-get-tag evilmi-simple-jump)
                                            (evilmi-javascript-get-tag evilmi-javascript-jump)
                                            (evilmi-html-get-tag evilmi-html-jump)))))

(defun react/pre-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    :post-config
    (progn
      (flycheck-add-mode 'javascript-eslint 'react-mode)

      (defun react/disable-jshint ()
        (push 'javascript-jshint flycheck-disabled-checkers))

      (add-hook 'react-mode-hook #'react/disable-jshint))))

(defun react/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'react-mode-hook))

(defun react/post-init-js-doc ()
  (add-hook 'react-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'react-mode))

(defun react//setup-imenu ()
  (setq imenu-create-index-function #'js2-mode-create-imenu-index))

(defun react/post-init-js2-mode ()
  (add-hook 'react-mode-hook 'react//setup-imenu)
  (add-hook 'react-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'react-mode-hook 'js2-minor-mode))

(defun react/post-init-js2-refactor ()
  (add-hook 'react-mode-hook 'spacemacs/js2-refactor-require)
  (spacemacs/js2-refactor-set-key-bindings 'react-mode))

(defun react/post-init-tern ()
  (add-hook 'react-mode-hook 'tern-mode))

(defun react/post-init-web-beautify ()
  (spacemacs/set-leader-keys-for-major-mode 'react-mode  "=" 'web-beautify-js))

(defun react/post-init-web-mode ()
  (define-derived-mode react-mode web-mode "react")
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . react-mode))
  (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . react-mode))
  (defun spacemacs//setup-react-mode ()
    "Adjust web-mode to accommodate react-mode"
    (emmet-mode 0)
    ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
    (setq-local emmet-expand-jsx-className? t)
    ;; Enable js-mode snippets
    (yas-activate-extra-mode 'js-mode)
    ;; Force jsx content type
    (web-mode-set-content-type "jsx")
    ;; Don't auto-quote attribute values
    (setq-local web-mode-enable-auto-quoting nil)
    ;; Why do we do this ?
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (let ((web-mode-enable-part-face nil))
        ad-do-it)))
  (add-hook 'react-mode-hook 'spacemacs//setup-react-mode))
