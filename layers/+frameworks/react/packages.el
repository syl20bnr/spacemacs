;;; packages.el --- react Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sylvain Benner
;; Copyright (c) 2014-2015 Andrea Moretti & Contributors
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

(defun react/pre-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    :post-config
    (progn
      (flycheck-add-mode 'javascript-eslint 'react-mode)
      (setq-default
       ;; disable jshint since we prefer eslint checking
       flycheck-disabled-checkers (append flycheck-disabled-checkers
                                          '(javascript-jshint))
       ;; disable json-jsonlist checking for json files
       flycheck-disabled-checkers (append flycheck-disabled-checkers
                                          '(json-jsonlist))))))

(defun react/post-init-flycheck ()
  (add-hook 'react-mode-hook 'flycheck-mode))

(defun react/post-init-js-doc ()
  (add-hook 'react-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'react-mode))

(defun react/post-init-js2-mode ()
  (add-hook 'react-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'react-mode-hook 'js2-minor-mode))

(defun react/post-init-js2-refactor ()
  (add-hook 'react-mode-hook 'spacemacs/js2-refactor-require)
  (spacemacs/js2-refactor-set-key-bindings 'react-mode))

(defun react/post-init-tern ()
  (add-hook 'react-mode-hook 'tern-mode))

(defun react/post-init-web-beautify ()
  (evil-leader/set-key-for-mode 'react-mode  "m=" 'web-beautify-js))

(defun react/post-init-web-mode ()
  (define-derived-mode react-mode web-mode "react")
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
  (add-to-list 'magic-mode-alist '("/** @jsx React.DOM */" . react-mode))
  (defun spacemacs//setup-react-mode ()
    "Adjust web-mode to accommodate react-mode"
    (emmet-mode 0)
    ;; Force jsx content type
    (web-mode-set-content-type "jsx")
    ;; Why do we do this ?
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (let ((web-mode-enable-part-face nil))
        ad-do-it)))
  (add-hook 'react-mode-hook 'spacemacs//setup-react-mode))
