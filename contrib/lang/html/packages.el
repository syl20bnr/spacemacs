;;; packages.el --- HTTP Layer packages File for Spacemacs
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

(defvar html-packages
  '(
    emmet-mode
    evil-matchit
    flycheck
    helm-css-scss
    less-css-mode
    scss-mode
    tagedit
    web-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun html/init-helm-css-scss ()
  (use-package helm-css-scss
    :defer t
    :init
    (eval-after-load 'scss-mode
      '(evil-leader/set-key-for-mode 'scss-mode "mgh" 'helm-css-scss))))

(defun html/init-web-mode ()
  (use-package web-mode
    :defer t
    :mode (("\\.phtml\\'"      . web-mode)
           ("\\.tpl\\.php\\'"  . web-mode)
           ("\\.html\\'"       . web-mode)
           ("\\.htm\\'"        . web-mode)
           ("\\.[gj]sp\\'"     . web-mode)
           ("\\.as[cp]x\\'"    . web-mode)
           ("\\.erb\\'"        . web-mode)
           ("\\.mustache\\'"   . web-mode)
           ("\\.handlebars\\'" . web-mode)
           ("\\.hbs\\'"        . web-mode)
           ("\\.djhtml\\'"     . web-mode))))

(defun html/init-emmet-mode ()
  (use-package emmet-mode
    :defer t
    :init
    (progn
      (add-hook 'web-mode-hook 'emmet-mode)
      (add-hook 'html-mode-hook 'emmet-mode)
      (add-hook 'css-mode-hook 'emmet-mode))
    :config
    (progn
      (local-set-key (kbd "<tab>") 'emmet-expand-yas)
      (spacemacs|hide-lighter emmet-mode))))

(defun html/init-evil-matchit ()
  (add-hook 'web-mode-hook 'evil-matchit-mode))

(defun html/init-scss-mode ()
  (use-package scss-mode
    :defer t
    :mode ("\\.scss\\'" . scss-mode)))

(defun html/init-flycheck ()
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'scss-mode-hook 'flycheck-mode))

(defun html/init-tagedit ()
  (use-package tagedit
    :defer t
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
      (spacemacs|diminish tagedit-mode " Ⓣ" " T"))))
