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
    yasnippet
    haml-mode
    slim-mode
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
    :config
    (progn

      (defun web-mode-micro-state-doc ()
        "Full documentation for web mode micro-state."
        "
  [?] display this help
  [h] previous [l] next   [L] sibling [k] parent [j] child
  [c] clone    [d] delete [r] rename  [w] wrap   [p] xpath
  [q] quit")

      (evil-leader/set-key-for-mode 'web-mode
        "meh" 'web-mode-dom-errors-show
        "mgb" 'web-mode-element-beginning
        "mgc" 'web-mode-element-child
        "mgp" 'web-mode-element-parent
        "mgs" 'web-mode-element-sibling-next
        "mhp" 'web-mode-dom-xpath
        "mrc" 'web-mode-element-clone
        "mrd" 'web-mode-element-vanish
        "mrr" 'web-mode-element-rename
        "mrw" 'web-mode-element-wrap
        "mz" 'web-mode-fold-or-unfold
        ;; TODO element close would be nice but broken with evil.
        )

      (spacemacs|define-micro-state web-mode
        :doc "[?] for help"
        :persistent t
        :bindings
        ("?" nil :doc (web-mode-micro-state-doc))
        ("h" web-mode-element-previous)
        ("l" web-mode-element-next)
        ("L" web-mode-element-sibling-next)
        ("k" web-mode-element-parent)
        ("j" web-mode-element-child)
        ("p" web-mode-dom-xpath)
        ("r" web-mode-element-rename)
        ("d" web-mode-element-vanish)
        ("w" web-mode-element-wrap)
        ("c" web-mode-element-clone)
        ("q" nil :exit t))
      )
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

(defun html/init-yasnippet ()
  (use-package yasnippet
    :defer t
    :init
    (add-hook 'css-mode-hook 'spacemacs/load-yasnippet)))

(defun html/init-haml-mode ()
  (use-package haml-mode
    :defer t))

(defun html/init-slim-mode ()
  (use-package slim-mode
    :defer t))
