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

(setq html-packages
  '(
    company
    company-web
    css-mode
    emmet-mode
    evil-matchit
    flycheck
    helm-css-scss
    less-css-mode
    rainbow-delimiters
    scss-mode
    sass-mode
    tagedit
    web-mode
    yasnippet
    haml-mode
    slim-mode
    ))

(defun html/init-css-mode ()
  (use-package css-mode
    :defer t
    :init
    (push 'company-css company-backends-css-mode)))

(defun html/init-helm-css-scss ()
  (use-package helm-css-scss
    :defer t
    :init
    (eval-after-load 'scss-mode
      '(evil-leader/set-key-for-mode 'scss-mode "mgh" 'helm-css-scss))))

(defun html/init-web-mode ()
  (use-package web-mode
    :defer t
    :init
    (push 'company-web-html company-backends-web-mode)
    :config
    (progn
      ;; Only use smartparens in web-mode
      (setq web-mode-enable-auto-pairing nil)

      (sp-local-pair 'web-mode "<% " " %>")
      (sp-local-pair 'web-mode "{ " " }")
      (sp-local-pair 'web-mode "<%= "  "  %>")
      (sp-local-pair 'web-mode "<%# "  " %>")
      (sp-local-pair 'web-mode "<%$ "  " %>")
      (sp-local-pair 'web-mode "<%@ "  " %>")
      (sp-local-pair 'web-mode "<%: "  " %>")
      (sp-local-pair 'web-mode "{{ "  " }}")
      (sp-local-pair 'web-mode "{% "  " %}")
      (sp-local-pair 'web-mode "{%- "  " %}")
      (sp-local-pair 'web-mode "{# "  " #}")


      (evil-leader/set-key-for-mode 'web-mode
        "meh" 'web-mode-dom-errors-show
        "mgb" 'web-mode-element-beginning
        "mgc" 'web-mode-element-child
        "mgp" 'web-mode-element-parent
        "mgs" 'web-mode-element-sibling-next
        "mhp" 'web-mode-dom-xpath
        "mrc" 'web-mode-element-clone
        "mrd" 'web-mode-element-vanish
        "mrk" 'web-mode-element-kill
        "mrr" 'web-mode-element-rename
        "mrw" 'web-mode-element-wrap
        "mz" 'web-mode-fold-or-unfold
        ;; TODO element close would be nice but broken with evil.
        )

      (defvar spacemacs--web-mode-ms-doc-toggle 0
        "Display a short doc when nil, full doc otherwise.")

      (defun spacemacs//web-mode-ms-doc ()
        (if (equal 0 spacemacs--web-mode-ms-doc-toggle)
            "[?] for help"
          "
  [?] display this help
  [k] previous [j] next   [K] previous sibling [J] next sibling
  [h] parent   [l] child  [c] clone [d] delete [D] kill [r] rename
  [w] wrap     [p] xpath
  [q] quit"))

      (defun spacemacs//web-mode-ms-toggle-doc ()
        (interactive)
        (setq spacemacs--web-mode-ms-doc-toggle
              (logxor spacemacs--web-mode-ms-doc-toggle 1)))

      (spacemacs|define-micro-state web-mode
        :doc (spacemacs//web-mode-ms-doc)
        :persistent t
        :evil-leader-for-mode (web-mode . "m.")
        :bindings
        ("<escape>" nil :exit t)
        ("?" spacemacs//web-mode-ms-toggle-doc)
        ("c" web-mode-element-clone)
        ("d" web-mode-element-vanish)
        ("D" web-mode-element-kill)
        ("j" web-mode-element-next)
        ("J" web-mode-element-sibling-next)
        ("k" web-mode-element-previous)
        ("K" web-mode-element-sibling-previous)
        ("h" web-mode-element-parent)
        ("l" web-mode-element-child)
        ("p" web-mode-dom-xpath)
        ("r" web-mode-element-rename)
        ("q" nil :exit t)
        ("w" web-mode-element-wrap)))

    :mode
    (("\\.phtml\\'"      . web-mode)
     ("\\.tpl\\.php\\'"  . web-mode)
     ("\\.html\\'"       . web-mode)
     ("\\.htm\\'"        . web-mode)
     ("\\.[gj]sp\\'"     . web-mode)
     ("\\.as[cp]x\\'"    . web-mode)
     ("\\.erb\\'"        . web-mode)
     ("\\.mustache\\'"   . web-mode)
     ("\\.handlebars\\'" . web-mode)
     ("\\.hbs\\'"        . web-mode)
     ("\\.eco\\'"        . web-mode)
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
      (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
      (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
      (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
      (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
      (spacemacs|hide-lighter emmet-mode))))

(defun html/post-init-evil-matchit ()
  (add-hook 'web-mode-hook 'evil-matchit-mode))

(defun html/init-scss-mode ()
  (use-package scss-mode
    :defer t
    :mode ("\\.scss\\'" . scss-mode)))

(defun html/init-sass-mode ()
  (use-package sass-mode
    :defer t
    :mode ("\\.sass\\'" . sass-mode)))

(defun html/init-less-css-mode ()
  (use-package less-css-mode
    :defer t
    :mode ("\\.less\\'" . less-css-mode)))

(defun html/post-init-flycheck ()
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'scss-mode-hook 'flycheck-mode)
  (add-hook 'sass-mode-hook 'flycheck-mode))

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

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun html/post-init-company ()
    (spacemacs|add-company-hook css-mode)
    (spacemacs|add-company-hook web-mode))

  (defun html/init-company-web ()
    (use-package company-web)))

(defun html/init-rainbow-delimiters ()
  (when (configuration-layer/package-usedp 'less-css-mode)
    (add-hook 'less-css-mode-hook 'rainbow-delimiters-mode))
  (when (configuration-layer/package-usedp 'scss-mode)
    (add-hook 'scss-mode-hook 'rainbow-delimiters-mode)))
