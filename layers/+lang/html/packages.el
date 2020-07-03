;;; packages.el --- HTML Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq html-packages
      '(
        add-node-modules-path
        company
        (company-web :requires company)
        css-mode
        emmet-mode
        evil-matchit
        flycheck
        haml-mode
        (counsel-css :requires ivy)
        (helm-css-scss :requires helm)
        impatient-mode
        less-css-mode
        prettier-js
        pug-mode
        sass-mode
        scss-mode
        slim-mode
        smartparens
        tagedit
        web-mode
        yasnippet
        web-beautify
        ))

(defun html/post-init-add-node-modules-path ()
  (add-hook 'css-mode-hook #'add-node-modules-path)
  (add-hook 'less-css-mode-hook #'add-node-modules-path)
  (add-hook 'pug-mode-hook #'add-node-modules-path)
  (add-hook 'sass-mode-hook #'add-node-modules-path)
  (add-hook 'scss-mode-hook #'add-node-modules-path)
  (add-hook 'slim-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'add-node-modules-path))

(defun html/post-init-company ()
  (unless css-enable-lsp
    (spacemacs|add-company-backends
      :backends company-css
      :modes css-mode)))

(defun html/init-company-web ()
  (use-package company-web
    :defer t
    :init
    (progn
      (spacemacs|add-company-backends
        :backends (company-web-html company-css)
        :modes web-mode)
      (spacemacs|add-company-backends
        :backends company-web-jade
        :modes pug-mode)
      (spacemacs|add-company-backends
        :backends company-web-slim
        :modes slim-mode))))

(defun html/init-css-mode ()
  (use-package css-mode
    :defer t
    :init
    (progn
      ;; Mark `css-indent-offset' as safe-local variable
      (put 'css-indent-offset 'safe-local-variable #'integerp)

      (when css-enable-lsp
        (add-hook 'css-mode-hook
                  #'spacemacs//setup-lsp-for-web-mode-buffers t))

      ;; Explicitly run prog-mode hooks since css-mode does not derive from
      ;; prog-mode major-mode in Emacs 24 and below.
      (when (version< emacs-version "25")
        (add-hook 'css-mode-hook 'spacemacs/run-prog-mode-hooks))

      (spacemacs/declare-prefix-for-mode 'css-mode "m=" "format")
      (spacemacs/declare-prefix-for-mode 'css-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'css-mode "mz" "foldz")

      (spacemacs/set-leader-keys-for-major-mode 'css-mode
        "zc" 'spacemacs/css-contract-statement
        "zo" 'spacemacs/css-expand-statement))))

(defun html/init-emmet-mode ()
  (use-package emmet-mode
    :defer t
    :init (spacemacs/add-to-hooks 'emmet-mode '(css-mode-hook
                                                html-mode-hook
                                                sass-mode-hook
                                                scss-mode-hook
                                                web-mode-hook))
    :config
    (progn
      (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'spacemacs/emmet-expand)
      (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'spacemacs/emmet-expand)
      (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") 'spacemacs/emmet-expand)
      (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") 'spacemacs/emmet-expand)
      (spacemacs|hide-lighter emmet-mode))))

(defun html/post-init-evil-matchit ()
  (add-hook 'web-mode-hook 'turn-on-evil-matchit-mode))

(defun html/post-init-flycheck ()
  (dolist (mode '(haml-mode
                  less-mode
                  pug-mode
                  sass-mode
                  scss-mode
                  slim-mode
                  web-mode))
    (spacemacs/enable-flycheck mode)))

(defun html/init-haml-mode ()
  (use-package haml-mode
    :defer t))

(defun html/init-counsel-css ()
  (use-package counsel-css
    :defer t
    :init (cl-loop for (mode . mode-hook) in '((css-mode . css-mode-hook)
                                               (scss-mode . scss-mode-hook))
                   do (add-hook mode-hook 'counsel-css-imenu-setup)
                   (spacemacs/set-leader-keys-for-major-mode mode "gh" 'counsel-css))))

(defun html/init-helm-css-scss ()
  (use-package helm-css-scss
    :defer t
    :init
    (dolist (mode '(css-mode scss-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "gh" 'helm-css-scss))))

(defun html/init-impatient-mode ()
  (use-package impatient-mode
    :defer t
    :init
    (progn
      (dolist (mode '(web-mode css-mode))
        (spacemacs/set-leader-keys-for-major-mode 'web-mode "I" 'spacemacs/impatient-mode)))))

(defun html/init-less-css-mode ()
  (use-package less-css-mode
    :defer t
    :init
    (when less-enable-lsp
      (add-hook 'less-css-mode-hook
                #'spacemacs//setup-lsp-for-web-mode-buffers t))
    :mode ("\\.less\\'" . less-css-mode)))

(defun html/pre-init-prettier-js ()
  (when (eq web-fmt-tool 'prettier)
    (dolist (mode '(css-mode less-css-mode scss-mode web-mode))
      (add-to-list 'spacemacs--prettier-modes mode))))

(defun html/init-pug-mode ()
  (use-package pug-mode
    :defer t
    :mode ("\\.pug$" . pug-mode)))

(defun html/init-sass-mode ()
  (use-package sass-mode
    :defer t
    :mode ("\\.sass\\'" . sass-mode)))

(defun html/init-scss-mode ()
  (use-package scss-mode
    :defer t
    :init
    (when scss-enable-lsp
      (add-hook 'scss-mode-hook #'spacemacs//setup-lsp-for-web-mode-buffers t))
    :mode ("\\.scss\\'" . scss-mode)))

(defun html/init-slim-mode ()
  (use-package slim-mode
    :defer t))

(defun html/post-init-smartparens ()
  (spacemacs/add-to-hooks
   (if dotspacemacs-smartparens-strict-mode
       'smartparens-strict-mode
     'smartparens-mode)
   '(css-mode-hook scss-mode-hook sass-mode-hook less-css-mode-hook))

  (add-hook 'web-mode-hook 'spacemacs/toggle-smartparens-off))

(defun html/init-tagedit ()
  (use-package tagedit
    :defer t
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
      (spacemacs|diminish tagedit-mode " Ⓣ" " T"))))

(defun html/init-web-mode ()
  (use-package web-mode
    :defer t
    :init
    (progn
      (spacemacs//web-setup-transient-state)
      (when html-enable-lsp
        (add-hook 'web-mode-hook #'spacemacs//setup-lsp-for-html-buffer t)))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'web-mode "m=" "format")
      (spacemacs/declare-prefix-for-mode 'web-mode "mE" "errors")
      (spacemacs/declare-prefix-for-mode 'web-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'web-mode "mh" "dom")
      (spacemacs/declare-prefix-for-mode 'web-mode "mr" "refactor")
      (spacemacs/set-leader-keys-for-major-mode 'web-mode
        "El" 'web-mode-dom-errors-show
        "gb" 'web-mode-element-beginning
        "gc" 'web-mode-element-child
        "gp" 'web-mode-element-parent
        "gs" 'web-mode-element-sibling-next
        "hp" 'web-mode-dom-xpath
        "rc" 'web-mode-element-clone
        "rd" 'web-mode-element-vanish
        "rk" 'web-mode-element-kill
        "rr" 'web-mode-element-rename
        "rw" 'web-mode-element-wrap
        "z" 'web-mode-fold-or-unfold
        ;; TODO element close would be nice but broken with evil.
        ))
    :mode
    (("\\.phtml\\'"      . web-mode)
     ("\\.tpl\\.php\\'"  . web-mode)
     ("\\.twig\\'"       . web-mode)
     ("\\.xml\\'"        . web-mode)
     ("\\.html\\'"       . web-mode)
     ("\\.htm\\'"        . web-mode)
     ("\\.[gj]sp\\'"     . web-mode)
     ("\\.as[cp]x?\\'"   . web-mode)
     ("\\.eex\\'"        . web-mode)
     ("\\.erb\\'"        . web-mode)
     ("\\.mustache\\'"   . web-mode)
     ("\\.handlebars\\'" . web-mode)
     ("\\.hbs\\'"        . web-mode)
     ("\\.eco\\'"        . web-mode)
     ("\\.ejs\\'"        . web-mode)
     ("\\.svelte\\'"     . web-mode)
     ("\\.ctp\\'"        . web-mode)
     ("\\.djhtml\\'"     . web-mode))))

(defun html/post-init-yasnippet ()
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(css-mode-hook
                                                      jade-mode
                                                      slim-mode)))
(defun html/pre-init-web-beautify ()
  (when (eq web-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes (cons 'css-mode 'web-beautify-css))
    (add-to-list 'spacemacs--web-beautify-modes (cons 'web-mode 'web-beautify-html))))
