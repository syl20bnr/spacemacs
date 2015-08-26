;;; packages.el --- Javascript Layer packages File for Spacemacs
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

(setq javascript-packages
  '(
    coffee-mode
    company
    company-tern
    flycheck
    js-doc
    js2-mode
    js2-refactor
    json-mode
    json-snatcher
    tern
    web-beautify
    ))

(defun javascript/init-coffee-mode ()
  (use-package coffee-mode
    :defer t
    :init
    (progn
      (defun javascript/coffee-indent ()
        (if (coffee-line-wants-indent)
            ;; We need to insert an additional tab because the last line was special.
            (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
          ;; otherwise keep at the same indentation level
          (coffee-insert-spaces (coffee-previous-indent)))
        )
      ;; indent to right position after `evil-open-below' and `evil-open-above'
      (add-hook 'coffee-mode-hook '(lambda ()
                                     (setq indent-line-function 'javascript/coffee-indent
                                           evil-shift-width coffee-tab-width))))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun javascript/post-init-company ()
    (spacemacs|add-company-hook js2-mode))

  (defun javascript/init-company-tern ()
    (use-package company-tern
      :if (and (configuration-layer/package-usedp 'company)
               (configuration-layer/package-usedp 'tern))
      :defer t
      :init
      (push 'company-tern company-backends-js2-mode))))

(defun javascript/post-init-flycheck ()
  (add-hook 'coffee-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook    'flycheck-mode)
  (add-hook 'json-mode-hook   'flycheck-mode))

(defun javascript/init-js-doc ()
  (use-package js-doc
    :defer t
    :init
    (progn
      (defun spacemacs/js-doc-require ()
        "Lazy load js-doc"
        (require 'js-doc))
      (add-hook 'js2-mode-hook 'spacemacs/js-doc-require)

      (defun spacemacs/js-doc-set-key-bindings (mode)
        "Setup the key bindings for `js2-doc' for the given MODE."
        (evil-leader/set-key-for-mode mode "mrdb" 'js-doc-insert-file-doc)
        (evil-leader/set-key-for-mode mode "mrdf" 'js-doc-insert-function-doc)
        (evil-leader/set-key-for-mode mode "mrdt" 'js-doc-insert-tag)
        (evil-leader/set-key-for-mode mode "mrdh" 'js-doc-describe-tag))
      (spacemacs/js-doc-set-key-bindings 'js2-mode))))

(defun javascript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      ;; required to make `<SPC> s l' to work correctly
      (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
    :config
    (progn
      (evil-leader/set-key-for-mode 'js2-mode "mw" 'js2-mode-toggle-warnings-and-errors)

      (spacemacs/declare-prefix-for-mode 'js2-mode "mz" "folding")
      (evil-leader/set-key-for-mode 'js2-mode "mzc" 'js2-mode-hide-element)
      (evil-leader/set-key-for-mode 'js2-mode "mzo" 'js2-mode-show-element)
      (evil-leader/set-key-for-mode 'js2-mode "mzr" 'js2-mode-show-all)
      (evil-leader/set-key-for-mode 'js2-mode "mze" 'js2-mode-toggle-element)
      (evil-leader/set-key-for-mode 'js2-mode "mzF" 'js2-mode-toggle-hide-functions)
      (evil-leader/set-key-for-mode 'js2-mode "mzC" 'js2-mode-toggle-hide-comments))))

(defun javascript/init-js2-refactor ()
  (use-package js2-refactor
    :defer t
    :init
    (progn
      (defun spacemacs/js2-refactor-require ()
        "Lazy load js2-refactor"
        (require 'js2-refactor))
      (add-hook 'js2-mode-hook 'spacemacs/js2-refactor-require)

      (defun spacemacs/js2-refactor-set-key-bindings (mode)
        (spacemacs/declare-prefix-for-mode mode "mr3" "ternary")
        (evil-leader/set-key-for-mode mode "mr3i" 'js2r-ternary-to-if)

        (spacemacs/declare-prefix-for-mode mode "mra" "add/args")
        (evil-leader/set-key-for-mode mode "mrag" 'js2r-add-to-globals-annotation)
        (evil-leader/set-key-for-mode mode "mrao" 'js2r-arguments-to-object)

        (spacemacs/declare-prefix-for-mode mode "mrb" "barf")
        (evil-leader/set-key-for-mode mode "mrba" 'js2r-forward-barf)

        (spacemacs/declare-prefix-for-mode mode "mrc" "contract")
        (evil-leader/set-key-for-mode mode "mrca" 'js2r-contract-array)
        (evil-leader/set-key-for-mode mode "mrco" 'js2r-contract-object)
        (evil-leader/set-key-for-mode mode "mrcu" 'js2r-contract-function)

        (spacemacs/declare-prefix-for-mode mode "mre" "expand/extract")
        (evil-leader/set-key-for-mode mode "mrea" 'js2r-expand-array)
        (evil-leader/set-key-for-mode mode "mref" 'js2r-extract-function)
        (evil-leader/set-key-for-mode mode "mrem" 'js2r-extract-method)
        (evil-leader/set-key-for-mode mode "mreo" 'js2r-expand-object)
        (evil-leader/set-key-for-mode mode "mreu" 'js2r-expand-function)
        (evil-leader/set-key-for-mode mode "mrev" 'js2r-extract-var)

        (spacemacs/declare-prefix-for-mode mode "mri" "inline/inject/introduct")
        (evil-leader/set-key-for-mode mode "mrig" 'js2r-inject-global-in-iife)
        (evil-leader/set-key-for-mode mode "mrip" 'js2r-introduce-parameter)
        (evil-leader/set-key-for-mode mode "mriv" 'js2r-inline-var)

        (spacemacs/declare-prefix-for-mode mode "mrl" "localize/log")
        (evil-leader/set-key-for-mode mode "mrlp" 'js2r-localize-parameter)
        (evil-leader/set-key-for-mode mode "mrlt" 'js2r-log-this)

        (spacemacs/declare-prefix-for-mode mode "mrr" "rename")
        (evil-leader/set-key-for-mode mode "mrrv" 'js2r-rename-var)

        (spacemacs/declare-prefix-for-mode mode "mrs" "split/slurp")
        (evil-leader/set-key-for-mode mode "mrsl" 'js2r-forward-slurp)
        (evil-leader/set-key-for-mode mode "mrss" 'js2r-split-string)
        (evil-leader/set-key-for-mode mode "mrsv" 'js2r-split-var-declaration)

        (spacemacs/declare-prefix-for-mode mode "mrt" "toggle")
        (evil-leader/set-key-for-mode mode "mrtf" 'js2r-toggle-function-expression-and-declaration)

        (spacemacs/declare-prefix-for-mode mode "mru" "unwrap")
        (evil-leader/set-key-for-mode mode "mruw" 'js2r-unwrap)

        (spacemacs/declare-prefix-for-mode mode "mrv" "var")
        (evil-leader/set-key-for-mode mode "mrvt" 'js2r-var-to-this)

        (spacemacs/declare-prefix-for-mode mode "mrw" "wrap")
        (evil-leader/set-key-for-mode mode "mrwi" 'js2r-wrap-buffer-in-iife)
        (evil-leader/set-key-for-mode mode "mrwl" 'js2r-wrap-in-for-loop)

        (evil-leader/set-key-for-mode mode "mk" 'js2r-kill)
        (evil-leader/set-key-for-mode mode "xmj" 'js2r-move-line-down)
        (evil-leader/set-key-for-mode mode "xmk" 'js2r-move-line-up))

      (spacemacs/js2-refactor-set-key-bindings 'js2-mode))))

(defun javascript/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun javascript/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (evil-leader/set-key-for-mode 'json-mode
      "mhp" 'jsons-print-path)
    ))

(defun javascript/init-tern ()
  (use-package tern
    :defer t
    :init (add-hook 'js2-mode-hook 'tern-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'js2-mode "mrrV" 'tern-rename-variable)
      (evil-leader/set-key-for-mode 'js2-mode "mhd" 'tern-get-docs)
      (evil-leader/set-key-for-mode 'js2-mode "mgg" 'tern-find-definition)
      (evil-leader/set-key-for-mode 'js2-mode "mgG" 'tern-find-definition-by-name)
      (evil-leader/set-key-for-mode 'js2-mode (kbd "m C-g") 'tern-pop-find-definition)
      (evil-leader/set-key-for-mode 'js2-mode "mht" 'tern-get-type))))

(defun javascript/init-web-beautify ()
  (use-package web-beautify
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'js2-mode  "m=" 'web-beautify-js)
      (evil-leader/set-key-for-mode 'json-mode "m=" 'web-beautify-js)
      (evil-leader/set-key-for-mode 'web-mode  "m=" 'web-beautify-html)
      (evil-leader/set-key-for-mode 'css-mode  "m=" 'web-beautify-css))))
