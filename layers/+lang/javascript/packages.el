;;; packages.el --- Javascript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    evil-matchit
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
  (dolist (hook '(coffee-mode-hook js2-mode-hook json-mode-hook))
    (spacemacs/add-flycheck-hook hook)))

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
        (spacemacs/set-leader-keys-for-major-mode mode "rdb" 'js-doc-insert-file-doc)
        (spacemacs/set-leader-keys-for-major-mode mode "rdf" 'js-doc-insert-function-doc)
        (spacemacs/set-leader-keys-for-major-mode mode "rdt" 'js-doc-insert-tag)
        (spacemacs/set-leader-keys-for-major-mode mode "rdh" 'js-doc-describe-tag))
      (spacemacs/js-doc-set-key-bindings 'js2-mode))))

(defun javascript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      ;; Required to make imenu functions work correctly
      (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "w" 'js2-mode-toggle-warnings-and-errors)

      (spacemacs/declare-prefix-for-mode 'js2-mode "mz" "folding")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "zc" 'js2-mode-hide-element)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "zo" 'js2-mode-show-element)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "zr" 'js2-mode-show-all)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "ze" 'js2-mode-toggle-element)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "zF" 'js2-mode-toggle-hide-functions)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "zC" 'js2-mode-toggle-hide-comments))))

(defun javascript/post-init-evil-matchit ()
  (add-hook `js2-mode `turn-on-evil-matchit-mode))

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
        (spacemacs/set-leader-keys-for-major-mode mode "r3i" 'js2r-ternary-to-if)

        (spacemacs/declare-prefix-for-mode mode "mra" "add/args")
        (spacemacs/set-leader-keys-for-major-mode mode "rag" 'js2r-add-to-globals-annotation)
        (spacemacs/set-leader-keys-for-major-mode mode "rao" 'js2r-arguments-to-object)

        (spacemacs/declare-prefix-for-mode mode "mrb" "barf")
        (spacemacs/set-leader-keys-for-major-mode mode "rba" 'js2r-forward-barf)

        (spacemacs/declare-prefix-for-mode mode "mrc" "contract")
        (spacemacs/set-leader-keys-for-major-mode mode "rca" 'js2r-contract-array)
        (spacemacs/set-leader-keys-for-major-mode mode "rco" 'js2r-contract-object)
        (spacemacs/set-leader-keys-for-major-mode mode "rcu" 'js2r-contract-function)

        (spacemacs/declare-prefix-for-mode mode "mre" "expand/extract")
        (spacemacs/set-leader-keys-for-major-mode mode "rea" 'js2r-expand-array)
        (spacemacs/set-leader-keys-for-major-mode mode "ref" 'js2r-extract-function)
        (spacemacs/set-leader-keys-for-major-mode mode "rem" 'js2r-extract-method)
        (spacemacs/set-leader-keys-for-major-mode mode "reo" 'js2r-expand-object)
        (spacemacs/set-leader-keys-for-major-mode mode "reu" 'js2r-expand-function)
        (spacemacs/set-leader-keys-for-major-mode mode "rev" 'js2r-extract-var)

        (spacemacs/declare-prefix-for-mode mode "mri" "inline/inject/introduct")
        (spacemacs/set-leader-keys-for-major-mode mode "rig" 'js2r-inject-global-in-iife)
        (spacemacs/set-leader-keys-for-major-mode mode "rip" 'js2r-introduce-parameter)
        (spacemacs/set-leader-keys-for-major-mode mode "riv" 'js2r-inline-var)

        (spacemacs/declare-prefix-for-mode mode "mrl" "localize/log")
        (spacemacs/set-leader-keys-for-major-mode mode "rlp" 'js2r-localize-parameter)
        (spacemacs/set-leader-keys-for-major-mode mode "rlt" 'js2r-log-this)

        (spacemacs/declare-prefix-for-mode mode "mrr" "rename")
        (spacemacs/set-leader-keys-for-major-mode mode "rrv" 'js2r-rename-var)

        (spacemacs/declare-prefix-for-mode mode "mrs" "split/slurp")
        (spacemacs/set-leader-keys-for-major-mode mode "rsl" 'js2r-forward-slurp)
        (spacemacs/set-leader-keys-for-major-mode mode "rss" 'js2r-split-string)
        (spacemacs/set-leader-keys-for-major-mode mode "rsv" 'js2r-split-var-declaration)

        (spacemacs/declare-prefix-for-mode mode "mrt" "toggle")
        (spacemacs/set-leader-keys-for-major-mode mode "rtf" 'js2r-toggle-function-expression-and-declaration)

        (spacemacs/declare-prefix-for-mode mode "mru" "unwrap")
        (spacemacs/set-leader-keys-for-major-mode mode "ruw" 'js2r-unwrap)

        (spacemacs/declare-prefix-for-mode mode "mrv" "var")
        (spacemacs/set-leader-keys-for-major-mode mode "rvt" 'js2r-var-to-this)

        (spacemacs/declare-prefix-for-mode mode "mrw" "wrap")
        (spacemacs/set-leader-keys-for-major-mode mode "rwi" 'js2r-wrap-buffer-in-iife)
        (spacemacs/set-leader-keys-for-major-mode mode "rwl" 'js2r-wrap-in-for-loop)

        (spacemacs/set-leader-keys-for-major-mode mode "k" 'js2r-kill)

        (spacemacs/declare-prefix-for-mode mode "mx" "text")
        (spacemacs/declare-prefix-for-mode mode "mxm" "move")
        (spacemacs/set-leader-keys-for-major-mode mode "xmj" 'js2r-move-line-down)
        (spacemacs/set-leader-keys-for-major-mode mode "xmk" 'js2r-move-line-up))

      (spacemacs/js2-refactor-set-key-bindings 'js2-mode))))

(defun javascript/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun javascript/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'json-mode
      "hp" 'jsons-print-path)
    ))

(defun javascript/init-tern ()
  (use-package tern
    :defer t
    :if (javascript//tern-detect)
    :init (add-hook 'js2-mode-hook 'tern-mode)
    :config
    (progn
      (when javascript-disable-tern-port-files
        (add-to-list 'tern-command "--no-port-file" 'append))
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "rrV" 'tern-rename-variable)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "hd" 'tern-get-docs)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "gg" 'tern-find-definition)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "gG" 'tern-find-definition-by-name)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode (kbd " C-g") 'tern-pop-find-definition)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode "ht" 'tern-get-type))))

(defun javascript/init-web-beautify ()
  (use-package web-beautify
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode  "=" 'web-beautify-js)
      (spacemacs/set-leader-keys-for-major-mode 'json-mode "=" 'web-beautify-js)
      (spacemacs/set-leader-keys-for-major-mode 'web-mode  "=" 'web-beautify-html)
      (spacemacs/set-leader-keys-for-major-mode 'css-mode  "=" 'web-beautify-css))))
