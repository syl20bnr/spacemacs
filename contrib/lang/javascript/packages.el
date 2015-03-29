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

(defvar javascript-packages
  '(
    coffee-mode
    company-tern
    flycheck
    js2-mode
    js2-refactor
    json-mode
    tern
    tern-auto-complete
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

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
      ;; indent to right position after `evil-open-blow' and `evil-open-above'
      (add-hook 'coffee-mode-hook '(lambda ()
                                     (setq indent-line-function 'javascript/coffee-indent
                                           evil-shift-width coffee-tab-width))))))

(defun javascript/init-flycheck ()
  (add-hook 'coffee-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook    'flycheck-mode)
  (add-hook 'json-mode-hook   'flycheck-mode))

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
      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "m" "major mode")

      (evil-leader/set-key-for-mode 'js2-mode "mw" 'js2-mode-toggle-warnings-and-errors)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mz" "folding")
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
      (defun javascript/load-js2-refactor ()
        "Lazy load js2-refactor"
        (require 'js2-refactor))
      (add-hook 'js2-mode-hook 'javascript/load-js2-refactor))
    :config
    (progn
      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mr" "refactor")

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mr3" "ternary")
      (evil-leader/set-key-for-mode 'js2-mode "mr3i" 'js2r-ternary-to-if)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mra" "add/args")
      (evil-leader/set-key-for-mode 'js2-mode "mrag" 'js2r-add-to-globals-annotation)
      (evil-leader/set-key-for-mode 'js2-mode "mrao" 'js2r-arguments-to-object)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrb" "barf")
      (evil-leader/set-key-for-mode 'js2-mode "mrba" 'js2r-forward-barf)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrc" "contract")
      (evil-leader/set-key-for-mode 'js2-mode "mrca" 'js2r-contract-array)
      (evil-leader/set-key-for-mode 'js2-mode "mrco" 'js2r-contract-object)
      (evil-leader/set-key-for-mode 'js2-mode "mrcu" 'js2r-contract-function)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mre" "expand/extract")
      (evil-leader/set-key-for-mode 'js2-mode "mrea" 'js2r-expand-array)
      (evil-leader/set-key-for-mode 'js2-mode "mref" 'js2r-extract-function)
      (evil-leader/set-key-for-mode 'js2-mode "mrem" 'js2r-extract-method)
      (evil-leader/set-key-for-mode 'js2-mode "mreo" 'js2r-expand-object)
      (evil-leader/set-key-for-mode 'js2-mode "mreu" 'js2r-expand-function)
      (evil-leader/set-key-for-mode 'js2-mode "mrev" 'js2r-extract-var)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mri" "inline/inject/introduct")
      (evil-leader/set-key-for-mode 'js2-mode "mrig" 'js2r-inject-global-in-iife)
      (evil-leader/set-key-for-mode 'js2-mode "mrip" 'js2r-introduce-parameter)
      (evil-leader/set-key-for-mode 'js2-mode "mriv" 'js2r-inline-var)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrl" "localize/log")
      (evil-leader/set-key-for-mode 'js2-mode "mrlp" 'js2r-localize-parameter)
      (evil-leader/set-key-for-mode 'js2-mode "mrlt" 'js2r-log-this)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrr" "rename")
      (evil-leader/set-key-for-mode 'js2-mode "mrrv" 'js2r-rename-var)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrs" "split/slurp")
      (evil-leader/set-key-for-mode 'js2-mode "mrsl" 'js2r-forward-slurp)
      (evil-leader/set-key-for-mode 'js2-mode "mrss" 'js2r-split-string)
      (evil-leader/set-key-for-mode 'js2-mode "mrsv" 'js2r-split-var-declaration)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrt" "toggle")
      (evil-leader/set-key-for-mode 'js2-mode "mrtf" 'js2r-toggle-function-expression-and-declaration)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mru" "unwrap")
      (evil-leader/set-key-for-mode 'js2-mode "mruw" 'js2r-unwrap)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrv" "var")
      (evil-leader/set-key-for-mode 'js2-mode "mrvt" 'js2r-var-to-this)

      ;;(spacemacs/declare-prefix-for-mode 'js2-mode "mrw" "wrap")
      (evil-leader/set-key-for-mode 'js2-mode "mrwi" 'js2r-wrap-buffer-in-iife)
      (evil-leader/set-key-for-mode 'js2-mode "mrwl" 'js2r-wrap-in-for-loop)

      (evil-leader/set-key-for-mode 'js2-mode "mk" 'js2r-kill)
      (evil-leader/set-key-for-mode 'js2-mode "xmj" 'js2r-move-line-down)
      (evil-leader/set-key-for-mode 'js2-mode "xmk" 'js2r-move-line-up))))

(defun javascript/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun javascript/init-tern ()
  (use-package tern
    :defer t
    :init (add-hook 'js2-mode-hook 'tern-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'js2-mode "mc" 'tern-rename-variable)
      (evil-leader/set-key-for-mode 'js2-mode "mhd" 'tern-get-docs)
      (evil-leader/set-key-for-mode 'js2-mode "mgg" 'tern-find-definition)
      (evil-leader/set-key-for-mode 'js2-mode "mgG" 'tern-find-definition-by-name)
      (evil-leader/set-key-for-mode 'js2-mode (kbd "m C-g") 'tern-pop-find-definition)
      (evil-leader/set-key-for-mode 'js2-mode "mt" 'tern-get-type))))

(defun javascript/init-tern-auto-complete ()
  (use-package tern-auto-complete
    :if (and (configuration-layer/package-declaredp 'tern)
             (configuration-layer/package-declaredp 'auto-complete))
    :defer t
    :init (add-hook 'tern-mode-hook 'tern-ac-setup)))

(defun javascript/init-company-tern ()
  (use-package company-tern
    :if (and (configuration-layer/package-declaredp 'tern)
             (configuration-layer/package-declaredp 'company))
    :defer t
    :init
    (progn
      (spacemacs|add-mode-company-backend tern-mode company-tern))))
