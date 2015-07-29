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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq react-packages
      '(
        web-mode
        js2-mode
        js2-refactor
        js-doc
        json-mode
        json-snatcher
        flycheck
        tern
        company
        company-tern
        web-beautify
        ))

;; List of packages to exclude.
(setq react-excluded-packages '())

;; For each package, define a function react/init-<package-name>
;;
(defun react/post-init-flycheck ()
  (add-hook 'react-mode-hook
            (lambda ()
              (with-eval-after-load 'flycheck
                ;; use eslint with web-mode for jsx files
                (flycheck-add-mode 'javascript-eslint 'react-mode)

                ;; disable jshint since we prefer eslint checking
                (setq-default flycheck-disabled-checkers
                              (append flycheck-disabled-checkers
                                      '(javascript-jshint)))

                ;; disable json-jsonlist checking for json files
                (setq-default flycheck-disabled-checkers
                              (append flycheck-disabled-checkers
                                      '(json-jsonlist)))))))

(defun react/post-init-web-mode ()
  (define-derived-mode react-mode web-mode "react")
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . react-mode))
  (add-to-list 'auto-mode-alist '("\\.react.js\\'" . react-mode))
  (add-to-list 'magic-mode-alist '("/** @jsx React.DOM */" . react-mode))

  (add-hook 'react-mode-hook
            (lambda ()
              (emmet-mode 0)
              (defadvice web-mode-highlight-part (around tweak-jsx activate)
                (let ((web-mode-enable-part-face nil))
                  ad-do-it)))))

(defun react/init-tern ()
  (use-package tern
    :defer t
    :config
    (progn
      (evil-leader/set-key-for-mode 'react-mode "mrrV" 'tern-rename-variable)
      (evil-leader/set-key-for-mode 'react-mode "mhd" 'tern-get-docs)
      (evil-leader/set-key-for-mode 'react-mode "mgg" 'tern-find-definition)
      (evil-leader/set-key-for-mode 'react-mode "mgG" 'tern-find-definition-by-name)
      (evil-leader/set-key-for-mode 'react-mode (kbd "m C-g") 'tern-pop-find-definition)
      (evil-leader/set-key-for-mode 'react-mode "mht" 'tern-get-type))))

(defun react/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    ;; required to make `<SPC> s l' to work correctly
    (add-hook 'react-mode-hook 'js2-imenu-extras-mode)
    (add-hook 'react-mode-hook 'js2-minor-mode))
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
    (evil-leader/set-key-for-mode 'js2-mode "mzC" 'js2-mode-toggle-hide-comments)))

(defun react/init-js2-refactor ()
  (use-package js2-refactor
    :defer t
    :init
    (progn
      (defun react/load-js2-refactor ()
        "Lazy load js2-refactor"
        (require 'js2-refactor))
      (add-hook 'react-mode-hook 'react/load-js2-refactor))
    :config
    (progn
      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mr" "refactor")

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mr3" "ternary")
      (evil-leader/set-key-for-mode 'react-mode "mr3i" 'js2r-ternary-to-if)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mra" "add/args")
      (evil-leader/set-key-for-mode 'react-mode "mrag" 'js2r-add-to-globals-annotation)
      (evil-leader/set-key-for-mode 'react-mode "mrao" 'js2r-arguments-to-object)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrb" "barf")
      (evil-leader/set-key-for-mode 'react-mode "mrba" 'js2r-forward-barf)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrc" "contract")
      (evil-leader/set-key-for-mode 'react-mode "mrca" 'js2r-contract-array)
      (evil-leader/set-key-for-mode 'react-mode "mrco" 'js2r-contract-object)
      (evil-leader/set-key-for-mode 'react-mode "mrcu" 'js2r-contract-function)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mre" "expand/extract")
      (evil-leader/set-key-for-mode 'react-mode "mrea" 'js2r-expand-array)
      (evil-leader/set-key-for-mode 'react-mode "mref" 'js2r-extract-function)
      (evil-leader/set-key-for-mode 'react-mode "mrem" 'js2r-extract-method)
      (evil-leader/set-key-for-mode 'react-mode "mreo" 'js2r-expand-object)
      (evil-leader/set-key-for-mode 'react-mode "mreu" 'js2r-expand-function)
      (evil-leader/set-key-for-mode 'react-mode "mrev" 'js2r-extract-var)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mri" "inline/inject/introduct")
      (evil-leader/set-key-for-mode 'react-mode "mrig" 'js2r-inject-global-in-iife)
      (evil-leader/set-key-for-mode 'react-mode "mrip" 'js2r-introduce-parameter)
      (evil-leader/set-key-for-mode 'react-mode "mriv" 'js2r-inline-var)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrl" "localize/log")
      (evil-leader/set-key-for-mode 'react-mode "mrlp" 'js2r-localize-parameter)
      (evil-leader/set-key-for-mode 'react-mode "mrlt" 'js2r-log-this)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrr" "rename")
      (evil-leader/set-key-for-mode 'react-mode "mrrv" 'js2r-rename-var)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrs" "split/slurp")
      (evil-leader/set-key-for-mode 'react-mode "mrsl" 'js2r-forward-slurp)
      (evil-leader/set-key-for-mode 'react-mode "mrss" 'js2r-split-string)
      (evil-leader/set-key-for-mode 'react-mode "mrsv" 'js2r-split-var-declaration)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrt" "toggle")
      (evil-leader/set-key-for-mode 'react-mode "mrtf" 'js2r-toggle-function-expression-and-declaration)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mru" "unwrap")
      (evil-leader/set-key-for-mode 'react-mode "mruw" 'js2r-unwrap)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrv" "var")
      (evil-leader/set-key-for-mode 'react-mode "mrvt" 'js2r-var-to-this)

      ;;(spacemacs/declare-prefix-for-mode 'react-mode "mrw" "wrap")
      (evil-leader/set-key-for-mode 'react-mode "mrwi" 'js2r-wrap-buffer-in-iife)
      (evil-leader/set-key-for-mode 'react-mode "mrwl" 'js2r-wrap-in-for-loop)

      (evil-leader/set-key-for-mode 'react-mode "mk" 'js2r-kill)
      (evil-leader/set-key-for-mode 'react-mode "xmj" 'js2r-move-line-down)
      (evil-leader/set-key-for-mode 'react-mode "xmk" 'js2r-move-line-up))))

(defun react/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun react/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (evil-leader/set-key-for-mode 'json-mode
      "mhp" 'jsons-print-path)
    ))

(defun react/init-tern ()
  (use-package tern
    :defer t
    :init (add-hook 'react-mode-hook 'tern-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'react-mode "mrrV" 'tern-rename-variable)
      (evil-leader/set-key-for-mode 'react-mode "mhd" 'tern-get-docs)
      (evil-leader/set-key-for-mode 'react-mode "mgg" 'tern-find-definition)
      (evil-leader/set-key-for-mode 'react-mode "mgG" 'tern-find-definition-by-name)
      (evil-leader/set-key-for-mode 'react-mode (kbd "m C-g") 'tern-pop-find-definition)
      (evil-leader/set-key-for-mode 'react-mode "mht" 'tern-get-type))))

(defun react/init-js-doc ()
  (use-package js-doc
    :defer t
    :init
    (progn
      (defun react/load-js-doc ()
          "Lazy load js-doc"
        (require 'js-doc))
      (add-hook 'react-mode-hook 'javascript/load-js-doc))
    :config
    (progn
      (evil-leader/set-key-for-mode 'react-mode "mrdb" 'js-doc-insert-file-doc)
      (evil-leader/set-key-for-mode 'react-mode "mrdf" 'js-doc-insert-function-doc)
      (evil-leader/set-key-for-mode 'react-mode "mrdt" 'js-doc-insert-tag)
      (evil-leader/set-key-for-mode 'react-mode "mrdh" 'js-doc-describe-tag))))

(defun react/init-web-beautify ()
  (use-package web-beautify
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'react-mode  "m=" 'web-beautify-js))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun react/post-init-company ()
    (spacemacs|add-company-hook react-mode))

  (defun react/init-company-tern ()
    (use-package company-tern
      :if (and (configuration-layer/package-usedp 'company)
               (configuration-layer/package-usedp 'tern))
      :defer t
      :init
      (push 'company-tern company-backends-react-mode))))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
