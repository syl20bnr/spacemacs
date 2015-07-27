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
      flycheck
      tern
      jsfmt
      company
      company-tern
      ))

;; List of packages to exclude.
(setq react-excluded-packages '())

;; For each package, define a function react/init-<package-name>
;;
(defun react/post-init-flycheck ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (with-eval-after-load 'flycheck
                  ;; use eslint with web-mode for jsx files
                  (flycheck-add-mode 'javascript-eslint 'web-mode)

                  ;; disable jshint since we prefer eslint checking
                  (setq-default flycheck-disabled-checkers
                                (append flycheck-disabled-checkers
                                        '(javascript-jshint)))

                  ;; disable json-jsonlist checking for json files
                  (setq-default flycheck-disabled-checkers
                                (append flycheck-disabled-checkers
                                        '(json-jsonlist))))))))

(defun react/post-init-web-mode ()
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (defadvice web-mode-highlight-part (around tweak-jsx activate)
                (if (equal web-mode-content-type "jsx")
                    (let ((web-mode-enable-part-face nil))
                      ad-do-it)
                  ad-do-it)))))

(defun react/post-init-js2-mode ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (js2-minor-mode)))))

(defun react/post-init-js2-refactor ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (require 'js2-refactor)))))

(defun react/post-init-js2-refactor ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (require 'js-doc)))))

(defun react/post-init-tern ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (tern-mode)))))

(defun react/post-init-company-tern ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (push 'company-tern company-backends-web-mode)))))

(defun react/pre-init-js2-mode ()
  (spacemacs|use-package-add-hook js2-mode
    :post-config
    (progn
      (evil-leader/set-key-for-mode 'web-mode "mw" 'js2-mode-toggle-warnings-and-errors))))

(defun react/pre-init-tern ()
  (spacemacs|use-package-add-hook tern
    :post-config
    (progn
        (evil-leader/set-key-for-mode 'web-mode "mrv" 'tern-rename-variable) ;; was mrrV
        (evil-leader/set-key-for-mode 'web-mode "mhd" 'tern-get-docs)
        (evil-leader/set-key-for-mode 'web-mode "mgg" 'tern-find-definition)
        (evil-leader/set-key-for-mode 'web-mode "mgG" 'tern-find-definition-by-name)
        (evil-leader/set-key-for-mode 'web-mode (kbd "m C-g") 'tern-pop-find-definition)
        (evil-leader/set-key-for-mode 'web-mode "mht" 'tern-get-type))))

(defun react/pre-init-js2-refactor ()
  (spacemacs|use-package-add-hook js2-refactor
    :post-config
    (progn
      (evil-leader/set-key-for-mode 'web-mode "mr3i" 'js2r-ternary-to-if)

      (evil-leader/set-key-for-mode 'web-mode "mrag" 'js2r-add-to-globals-annotation)
      (evil-leader/set-key-for-mode 'web-mode "mrao" 'js2r-arguments-to-object)

      (evil-leader/set-key-for-mode 'web-mode "mrba" 'js2r-forward-barf)

      ;; (evil-leader/set-key-for-mode 'web-mode "mrca" 'js2r-contract-array)
      ;; (evil-leader/set-key-for-mode 'web-mode "mrco" 'js2r-contract-object)
      ;; (evil-leader/set-key-for-mode 'web-mode "mrcu" 'js2r-contract-function)

      (evil-leader/set-key-for-mode 'web-mode "mrea" 'js2r-expand-array)
      (evil-leader/set-key-for-mode 'web-mode "mref" 'js2r-extract-function)
      (evil-leader/set-key-for-mode 'web-mode "mrem" 'js2r-extract-method)
      (evil-leader/set-key-for-mode 'web-mode "mreo" 'js2r-expand-object)
      (evil-leader/set-key-for-mode 'web-mode "mreu" 'js2r-expand-function)
      (evil-leader/set-key-for-mode 'web-mode "mrev" 'js2r-extract-var)

      (evil-leader/set-key-for-mode 'web-mode "mrig" 'js2r-inject-global-in-iife)
      (evil-leader/set-key-for-mode 'web-mode "mrip" 'js2r-introduce-parameter)
      (evil-leader/set-key-for-mode 'web-mode "mriv" 'js2r-inline-var)

      (evil-leader/set-key-for-mode 'web-mode "mrlp" 'js2r-localize-parameter)
      (evil-leader/set-key-for-mode 'web-mode "mrlt" 'js2r-log-this)

      ;; (evil-leader/set-key-for-mode 'web-mode "mrrv" 'js2r-rename-var)

      (evil-leader/set-key-for-mode 'web-mode "mrsl" 'js2r-forward-slurp)
      (evil-leader/set-key-for-mode 'web-mode "mrss" 'js2r-split-string)
      (evil-leader/set-key-for-mode 'web-mode "mrsv" 'js2r-split-var-declaration)

      (evil-leader/set-key-for-mode 'web-mode "mrtf" 'js2r-toggle-function-expression-and-declaration)

      (evil-leader/set-key-for-mode 'web-mode "mruw" 'js2r-unwrap)

      ;; (evil-leader/set-key-for-mode 'web-mode "mrvt" 'js2r-var-to-this)

      ;; (evil-leader/set-key-for-mode 'web-mode "mrwi" 'js2r-wrap-buffer-in-iife)
      ;; (evil-leader/set-key-for-mode 'web-mode "mrwl" 'js2r-wrap-in-for-loop)

      (evil-leader/set-key-for-mode 'web-mode "mk" 'js2r-kill)
      (evil-leader/set-key-for-mode 'web-mode "xmj" 'js2r-move-line-down)
      (evil-leader/set-key-for-mode 'web-mode "xmk" 'js2r-move-line-up))))

(defun react/pre-init-js-doc ()
  (spacemacs|use-package-add-hook js-doc
    :post-config
    (progn
      (evil-leader/set-key-for-mode 'web-mode "midb" 'js-doc-insert-file-doc) ;; was mrdb
      (evil-leader/set-key-for-mode 'web-mode "midf" 'js-doc-insert-function-doc) ;; was mrdf
      (evil-leader/set-key-for-mode 'web-mode "midt" 'js-doc-insert-tag) ;; was mrdt
      (evil-leader/set-key-for-mode 'web-mode "midh" 'js-doc-describe-tag)))) ;; was mrdh

(defun react/init-jsfmt ()
  (use-package jsfmt
    :commands (jsfmt)
    :config
    (evil-leader/set-key-for-mode 'web-mode "mf" 'jsfmt)))

(defun react/post-init-jsfmt ()
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (require 'jsfmt)))))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
