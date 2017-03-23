;;; packages.el --- Python Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq perl5-packages
      '((bioperl-mode :location local)
        ;;company
        ;;(company-anaconda :toggle (configuration-layer/package-usedp 'company))
        cperl-mode
        eldoc
        ;;evil-matchit
        flycheck
        ;;ggtags
        ;;helm-cscope
        ;;helm-gtags
        ;;org
        (template :location local)
        (perlnow :location local)
        ;;semantic
        mmm-mode
        smartparens
        ;;stickyfunc-enhance
        ;;xcscope
        ))

(defun perl5/init-perlnow ()
  (interactive)

  (use-package perlnow
    :defer t
    :commands (perlnow-script
               perlnow-script-simple
               perlnow-module
               perlnow-object-module
               perlnow-h2xs
               perlnow-module-starter
               perlnow-run-check
               perlnow-run
               perlnow-set-run-string
               perlnow-perldb
               perlnow-script
               perlnow-module
               perlnow-object-module
               perlnow-module-two-questions
               perlnow-run-check)
    :init
    (progn
      (template-initialize)
      (add-to-list 'template-default-directories
                   (concat (configuration-layer/get-layer-local-dir 'perl5)
                           "perlnow/templates"))
      )))

(defun perl5/pre-init-perlnow ()
  (add-to-list 'load-path
               (concat (configuration-layer/get-layer-local-dir 'perl5)
                       "template"))
  (use-package template
    :commands (template-initialize))
)

(defun perl5/post-init-mmm-mode ()
  (interactive)
  (use-package mmm-mode
    :defer t
    :mode "\\.mhtml\\'"
    :commands (mmm-mode-on
               mmm-indent-region
               mmm-parse-block
               mmm-ify-region
               mmm-parse-buffer
               mmm-insert-region
               mmm-clear-history
               mmm-insertion-help
               mmm-ify-by-class
               mmm-ify-by-regexp
               mmm-version)
    :config
    (progn
      (setq mmm-global-mode 'maybe)
      (add-to-list 'auto-mode-alist '("\\.mhtml\\'" . html-mode))
      (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
      (mmm-add-mode-ext-class 'html-mode nil 'mason)
      (mmm-add-mode-ext-class nil "\\.nw\\'" 'noweb)
      (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)
      )))

(defun perl5/init-bioperl-mode ()
  (interactive)
  (use-package bioperl-mode
    :commands (bioperl-view-pod
               bioperl-view-mode
               bioperl-view-source
               bioperl-insert-class
               bioperl-insert-module
               bioperl-skel-elements
               bioperl-insert-accessor
               bioperl-module-at-point
               bioperl-view-pod-method
               bioperl-view-pod-appendix
               bioperl-insert-method-pod
               bioperl-view-pod-synopsis
               bioperl-clear-module-cache
               bioperl-view-pod-description
               bioperl-insert-array-accessor
               bioperl-insert-abstract-method
               bioperl-find-module-at-point
               )
    ))


(defun perl5/init-cperl-mode ()
  (use-package cperl-mode))

(defun perl5/post-init-eldoc ()
  (use-package eldoc))

(defun perl5/post-init-flycheck ()
  (use-package flycheck))

(defun perl5/init-template ()
  (use-package template))

(defun perl5/post-init-smartparens ()
  (use-package smartparens))
