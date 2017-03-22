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

(setq perl5-packages '(
  anything
  company
  (company-anaconda :toggle (configuration-layer/package-usedp 'company))
  cperl
  eldoc
  evil-matchit
  flycheck
  ggtags
  helm-cscope
  helm-gtags
  mmm-mode
  org
  (perlnow :location local)
  ;;perl-completion
  semantic
  smartparens
  stickyfunc-enhance
  (template :location local)
  xcscope
  ))

(defun perl5/init-cperl ()
  (interactive)
  (use-package cperl)
  (setq cperl-hairy t))

(defun perl5/init-perlnow ()
  (interactive)
  (add-to-list 'load-path
               (concat (configuration-layer/get-layer-local-dir 'perl5)
                       "template"))
  (use-package template)
  (require 'template)
  (template-initialize)
  (add-to-list 'template-default-directories
               (concat (configuration-layer/get-layer-local-dir 'perl5)
                       "perlnow/templates"))
  (setq perlnow-template-location
        (concat (configuration-layer/get-layer-local-dir 'perl5)
                                           "perlnow/templates"))
  (use-package perlnow)
  (require 'mmm-mode)
  (add-to-list 'auto-mode-alist '("\\.mhtml\\'" . html-mode))
  (mmm-add-mode-ext-class 'html-mode "\\.mhtml\\'" 'mason)
  )

;(defun perl5/init-perl-completion
;  (use-package perl-completion)
;  (require 'perl-completion)
;  (auto-complete-mode t)
;  (make-variable-buffer-local 'ac-sources)
;  (setq ac-sources '(ac-source-perl-completion)))
