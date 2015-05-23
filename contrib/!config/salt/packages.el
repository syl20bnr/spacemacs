;;; packages.el --- Salt Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 phils@stackoverflow & Ben Hayden
;;
;; Author: Ben Hayden <hayden767@gmail.com>
;; Pulled from Stackoverflow: http://stackoverflow.com/a/27737759/76267
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq salt-packages '(yaml-mode))

(defun salt/init-yaml-mode ()
  (use-package yaml-mode
    :defer t
    :init (progn
            (define-derived-mode saltstack-mode yaml-mode "Saltstack"
              "Minimal Saltstack mode, based on `yaml-mode'."
              (setq tab-width 2
                    indent-tabs-mode nil))
            (add-to-list 'auto-mode-alist '("\\.sls\\'" . saltstack-mode)))))
