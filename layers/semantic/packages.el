;;; packages.el --- semantic Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq semantic-packages
      '(
        ;; package semantic go here
        semantic
        ;; srefactor
        stickyfunc-enhance
        ))

(unless (version< emacs-version "24.4")
  (add-to-list 'semantic-packages 'srefactor))

(defvar semantic-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function semantic/init-<package-semantic>
;;
;; (defun semantic/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun semantic/enable-semantic-mode (mode)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda ()
                     (require 'semantic)
                     (add-to-list 'semantic-default-submodes
                                  'global-semantic-stickyfunc-mode)
                     (add-to-list 'semantic-default-submodes
                                  'global-semantic-idle-summary-mode)
                     (semantic-mode 1)))))

(defun semantic/init-semantic ()
  (use-package semantic
    :defer t
    :init
    (progn
      (setq srecode-map-save-file (concat spacemacs-cache-directory
                                          "srecode-map.el"))
      (setq semanticdb-default-save-directory (concat spacemacs-cache-directory
                                                      "semanticdb/"))
      (unless (file-exists-p semanticdb-default-save-directory)
        (make-directory semanticdb-default-save-directory))
      )))

(defun semantic/init-srefactor ()
  (use-package srefactor
    :defer t
    :init
    (progn
      (defun spacemacs/lazy-load-srefactor ()
        "Lazy load the package."
        (require 'srefactor)
        ;; currently, evil-mode overrides key mapping of srefactor menu
        ;; must expplicity enable evil-emacs-state. This is ok since
        ;; srefactor supports j,k,/ and ? commands when Evil is
        ;; available
        (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state)))))

(defun semantic/init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (defun spacemacs/lazy-load-stickyfunc-enhance ()
      "Lazy load the package."
      (require 'stickyfunc-enhance))))
