;;; packages.el --- pandoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Christoph Paulik & Contributors
;;
;; Author: Christoph Paulik <cpaulik@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar pandoc-packages
  '(
    ;; package pandocs go here
    pandoc-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar pandoc-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function pandoc/init-<package-pandoc>
;;
(defun pandoc/init-pandoc-mode ()
  "Initialize my package"
  (use-package pandoc-mode
    :defer t
    :commands spacemacs/run-pandoc
    :config
    (progn
      (defun spacemacs/run-pandoc ()
        "Start pandoc for the buffer and open the menu"
        (interactive)
        (pandoc-mode)
        (pandoc-main-hydra/body)
        )
      (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
      )
    :init
    (progn
      (evil-leader/set-key "P/" 'spacemacs/run-pandoc)
      )
    )
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
