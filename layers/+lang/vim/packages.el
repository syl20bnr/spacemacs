;;; packages.el --- vim Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq vim-packages
    '(
      vimrc-mode
      dactyl-mode
      ;; package vims go here
      ))

;; List of packages to exclude.
(setq vim-excluded-packages '())

;; For each package, define a function vim/init-<package-vim>
;;
;; (defun vim/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun vim/init-vimrc-mode ()
  "Initialize vimrc package"
  (use-package vimrc-mode
    :mode "\\.vim[rc]?\\'"
    :mode "_vimrc\\'"
    :defer t
    :init
    (progn
      (add-hook 'vimrc-mode-hook '(lambda () 
                                    (highlight-numbers-mode -1)
                                    (rainbow-delimiters-mode-disable))))))

(defun vim/init-dactyl-mode ()
  (use-package dactyl-mode
    :mode "pentadactylrc\\'"
    :mode "vimperatorrc\\'"
    :mode "_pentadactylrc\\'"
    :mode "_vimperatorrc\\'"
    :mode "\\.penta\\'"
    :mode "\\.vimp\\'"
    :defer t
    ))
