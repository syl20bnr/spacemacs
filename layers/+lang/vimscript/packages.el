;;; packages.el --- vimscript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq vimscript-packages
    '(
      vimrc-mode
      ggtags
      helm-gtags
      dactyl-mode
      ))

(defun vimscript/init-vimrc-mode ()
  "Initialize vimrc package"
  (use-package vimrc-mode
    :mode "\\.vim[rc]?\\'"
    :mode "_vimrc\\'"
    :defer t
    :init
    (progn
    (defun spacemacs//vimrc-mode-hook ()
      "Hooked function for `vimrc-mode-hook'."
      (highlight-numbers-mode -1)
      (rainbow-delimiters-mode-disable))
    (add-hook 'vimrc-mode-hook 'spacemacs//vimrc-mode-hook))))

(defun vimscript/init-dactyl-mode ()
  (use-package dactyl-mode
    :mode "pentadactylrc\\'"
    :mode "vimperatorrc\\'"
    :mode "_pentadactylrc\\'"
    :mode "_vimperatorrc\\'"
    :mode "\\.penta\\'"
    :mode "\\.vimp\\'"
    :defer t))

(defun vimscript/post-init-ggtags ()
  (add-hook 'vimrc-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun vimscript/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'vimrc-mode))
