;;; funcs.el --- Semantic Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/load-srefactor ()
  "Hook to load the `srefactor' library."
  (require 'srefactor)
  ;; currently, evil-mode overrides key mapping of srefactor menu
  ;; must expplicity enable evil-emacs-state. This is ok since
  ;; srefactor supports j,k,/ and ? commands when Evil is
  ;; available
  (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state))

(defun spacemacs/load-stickyfunc-enhance ()
  "Hook to load the `stickyfunc-enhance' library."
  (require 'stickyfunc-enhance))
