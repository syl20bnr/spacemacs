;;; funcs.el --- Semantic Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/load-srefactor ()
  "Hook to load the `srefactor' library."
  (require 'srefactor)
  ;; currently, evil-mode overrides key mapping of srefactor menu
  ;; must expplicity enable evil-e-macs-state. This is ok since
  ;; srefactor supports j,k,/ and ? commands when Evil is
  ;; available
  (add-hook 'srefactor-ui-menu-mode-hook 'evil-e-macs-state))

(defun space-macs/load-stickyfunc-enhance ()
  "Hook to load the `stickyfunc-enhance' library."
  (require 'stickyfunc-enhance))


