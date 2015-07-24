;;;; packages.el --- edebug Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Christopher McCloud <mccloud.christopher@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq edebug-packages
      '(
        edebug
        edebug-x
        ))

(defun edebug/init-edebug ()
  (use-package edebug
   :config
   (progn

     (defun spacemacs//toggle-edebug-micro-state ()
       "Activates evilified mode and triggers edebug micro state.
Attaches to edebug-mode-hook"
       (if (evil-evilified-state-p)
           (normal-mode)
         (evil-evilified-state))
       (spacemacs/edebug-mode-micro-state))

     (defun spacemacs//edebug-doc ()
       "docstring for edebug-mode micro state"
       (concat
        "[q] quit [s] step [n] next [f] forward-sexp [i] step-in [o] step-out "
        "[t] trace [e] eval-expression [f] forward-sexp [b] set-breakpoint "
        "[c] goto-point [I] instrument-callee [S] stop [?] edebug-mode help"))

     (spacemacs|define-micro-state edebug-mode
       :doc (spacemacs//edebug-doc)
       :persistent t
       :bindings
       ("q" edebug-top-level-nonstop :exit t)
       ("s" edebug-step-mode)
       ("n" edebug-next-mode)
       ("i" edebug-step-in)
       ("o" edebug-step-out)
       ("b" edebug-set-breakpoint)
       ("t" edebug-trace-mode)
       ("e" edebug-eval-last-sexp)
       ("f" edebug-forward-sexp)
       ("c" edebug-goto-here)
       ("I" edebug-instrument-callee)
       ("S" edebug-stop)
       ("?" edebug-help))

     (spacemacs|evilify-map edebug-mode-map)

     (add-hook 'edebug-mode-hook 'spacemacs//toggle-edebug-micro-state))))

(defun edebug/init-edebug-x ()
  (use-package edebug-x))

