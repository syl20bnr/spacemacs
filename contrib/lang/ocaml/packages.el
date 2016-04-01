;;; packages.el --- ocaml Layer packages File for Spacemacs
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

(setq ocaml-packages
  '(
    tuareg
    merlin
    utop
    ocp-indent
;;    flycheck
;;    flycheck-ocaml
    ;; package ocamls go here
    ))

(defun ocaml/init-tuareg ()
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  )

(defun ocaml/opam ()
  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (setq opam-load-path (concat opam-share "/emacs/site-lisp"))
  (add-to-list 'load-path opam-load-path))

(defun ocaml/init-utop ()
  (use-package utop
    :init
    (autoload 'utop "utop" "Toplevel for OCaml" t)
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    :config
    ;; Setup environment variables using opam
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))
    ;; Update the emacs path
    (setq exec-path (append (parse-colon-path (getenv "PATH"))
                            (list exec-directory)))
    )
  )

(defun ocaml/init-ocp-indent ()
  (use-package ocp-indent
    :defer t
    :init
    (ocaml/opam)
    )
  )

(defun ocaml/init-merlin ())
;; (defun ocaml/init-flycheck-ocaml ()
;;   (progn
;;     (setq merlin-error-after-save nil)
;;     (flycheck-ocaml-setup)
;;     )
;; )

;; For each package, define a function ocaml/init-<package-ocaml>
;;
;; (defun ocaml/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
