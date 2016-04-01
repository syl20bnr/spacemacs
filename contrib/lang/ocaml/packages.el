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
    company
    flycheck
    flycheck-ocaml
    ;; package ocamls go here
    ))

(defun ocaml/init-tuareg ()
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (evil-leader/set-key-for-mode 'tuareg-mode
   "mcc" 'compile
  )
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
    (defun utop-eval-phrase-and-go ()
      (interactive)
      (utop-eval-phrase)
      (utop))
    (defun utop-eval-buffer-and-go ()
      (interactive)
      (utop-eval-buffer)
      (utop))
    (defun utop-eval-region-and-go (start end)
      (interactive "r")
      (utop-eval-region start end)
      (utop))
    (evil-leader/set-key-for-mode 'tuareg-mode
      "msb" 'utop-eval-buffer
      "msB" 'utop-eval-buffer-and-go
      "msi" 'utop
      "msp" 'utop-eval-phrase
      "msP" 'utop-eval-phrase-and-go
      "msr" 'utop-eval-region
      "msR" 'utop-eval-region-and-go
      )
    )
    (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
    (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev)
  )

(defun ocaml/init-ocp-indent ()
  (use-package ocp-indent
    :defer t
    :init
    (ocaml/opam)
    )
  )

(defun ocaml/init-merlin ()
  (use-package merlin
    :defer t
    :init
    (ocaml/opam)
    (set-default 'merlin-use-auto-complete-mode 'easy)
    (when (configuration-layer/package-usedp 'company)
      (push 'merlin-company-backend company-backends-merlin-mode))
    )
    (evil-leader/set-key-for-mode 'tuareg-mode
      "mgg" 'merlin-locate
      "met" 'merlin-type-enclosing
     ;;      "mhh" 'merlin-document
      )
  )

(when (configuration-layer/layer-usedp 'auto-completion)
  ;; Hook company to merlin-mode
  (defun ocaml/post-init-company ()
    (spacemacs|add-company-hook merlin-mode)
    ))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun ocaml/init-flycheck-ocaml ()
    (use-package flycheck-ocaml
      :if (configuration-layer/package-usedp 'flycheck)
      :defer t
      :init
      (add-hook 'merlin-mode-hook 'flycheck-mode)
      (with-eval-after-load 'merlin
        ;; Disable Merlin's own error checking
        (setq merlin-error-after-save nil)
        ;; Enable Flycheck checker
        (flycheck-ocaml-setup))
      )))

;; For each package, define a function ocaml/init-<package-ocaml>
;;
;; (defun ocaml/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
