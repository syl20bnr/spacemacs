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
   ;; auto-complete
    company
    flycheck
    flycheck-ocaml
    merlin
    ocp-indent
    tuareg
    utop
    ))

(defun ocaml/post-init-auto-complete ()
  (spacemacs|enable-auto-complete merlin-mode))

(defun ocaml/post-init-company ()
  (spacemacs|add-company-hook merlin-mode))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun ocaml/init-flycheck-ocaml ()
    (use-package flycheck-ocaml
      :if (configuration-layer/package-usedp 'flycheck)
      :defer t
      :init
      (progn
        (add-to-hook 'merlin-mode-hook '(flycheck-mode
                                         flycheck-ocaml-setup))
        (eval-after-load 'merlin
          (setq merlin-use-auto-complete-mode nil))))))

(defun ocaml/init-merlin ()
  (use-package merlin
    :defer t
    :init
    (progn
      (add-hook 'tuareg-mode-hook 'merlin-mode)
      (set-default 'merlin-use-auto-complete-mode t)
      (push 'merlin-company-backend company-backends-merlin-mode)
      (evil-leader/set-key-for-mode 'tuareg-mode
        "met" 'merlin-type-enclosing
        "mgg" 'merlin-locate
        ;;"mhh" 'merlin-document
        ))))

(defun ocaml/init-ocp-indent ()
  (use-package ocp-indent
    :defer t
    :init
    (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup)))

(defun ocaml/init-tuareg ()
  (use-package tuareg
    :defer t
    :init
    (progn
      (spacemacs//init-ocaml-opam)
      (evil-leader/set-key-for-mode 'tuareg-mode
        "mcc" 'compile))
    :config
    (when (fboundp 'sp-local-pair)
      ;; don't auto-close apostrophes (type 'a = foo)
      (sp-local-pair 'tuareg-mode "'" nil :actions nil))))

(defun ocaml/init-utop ()
  (use-package utop
    :defer t
    :init (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    :config
    (progn
      ;; Setup environment variables using opam
      (if (executable-find "opam")
          (let ((vars (car (read-from-string
                            (shell-command-to-string "opam config env --sexp")))))
            (dolist (var vars)
              (setenv (car var) (cadr var))))
        (spacemacs-buffer/warning "Cannot find \"opam\" executable."))
      ;; Update the emacs path
      (setq exec-path (append (parse-colon-path (getenv "PATH"))
                              (list exec-directory)))

      (defun spacemacs/utop-eval-phrase-and-go ()
        "Send phrase to REPL and evaluate it and switch to the REPL in
`insert state'"
        (interactive)
        (utop-eval-phrase)
        (utop)
        (evil-insert-state))

      (defun spacemacs/utop-eval-buffer-and-go ()
        "Send buffer to REPL and evaluate it and switch to the REPL in
`insert state'"
        (interactive)
        (utop-eval-buffer)
        (utop)
        (evil-insert-state))

      (defun spacemacs/utop-eval-region-and-go (start end)
        "Send region to REPL and evaluate it and switch to the REPL in
`insert state'"
        (interactive "r")
        (utop-eval-region start end)
        (utop)
        (evil-insert-state))

      (evil-leader/set-key-for-mode 'tuareg-mode
        "msb" 'utop-eval-buffer
        "msB" 'spacemacs/utop-eval-buffer-and-go
        "msi" 'utop
        "msp" 'utop-eval-phrase
        "msP" 'spacemacs/utop-eval-phrase-and-go
        "msr" 'utop-eval-region
        "msR" 'spacemacs/utop-eval-region-and-go))
    (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
    (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev)))
