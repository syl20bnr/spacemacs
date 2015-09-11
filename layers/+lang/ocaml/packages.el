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
   ;; flycheck
   ;; flycheck-ocaml
    merlin
    ocp-indent
    tuareg
    utop
    ))

;;(defun ocaml/post-init-auto-complete ()
;;  (spacemacs|enable-auto-complete merlin-mode))

(defun ocaml/post-init-company ()
  (spacemacs|add-company-hook merlin-mode))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun ocaml/post-init-flycheck ()
    (add-hook 'merlin-mode-hook 'flycheck-mode))
  (defun ocaml/init-flycheck-ocaml ()
    (use-package flycheck-ocaml
      :if (configuration-layer/package-usedp 'flycheck)
      :defer t
      :init
      (progn
        (with-eval-after-load 'merlin
          (progn
            (setq merlin-error-after-save nil)
            (flycheck-ocaml-setup))
          )))))

(defun ocaml/init-merlin ()
  (use-package merlin
    :defer t
    :init
    (progn
      (add-hook 'tuareg-mode-hook 'merlin-mode)
;;      (set-default 'merlin-use-auto-complete-mode t)
      (set-default 'merlin-use-auto-complete-mode nil)
      (setq merlin-completion-with-doc t)
      (push 'merlin-company-backend company-backends-merlin-mode)
      (evil-leader/set-key-for-mode 'tuareg-mode
        "mcp" 'merlin-project-check
        "mcr" 'merlin-refresh
        "mcv" 'merlin-goto-project-file
        "meC" 'merlin-error-check
        "men" 'merlin-error-next
        "meN" 'merlin-error-prev
        "mgb" 'merlin-pop-stack
        "mgg" #'(lambda ()
                (interactive)
                (let ((merlin-locate-in-new-window 'never))
                  (merlin-locate)))
        "mgG" #'(lambda ()
                (interactive)
                (let ((merlin-locate-in-new-window 'always))
                  (merlin-locate)))
        "mgl" 'merlin-locate-ident
        "mgi" 'merlin-switch-to-ml
        "mgI" 'merlin-switch-to-mli
        "mhh" 'merlin-document
        "mht" 'merlin-type-enclosing
        "mhT" 'merlin-type-expr
        "mrd" 'merlin-destruct
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
        "mga" 'tuareg-find-alternate-file
        "mcc" 'compile))
    :config
    (when (fboundp 'sp-local-pair)
      ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
      (sp-local-pair 'tuareg-mode "'" nil :actions nil)
      (sp-local-pair 'tuareg-mode "`" nil :actions nil))))

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
