;;; packages.el --- ocaml Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    smartparens
    tuareg
    utop
    ))

(defun ocaml/post-init-company ()
  (spacemacs|add-company-hook merlin-mode))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun ocaml/post-init-flycheck ()
    (spacemacs/add-flycheck-hook 'merlin-mode-hook))
  (defun ocaml/init-flycheck-ocaml ()
    (use-package flycheck-ocaml
      :if (configuration-layer/package-usedp 'flycheck)
      :defer t
      :init
      (progn
        (with-eval-after-load 'merlin
          (setq merlin-error-after-save nil)
          (flycheck-ocaml-setup))))))

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
      (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
        "cp" 'merlin-project-check
        "cr" 'merlin-refresh
        "cv" 'merlin-goto-project-file
        "eC" 'merlin-error-check
        "en" 'merlin-error-next
        "eN" 'merlin-error-prev
        "gb" 'merlin-pop-stack
        "gg" #'(lambda ()
                (interactive)
                (let ((merlin-locate-in-new-window 'never))
                  (merlin-locate)))
        "gG" #'(lambda ()
                (interactive)
                (let ((merlin-locate-in-new-window 'always))
                  (merlin-locate)))
        "gl" 'merlin-locate-ident
        "gi" 'merlin-switch-to-ml
        "gI" 'merlin-switch-to-mli
        "hh" 'merlin-document
        "ht" 'merlin-type-enclosing
        "hT" 'merlin-type-expr
        "rd" 'merlin-destruct))))

(defun ocaml/init-ocp-indent ()
  (use-package ocp-indent
    :defer t
    :init
    (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup)))

(defun ocaml/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

(defun ocaml/init-tuareg ()
  (use-package tuareg
    :defer t
    :init
    (progn
      (spacemacs//init-ocaml-opam)
      (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
        "ga" 'tuareg-find-alternate-file
        "cc" 'compile)
      ;; Make OCaml-generated files invisible to filename completion
      (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))
        (add-to-list 'completion-ignored-extensions ext)))))

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

      (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
        "sb" 'utop-eval-buffer
        "sB" 'spacemacs/utop-eval-buffer-and-go
        "si" 'utop
        "sp" 'utop-eval-phrase
        "sP" 'spacemacs/utop-eval-phrase-and-go
        "sr" 'utop-eval-region
        "sR" 'spacemacs/utop-eval-region-and-go))
    (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
    (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev)))
