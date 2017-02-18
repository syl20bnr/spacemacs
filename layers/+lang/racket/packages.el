(setq racket-packages
  '(
    company
    company-quickhelp
    ggtags
    helm-gtags
    racket-mode
    ))

(defun racket/post-init-company ()
  ;; this is the only thing to do to enable company in racket-mode
  ;; because racket-mode handle everything for us when company
  ;; is loaded.
  (add-hook 'racket-mode-hook 'company-mode))

(defun racket/post-init-company-quickhelp ()
  ;; Bug exists in Racket company backend that opens docs in new window when
  ;; company-quickhelp calls it. Note hook is appendended for proper ordering.
  (add-hook 'company-mode-hook
            '(lambda ()
               (when (and (equal major-mode 'racket-mode)
                          (bound-and-true-p company-quickhelp-mode))
                 (company-quickhelp-mode -1))) t))

(defun racket/post-init-ggtags ()
  (add-hook 'racket-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun racket/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'racket-mode))

(defun racket/init-racket-mode ()
  (use-package racket-mode
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'racket-mode 'racket-repl "racket"))
    :config
    (progn
      ;; smartparens configuration
      (with-eval-after-load 'smartparens
        (add-to-list 'sp--lisp-modes 'racket-mode)
        (when (fboundp 'sp-local-pair)
          (sp-local-pair 'racket-mode "'" nil :actions nil)
          (sp-local-pair 'racket-mode "`" nil :actions nil)))

      (defun spacemacs/racket-test-with-coverage ()
        "Call `racket-test' with universal argument."
        (interactive)
        (racket-test t))

      (defun spacemacs/racket-run-and-switch-to-repl ()
        "Call `racket-run-and-switch-to-repl' and enable
`insert state'."
        (interactive)
        (racket-run-and-switch-to-repl)
        (evil-insert-state))

      (defun spacemacs/racket-send-last-sexp-focus ()
        "Call `racket-send-last-sexp' and switch to REPL buffer in
`insert state'."
        (interactive)
        (racket-send-last-sexp)
        (racket-repl)
        (evil-insert-state))

      (defun spacemacs/racket-send-definition-focus ()
        "Call `racket-send-definition' and switch to REPL buffer in
`insert state'."
        (interactive)
        (racket-send-definition)
        (racket-repl)
        (evil-insert-state))

      (defun spacemacs/racket-send-region-focus (start end)
        "Call `racket-send-region' and switch to REPL buffer in
`insert state'."
        (interactive "r")
        (racket-send-region start end)
        (racket-repl)
        (evil-insert-state))

      (dolist (prefix '(("mg" . "navigation")
                        ("mh" . "doc")
                        ("mi" . "insert")
                        ("ms" . "repl")
                        ("mt" . "tests")))
        (spacemacs/declare-prefix-for-mode 'racket-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'racket-mode
        ;; navigation
        "g`" 'racket-unvisit
        "gm" 'racket-visit-module
        "gr" 'racket-open-require-path
        ;; doc
        "hd" 'racket-describe
        "hh" 'racket-doc
        ;; insert
        "il" 'racket-insert-lambda
        ;; REPL
        "'"  'racket-repl
        "sb" 'racket-run
        "sB" 'spacemacs/racket-run-and-switch-to-repl
        "se" 'racket-send-last-sexp
        "sE" 'spacemacs/racket-send-last-sexp-focus
        "sf" 'racket-send-definition
        "sF" 'spacemacs/racket-send-definition-focus
        "si" 'racket-repl
        "sr" 'racket-send-region
        "sR" 'spacemacs/racket-send-region-focus
        "ss" 'racket-repl
        ;; Tests
        "tb" 'racket-test
        "tB" 'spacemacs/racket-test-with-coverage)
      (define-key racket-mode-map (kbd "H-r") 'racket-run)
      ;; remove racket auto-insert of closing delimiter
      ;; see https://github.com/greghendershott/racket-mode/issues/140
      (define-key racket-mode-map ")" 'self-insert-command)
      (define-key racket-mode-map "]" 'self-insert-command)
      (define-key racket-mode-map "}" 'self-insert-command))))
