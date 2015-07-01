(setq racket-packages
  '(
    company
    company-quickhelp-mode
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
               (when (equal major-mode 'racket-mode)
                 (company-quickhelp-mode -1))) t))

(defun racket/init-racket-mode ()
  (use-package racket-mode
    :defer t
    :config
    (progn
      ;; smartparens configuration
      (eval-after-load 'smartparens
        '(progn (add-to-list 'sp--lisp-modes 'racket-mode)
                (when (fboundp 'sp-local-pair)
                  (sp-local-pair 'racket-mode "'" nil :actions nil)
                  (sp-local-pair 'racket-mode "`" nil :actions nil))))

      (defun spacemacs/racket-test-with-coverage ()
        "Call `racket-test' with universal argument."
        (interactive)
        (racket-test t))

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

      (evil-leader/set-key-for-mode 'racket-mode
        ;; navigation
        "mg`" 'racket-unvisit
        "mgg" 'racket-visit-definition
        "mgm" 'racket-visit-module
        "mgr" 'racket-open-require-path
        ;; doc
        "mhd" 'racket-describe
        "mhh" 'racket-doc
        ;; insert
        "mil" 'racket-insert-lambda
        ;; REPL
        "msb" 'racket-run
        "msB" 'racket-run-and-switch-to-repl
        "mse" 'racket-send-last-sexp
        "msE" 'spacemacs/racket-send-last-sexp-focus
        "msf" 'racket-send-definition
        "msF" 'spacemacs/racket-send-definition-focus
        "msi" 'racket-repl
        "msr" 'racket-send-region
        "msR" 'spacemacs/racket-send-region-focus
        "mss" 'racket-repl
        ;; Tests
        "mtb" 'racket-test
        "mtB" 'spacemacs/racket-test-with-coverage)
      (define-key racket-mode-map (kbd "H-r") 'racket-run)
      ;; remove racket auto-insert of closing delimiter
      ;; see https://github.com/greghendershott/racket-mode/issues/140
      (define-key racket-mode-map ")" 'self-insert-command)
      (define-key racket-mode-map "]" 'self-insert-command)
      (define-key racket-mode-map "}" 'self-insert-command))))
