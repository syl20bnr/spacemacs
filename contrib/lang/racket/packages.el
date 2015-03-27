(defvar racket-packages
  '(racket-mode))

(defun racket/init-racket-mode ()
  (use-package racket-mode
    :defer t
    :config
    (progn
      (use-package smartparens :config
        (progn (add-to-list 'sp--lisp-modes 'racket-mode)
               (when (fboundp 'sp-local-pair) (sp-local-pair 'racket-mode "`" nil :actions nil))))
      (sp-local-pair 'racket-mode "'" nil :actions nil)
      (evil-leader/set-key-for-mode 'racket-mode
        "ml" 'evil-lisp-state
        "mt" 'racket-test
        "mr" 'racket-run
        "mg" 'racket-visit-definition
        "mhd" 'racket-doc)
      (define-key racket-mode-map (kbd "H-r") 'racket-run)
      ;; Bug exists in Racket company backend that opens docs in new window when
      ;; company-quickhelp calls it. Note hook is appendended for proper ordering.
      (when (configuration-layer/package-declaredp 'company-quickhelp)
        (add-hook 'company-mode-hook
                  '(lambda () (when (equal major-mode 'racket-mode) (company-quickhelp-mode -1))) t)))))
