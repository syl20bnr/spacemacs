(defconst lsp-packages
  '(
    (company-lsp :requires company)
    lsp-mode
    lsp-ui
    ))

(defun lsp/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :config
    (progn
      (add-hook 'lsp-mode-hook #'lsp-ui-mode)

      ;; Disable lsp-flycheck.el in favor of lsp-ui-flycheck.el
      (setq lsp-enable-flycheck nil)

      (spacemacs|diminish lsp-mode " Ⓛ" " L")
      )))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :config
    (progn
      (lsp//sync-peek-face)
      (add-hook 'spacemacs-post-theme-change-hook #'lsp//sync-peek-face)
      )))
