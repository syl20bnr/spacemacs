;;; packages.el --- Javascript Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst javascript-packages
  '(
    add-node-modules-path
    company
    counsel-gtags
    dap-mode
    evil-matchit
    flycheck
    ggtags
    helm-gtags
    imenu
    impatient-mode
    import-js
    js-doc
    js2-mode
    js2-refactor
    livid-mode
    nodejs-repl
    org
    prettier-js
    skewer-mode
    tern
    web-beautify))

(defun javascript/post-init-add-node-modules-path ()
  (space-macs/add-to-hooks #'add-node-modules-path '(css-mode-hook
                                                    js2-mode-hook)))

(defun javascript/post-init-company ()
  (add-hook 'js2-mode-local-vars-hook #'space-macs//javascript-setup-company))

(defun javascript/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'js2-mode))

(defun javascript/pre-init-dap-mode ()
  (pcase (space-macs//javascript-backend)
    (`lsp (add-to-list 'space-macs--dap-supported-modes 'js2-mode)))
  (add-hook 'js2-mode-local-vars-hook #'space-macs//javascript-setup-dap))

(defun javascript/post-init-evil-matchit ()
  (add-hook `js2-mode-hook `turn-on-evil-matchit-mode))

(defun javascript/post-init-flycheck ()
  (space-macs/enable-flycheck 'js2-mode)
  (add-hook 'js2-mode-hook #'space-macs//javascript-setup-checkers 'append))

(defun javascript/post-init-ggtags ()
  (add-hook 'js2-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun javascript/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'js2-mode))

(defun javascript/post-init-imenu ()
  ;; Required to make imenu functions work correctly
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

(defun javascript/post-init-impatient-mode ()
  (space-macs/set-leader-keys-for-major-mode 'js2-mode
    "I" 'space-macs/impatient-mode))

(defun javascript/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'space-macs--import-js-modes (cons 'js2-mode 'js2-mode-hook))))

(defun javascript/init-js-doc ()
  (use-package js-doc
    :defer t
    :init (space-macs/js-doc-set-key-bindings 'js2-mode)
    (add-hook 'js2-mode-hook 'space-macs/js-doc-require)))

(defun javascript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode (("\\.m?js\\'"  . js2-mode))
    :init
    (progn
      (add-hook 'js2-mode-local-vars-hook #'space-macs//javascript-setup-backend)
      (add-hook 'js2-mode-local-vars-hook #'space-macs//javascript-setup-next-error-fn)
      ;; safe values for backend to be used in directory file variables
      (dolist (value '(lsp tern tide))
        (add-to-list 'safe-local-variable-values
                     (cons 'javascript-backend value))))
    :config
    (progn
      (when javascript-fmt-on-save
        (add-hook 'js2-mode-local-vars-hook 'space-macs/javascript-fmt-before-save-hook))
      ;; prefixes
      (space-macs/declare-prefix-for-mode 'js2-mode "mh" "documentation")
      (space-macs/declare-prefix-for-mode 'js2-mode "mg" "goto")
      (space-macs/declare-prefix-for-mode 'js2-mode "mr" "refactor")
      (space-macs/declare-prefix-for-mode 'js2-mode "mz" "folding")
      ;; key bindings
      (space-macs/set-leader-keys-for-major-mode 'js2-mode
        "w" 'js2-mode-toggle-warnings-and-errors
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments))))

(defun javascript/init-js2-refactor ()
  (use-package js2-refactor
    :defer t
    :init
    (progn
      (add-hook 'js2-mode-hook 'space-macs/js2-refactor-require)
      ;; prefixes
      (space-macs/declare-prefix-for-mode 'js2-mode "mr3" "ternary")
      (space-macs/declare-prefix-for-mode 'js2-mode "mra" "add/args")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrb" "barf")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrc" "contract")
      (space-macs/declare-prefix-for-mode 'js2-mode "mre" "expand/extract")
      (space-macs/declare-prefix-for-mode 'js2-mode "mri" "inline/inject/introduct")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrl" "localize/log")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrr" "rename")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrs" "split/slurp")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrt" "toggle")
      (space-macs/declare-prefix-for-mode 'js2-mode "mru" "unwrap")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrv" "var")
      (space-macs/declare-prefix-for-mode 'js2-mode "mrw" "wrap")
      (space-macs/declare-prefix-for-mode 'js2-mode "mx" "text")
      (space-macs/declare-prefix-for-mode 'js2-mode "mxm" "move")
      ;; key bindings
      (space-macs/set-leader-keys-for-major-mode 'js2-mode
        "r3i" 'js2r-ternary-to-if
        "rag" 'js2r-add-to-globals-annotation
        "rao" 'js2r-arguments-to-object
        "rba" 'js2r-forward-barf
        "rca" 'js2r-contract-array
        "rco" 'js2r-contract-object
        "rcu" 'js2r-contract-function
        "rea" 'js2r-expand-array
        "ref" 'js2r-extract-function
        "rem" 'js2r-extract-method
        "reo" 'js2r-expand-object
        "reu" 'js2r-expand-function
        "rev" 'js2r-extract-var
        "rig" 'js2r-inject-global-in-iife
        "rip" 'js2r-introduce-parameter
        "riv" 'js2r-inline-var
        "rlp" 'js2r-localize-parameter
        "rlt" 'js2r-log-this
        "rrv" 'js2r-rename-var
        "rsl" 'js2r-forward-slurp
        "rss" 'js2r-split-string
        "rsv" 'js2r-split-var-declaration
        "rtf" 'js2r-toggle-function-expression-and-declaration
        "ruw" 'js2r-unwrap
        "rvt" 'js2r-var-to-this
        "rwi" 'js2r-wrap-buffer-in-iife
        "rwl" 'js2r-wrap-in-for-loop
        "k" 'js2r-kill
        "xmj" 'js2r-move-line-down
        "xmk" 'js2r-move-line-up))))

(defun javascript/init-livid-mode ()
  (when (eq javascript-repl 'skewer)
    (use-package livid-mode
      :defer t
      :init
      (progn
        (space-macs/declare-prefix-for-mode 'js2-mode "mT" "toggle")
        (space-macs|add-toggle javascript-repl-live-evaluation
          :mode livid-mode
          :documentation "Live evaluation of JS buffer change."
          :evil-leader-for-mode (js2-mode . "Tl"))
        (space-macs|diminish livid-mode " ðŸ…»" " [l]")))))

(defun javascript/init-nodejs-repl ()
  (when (eq javascript-repl 'nodejs)
    (use-package nodejs-repl
      :defer nil
      :init
      (space-macs/register-repl 'nodejs-repl
                               'nodejs-repl
                               "nodejs-repl")
      :config
      (progn
        (space-macs/declare-prefix-for-mode 'js2-mode "ms" "nodejs-repl")
        (space-macs/set-leader-keys-for-major-mode 'js2-mode
          "'" 'nodejs-repl
          "ss" 'nodejs-repl
          "si" 'nodejs-repl-switch-to-repl
          "se" 'nodejs-repl-send-last-expression
          "sE" (lambda ()
                 (interactive)
                 (nodejs-repl-send-last-expression)
                 (nodejs-repl-switch-to-repl))
          "sb" 'nodejs-repl-send-buffer
          "sB" (lambda ()
                 (interactive)
                 (nodejs-repl-send-buffer)
                 (nodejs-repl-switch-to-repl))
          "sl" 'nodejs-repl-send-line
          "sL" (lambda ()
                 (interactive)
                 (nodejs-repl-send-line)
                 (nodejs-repl-switch-to-repl))
          "sr" 'nodejs-repl-send-region
          "sR" (lambda (start end)
                 (interactive "r")
                 (nodejs-repl-send-region start end)
                 (nodejs-repl-switch-to-repl)))
        (space-macs/declare-prefix-for-mode 'js2-mode
          "msE" "nodejs-send-last-expression-and-focus")
        (space-macs/declare-prefix-for-mode 'js2-mode
          "msB" "nodejs-send-buffer-and-focus")
        (space-macs/declare-prefix-for-mode 'js2-mode
          "msL" "nodejs-send-line-and-focus")
        (space-macs/declare-prefix-for-mode 'js2-mode
          "msR" "nodejs-send-region-and-focus")))))


(defun javascript/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(js . t))))

(defun javascript/pre-init-prettier-js ()
  (when (eq javascript-fmt-tool 'prettier)
    (add-to-list 'space-macs--prettier-modes 'js2-mode)))

(defun javascript/init-skewer-mode ()
  (when (eq javascript-repl 'skewer)
    (use-package skewer-mode
      :defer t
      :init
      (progn
        (space-macs/register-repl 'skewer-mode
                                 'space-macs/skewer-start-repl
                                 "skewer")
        (add-hook 'js2-mode-hook 'skewer-mode))
      :config
      (progn
        (space-macs|hide-lighter skewer-mode)
        (space-macs/declare-prefix-for-mode 'js2-mode "ms" "skewer")
        (space-macs/declare-prefix-for-mode 'js2-mode "me" "eval")
        (space-macs/set-leader-keys-for-major-mode 'js2-mode
          "'" 'space-macs/skewer-start-repl
          "ee" 'skewer-eval-last-expression
          "eE" 'skewer-eval-print-last-expression
          "sb" 'skewer-load-buffer
          "sB" 'space-macs/skewer-load-buffer-and-focus
          "si" 'space-macs/skewer-start-repl
          "sf" 'skewer-eval-defun
          "sF" 'space-macs/skewer-eval-defun-and-focus
          "sr" 'space-macs/skewer-eval-region
          "sR" 'space-macs/skewer-eval-region-and-focus
          "ss" 'skewer-repl)))))

(defun javascript/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'js2-mode))

(defun javascript/pre-init-web-beautify ()
  (when (eq javascript-fmt-tool 'web-beautify)
    (add-to-list 'space-macs--web-beautify-modes
                 (cons 'js2-mode 'web-beautify-js))))


