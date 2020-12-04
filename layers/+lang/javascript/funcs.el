;;; funcs.el --- Javascript Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; backend

(defun space-macs//javascript-backend ()
  "Returns selected backend."
  (if javascript-backend
      javascript-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'tern))))

(defun space-macs//javascript-setup-backend ()
  "Conditionally setup javascript backend."
  (pcase (space-macs//javascript-backend)
    (`tern (space-macs//javascript-setup-tern))
    (`tide (space-macs//tide-setup))
    (`lsp (space-macs//javascript-setup-lsp))))

(defun space-macs//javascript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//javascript-backend)
    (`tide (space-macs//tide-setup-company 'js2-mode))))

(defun space-macs//javascript-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (space-macs//javascript-backend)
    (`lsp (space-macs//javascript-setup-lsp-dap))))

(defun space-macs//javascript-setup-next-error-fn ()
  "If the `syntax-checking' layer is enabled, then disable `js2-mode''s
`next-error-function', and let `flycheck' handle any errors."
  (when (configuration-layer/layer-used-p 'syntax-checking)
    (setq-local next-error-function nil)))

;; lsp

(defun space-macs//javascript-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (when (not javascript-lsp-linter)
          (setq-local lsp-diagnostics-provider :none))
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun space-macs//javascript-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-firefox)
  (require 'dap-chrome))


;; tern
(defun space-macs//javascript-setup-tern ()
  (if (configuration-layer/layer-used-p 'tern)
      (when (locate-file "tern" exec-path)
        (space-macs/tern-setup-tern))
    (message (concat "Tern was configured as the javascript backend but "
                     "the `tern' layer is not present in your `.space-macs'!"))))


;; js-doc

(defun space-macs/js-doc-require ()
  "Lazy load js-doc"
  (require 'js-doc))

(defun space-macs/js-doc-set-key-bindings (mode)
  "Setup the key bindings for `js2-doc' for the given MODE."
  (space-macs/declare-prefix-for-mode mode "mrd" "documentation")
  (space-macs/set-leader-keys-for-major-mode mode
    "rdb" 'js-doc-insert-file-doc
    "rdf" (if (configuration-layer/package-used-p 'yasnippet)
              'js-doc-insert-function-doc-snippet
            'js-doc-insert-function-doc)
    "rdt" 'js-doc-insert-tag
    "rdh" 'js-doc-describe-tag))

;; js-refactor

(defun space-macs/js2-refactor-require ()
  "Lazy load js2-refactor"
  (require 'js2-refactor))


;; skewer

(defun space-macs/skewer-start-repl ()
  "Attach a browser to e-macs and start a skewer REPL."
  (interactive)
  (run-skewer)
  (skewer-repl))

(defun space-macs/skewer-load-buffer-and-focus ()
  "Execute whole buffer in browser and switch to REPL in insert state."
  (interactive)
  (skewer-load-buffer)
  (skewer-repl)
  (evil-insert-state))

(defun space-macs/skewer-eval-defun-and-focus ()
  "Execute function at point in browser and switch to REPL in insert state."
  (interactive)
  (skewer-eval-defun)
  (skewer-repl)
  (evil-insert-state))

(defun space-macs/skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browser."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(defun space-macs/skewer-eval-region-and-focus (beg end)
  "Execute the region in browser and swith to REPL in insert state."
  (interactive "r")
  (space-macs/skewer-eval-region beg end)
  (skewer-repl)
  (evil-insert-state))


;; Others

(defun space-macs//javascript-setup-checkers ()
  (when-let* ((found (executable-find "eslint_d")))
    (set (make-local-variable 'flycheck-javascript-eslint-executable) found)))

(defun space-macs/javascript-format ()
  "Call formatting tool specified in `javascript-fmt-tool'."
  (interactive)
  (cond
   ((eq javascript-fmt-tool 'prettier)
    (call-interactively 'prettier-js))
   ((eq javascript-fmt-tool 'web-beautify)
    (call-interactively 'web-beautify-js))
   (t (error (concat "%s isn't valid javascript-fmt-tool value."
                     " It should be 'web-beutify or 'prettier.")
             (symbol-name javascript-fmt-tool)))))

(defun space-macs/javascript-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'space-macs/javascript-format t t))


