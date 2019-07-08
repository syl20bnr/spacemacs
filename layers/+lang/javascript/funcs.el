;;; funcs.el --- Javascript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; backend

(defun spacemacs//javascript-setup-backend ()
  "Conditionally setup javascript backend."
  (pcase javascript-backend
    (`tern (spacemacs//javascript-setup-tern))
    (`lsp (spacemacs//javascript-setup-lsp))))

(defun spacemacs//javascript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase javascript-backend
    (`tern (spacemacs//javascript-setup-tern-company))
    (`lsp (spacemacs//javascript-setup-lsp-company))))


;; lsp

(defun spacemacs//javascript-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile.")))
  (if (configuration-layer/layer-used-p 'dap)
      (progn
        (require 'dap-firefox)
        (require 'dap-chrome)
        (spacemacs/dap-bind-keys-for-mode 'js2-mode))
    (message "`dap' layer is not installed, please add `dap' layer to your dotfile.")))

(defun spacemacs//javascript-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes js2-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))


;; tern
(defun spacemacs//javascript-setup-tern ()
  (if (configuration-layer/layer-used-p 'tern)
      (when (locate-file "tern" exec-path)
        (spacemacs/tern-setup-tern))
    (message (concat "Tern was configured as the javascript backend but "
                     "the `tern' layer is not present in your `.spacemacs'!"))))

(defun spacemacs//javascript-setup-tern-company ()
  (if (configuration-layer/layer-used-p 'tern)
      (when (locate-file "tern" exec-path)
        (spacemacs/tern-setup-tern-company 'js2-mode))
    (message (concat "Tern was configured as the javascript backend but "
                     "the `tern' layer is not present in your `.spacemacs'!"))))


;; js-doc

(defun spacemacs/js-doc-require ()
  "Lazy load js-doc"
  (require 'js-doc))
(add-hook 'js2-mode-hook 'spacemacs/js-doc-require)

(defun spacemacs/js-doc-set-key-bindings (mode)
  "Setup the key bindings for `js2-doc' for the given MODE."
  (spacemacs/declare-prefix-for-mode mode "mrd" "documentation")
  (spacemacs/set-leader-keys-for-major-mode mode
    "rdb" 'js-doc-insert-file-doc
    "rdf" (if (configuration-layer/package-used-p 'yasnippet)
              'js-doc-insert-function-doc-snippet
            'js-doc-insert-function-doc)
    "rdt" 'js-doc-insert-tag
    "rdh" 'js-doc-describe-tag))

;; js-refactor

(defun spacemacs/js2-refactor-require ()
  "Lazy load js2-refactor"
  (require 'js2-refactor))


;; skewer

(defun spacemacs/skewer-start-repl ()
  "Attach a browser to Emacs and start a skewer REPL."
  (interactive)
  (run-skewer)
  (skewer-repl))

(defun spacemacs/skewer-load-buffer-and-focus ()
  "Execute whole buffer in browser and switch to REPL in insert state."
  (interactive)
  (skewer-load-buffer)
  (skewer-repl)
  (evil-insert-state))

(defun spacemacs/skewer-eval-defun-and-focus ()
  "Execute function at point in browser and switch to REPL in insert state."
  (interactive)
  (skewer-eval-defun)
  (skewer-repl)
  (evil-insert-state))

(defun spacemacs/skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browser."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(defun spacemacs/skewer-eval-region-and-focus (beg end)
  "Execute the region in browser and swith to REPL in insert state."
  (interactive "r")
  (spacemacs/skewer-eval-region beg end)
  (skewer-repl)
  (evil-insert-state))


;; Others

(defun spacemacs/javascript-format ()
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

(defun spacemacs/javascript-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'spacemacs/javascript-format t t))
