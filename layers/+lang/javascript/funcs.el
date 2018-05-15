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


;; tern

(defun spacemacs//javascript-setup-tern ()
  "Setup tern backend."
  (tern-mode))

(defun spacemacs//javascript-setup-tern-company ()
  "Setup tern auto-completion."
  (spacemacs|add-company-backends
    :backends company-tern
    :modes js2-mode
    :append-hooks nil
    :call-hooks t)
  (company-mode))

(defun spacemacs//set-tern-key-bindings (mode)
  "Set the key bindings for tern and the given MODE."
  (add-to-list (intern (format "spacemacs-jump-handlers-%S" mode))
               '(tern-find-definition :async t))
  (spacemacs/set-leader-keys-for-major-mode mode
    "rrV" 'tern-rename-variable
    "hd" 'tern-get-docs
    "gG" 'tern-find-definition-by-name
    (kbd "C-g") 'tern-pop-find-definition
    "ht" 'tern-get-type))


;; lsp

(defun spacemacs//javascript-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs//setup-lsp-jump-handler 'js2-mode)
        (lsp-javascript-typescript-enable)
        (lsp-javascript-flow-enable))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dofile."))))

(defun spacemacs//javascript-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; fix lsp-javascript company prefix
        ;; https://github.com/emacs-lsp/lsp-javascript/issues/9#issuecomment-379515379
        (defun lsp-prefix-company-transformer (candidates)
          (let ((completion-ignore-case t))
            (all-completions (company-grab-symbol) candidates)))
        (make-local-variable 'company-transformers)
        (add-to-list 'company-transformers 'lsp-prefix-company-transformer)
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes js2-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dofile."))))


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

