;;; funcs.el --- Clojure Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//clojure-backend ()
  "Return selected backend."
  (if clojure-backend
      clojure-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'cider))))

(defun spacemacs//clojure-setup-backend ()
  "Conditionally setup clojure backend."
  (pcase (spacemacs//clojure-backend)
    (`lsp (lsp))))

(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
                          `(("(\\(fn\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "λ")
                                       nil)))
                            ("(\\(partial\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "Ƥ")
                                       nil)))
                            ("(\\(comp\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∘")
                                       nil)))
                            ("\\(#\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))
                            ("\\(#\\){"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∈")
                                       nil))))))


(defun spacemacs/cider-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (cider-eval-last-sexp)))


(defun spacemacs//cider-eval-in-repl-no-focus (form)
  "Insert FORM in the REPL buffer and eval it."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-connection)
    (let ((pt-max (point-max)))
      (goto-char pt-max)
      (insert form)
      (indent-region pt-max (point))
      (cider-repl-return)
      (with-selected-window (get-buffer-window (cider-current-connection))
        (goto-char (point-max))))))

(defun spacemacs/cider-send-last-sexp-to-repl ()
  "Send last sexp to REPL and evaluate it without changing
the focus."
  (interactive)
  (spacemacs//cider-eval-in-repl-no-focus (cider-last-sexp)))

(defun spacemacs/cider-send-last-sexp-to-repl-focus ()
  "Send last sexp to REPL and evaluate it and switch to the REPL in
`insert state'."
  (interactive)
  (cider-insert-last-sexp-in-repl t)
  (evil-insert-state))

(defun spacemacs/cider-send-region-to-repl (start end)
  "Send region to REPL and evaluate it without changing
the focus."
  (interactive "r")
  (spacemacs//cider-eval-in-repl-no-focus
   (buffer-substring-no-properties start end)))

(defun spacemacs/cider-send-region-to-repl-focus (start end)
  "Send region to REPL and evaluate it and switch to the REPL in
`insert state'."
  (interactive "r")
  (cider-insert-in-repl
   (buffer-substring-no-properties start end) t)
  (evil-insert-state))

(defun spacemacs/cider-send-function-to-repl ()
  "Send current function to REPL and evaluate it without changing
the focus."
  (interactive)
  (spacemacs//cider-eval-in-repl-no-focus (cider-defun-at-point)))

(defun spacemacs/cider-send-function-to-repl-focus ()
  "Send current function to REPL and evaluate it and switch to the REPL in
`insert state'."
  (interactive)
  (cider-insert-defun-in-repl t)
  (evil-insert-state))

(defun spacemacs/cider-send-ns-form-to-repl ()
  "Send buffer's ns form to REPL and evaluate it without changing
the focus."
  (interactive)
  (spacemacs//cider-eval-in-repl-no-focus (cider-ns-form)))

(defun spacemacs/cider-send-ns-form-to-repl-focus ()
  "Send ns form to REPL and evaluate it and switch to the REPL in
`insert state'."
  (interactive)
  (cider-insert-ns-form-in-repl t)
  (evil-insert-state))

(defun spacemacs/cider-send-buffer-in-repl-and-focus (&optional set-namespace)
  "Send the current buffer in the REPL and switch to the REPL in
`insert state'. When set-namespace, also change into the namespace of the buffer."
  (interactive "P")
  (cider-load-buffer)
  (cider-switch-to-repl-buffer set-namespace)
  (evil-insert-state))

(defun spacemacs/cider-test-run-focused-test ()
  "Run test around point."
  (interactive)
  (cider-load-buffer)
  (cider-test-run-test))

(defalias 'spacemacs/cider-test-run-all-tests #'spacemacs/cider-test-run-project-tests
  "Runs all tests in all project namespaces.")

(defun spacemacs/cider-test-run-ns-tests ()
  "Run namespace test."
  (interactive)
  (cider-load-buffer)
  (call-interactively #'cider-test-run-ns-tests))

(defun spacemacs/cider-test-run-loaded-tests ()
  "Run loaded tests."
  (interactive)
  (cider-load-buffer)
  (call-interactively #'cider-test-run-loaded-tests))

(defun spacemacs/cider-test-run-project-tests ()
  "Run project tests."
  (interactive)
  (cider-load-buffer)
  (call-interactively #'cider-test-run-project-tests))

(defun spacemacs/cider-test-rerun-failed-tests ()
  "Rerun failed tests."
  (interactive)
  (cider-load-buffer)
  (cider-test-rerun-failed-tests))

(defun spacemacs/cider-display-error-buffer (&optional arg)
  "Displays the *cider-error* buffer in the current window.
If called with a prefix argument, uses the other-window instead."
  (interactive "P")
  (let ((buffer (get-buffer cider-error-buffer)))
    (when buffer
      (funcall (if (equal arg '(4))
                   'switch-to-buffer-other-window
                 'switch-to-buffer)
               buffer))))

(defun spacemacs/cider-toggle-repl-pretty-printing ()
  "Toggle REPL pretty printing on and off."
  (interactive)
  (setq cider-repl-use-pretty-printing
        (if cider-repl-use-pretty-printing nil t))
  (message "Cider REPL pretty printing: %s"
           (if cider-repl-use-pretty-printing "ON" "OFF")))

(defun spacemacs/cider-toggle-repl-font-locking ()
  "Toggle font locking in REPL."
  (interactive)
  (setq cider-repl-use-clojure-font-lock
        (if cider-repl-use-pretty-printing nil t))
  (message "Cider REPL clojure-mode font-lock: %s"
           (if cider-repl-use-clojure-font-lock "ON" "OFF")))

(defun spacemacs/cider-debug-setup ()
  "Initialize debug mode."
  (when (memq dotspacemacs-editing-style '(hybrid vim))
    (evil-make-overriding-map cider--debug-mode-map 'normal)
    (evil-normalize-keymaps)))

(defun spacemacs/clj-find-var (sym-name &optional arg)
  "Attempts to jump-to-definition of the symbol-at-point.

If CIDER fails, or not available, falls back to dumb-jump."
  (interactive (list (cider-symbol-at-point)))
  (if (and (cider-connected-p) (cider-var-info sym-name))
      (unless (eq 'symbol (type-of (cider-find-var nil sym-name)))
        (dumb-jump-go))
    (dumb-jump-go)))

(defun spacemacs/clj-describe-missing-refactorings ()
  "Inform the user to add clj-refactor to configuration"
  (interactive)
  (with-help-window (help-buffer)
    (princ "The package clj-refactor is disabled by default.
To enable it, add the following variable to the clojure layer
in your Spacemacs configuration:

  dotspacemacs-configuration-layers
  '(...
    (clojure :variables
             clojure-enable-clj-refactor t)
    ) ")))

(defmacro spacemacs|forall-clojure-modes (m &rest body)
  "Executes BODY with M bound to all clojure derived modes."
  (declare (indent 1))
  `(dolist (,m '(clojure-mode
                 clojurec-mode
                 clojurescript-mode
                 clojurex-mode
                 cider-repl-mode
                 cider-clojure-interaction-mode))
     ,@body))

(defun spacemacs//clj-repl-wrap-c-j ()
  "Dynamically dispatch c-j to company or repl functions."
  (interactive)
  (if (company-tooltip-visible-p)
      (company-select-next)
    (cider-repl-next-input)))

(defun spacemacs//clj-repl-wrap-c-k ()
  "Dynamically dispatch c-k to company or repl functions."
  (interactive)
  (if (company-tooltip-visible-p)
      (company-select-previous)
    (cider-repl-previous-input)))

(defun spacemacs/cider-find-and-clear-repl-buffer ()
  "Calls cider-find-and-clear-repl-output interactively with C-u prefix
set so that it clears the whole REPL buffer, not just the output."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'cider-find-and-clear-repl-output)))
