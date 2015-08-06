(setq clojure-packages
  '(
    align-cljlet
    cider
    cider-eval-sexp-fu
    clj-refactor
    clojure-mode
    company
    rainbow-delimiters
    subword
   ))

(defun clojure/init-align-cljlet ()
  (use-package align-cljlet
    :defer t
    :init
    (add-hook 'clojure-mode-hook (lambda () (require 'align-cljlet)))
    :config
    (evil-leader/set-key-for-mode 'clojure-mode
      "mfl" 'align-cljlet)))

(defun clojure/init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
      (setq cider-stacktrace-default-filters '(tooling dup)
            cider-repl-pop-to-buffer-on-connect nil
            cider-prompt-save-file-on-load nil
            cider-repl-use-clojure-font-lock t)
      (push "\\*cider-repl\.\+\\*" spacemacs-useful-buffers-regexp)
      (add-hook 'clojure-mode-hook 'cider-mode)
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
      (if dotspacemacs-smartparens-strict-mode
          (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)))
    :config
    (progn
      ;; add support for golden-ratio
      (eval-after-load 'golden-ratio
        '(push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))
      ;; add support for evil
      (push 'cider-stacktrace-mode evil-motion-state-modes)
      (push 'cider-popup-buffer-mode evil-motion-state-modes)

      (defun spacemacs//cider-eval-in-repl-no-focus (form)
        "Insert FORM in the REPL buffer and eval it."
        (let ((start-pos (point)))
          (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
            (setq form (replace-match "" t t form)))
          (with-current-buffer (cider-current-repl-buffer)
            (insert form)
            (indent-region start-pos (point))
            (cider-repl-return))))

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

      (defun spacemacs/cider-send-buffer-in-repl-and-focus ()
        "Send the current buffer in the REPL and switch to the REPL in
`insert state'."
        (interactive)
        (cider-load-buffer)
        (cider-switch-to-repl-buffer)
        (evil-insert-state))

      (defun spacemacs/cider-test-run-focused-test ()
        (interactive)
        (cider-load-buffer)
        (spacemacs//cider-eval-in-repl-no-focus (cider-test-run-test)))

      (defun spacemacs/cider-test-run-all-tests ()
        (interactive)
        (cider-load-buffer)
        (spacemacs//cider-eval-in-repl-no-focus (cider-test-run-tests nil)))

      (defun spacemacs/cider-test-rerun-tests ()
        (interactive)
        (cider-load-buffer)
        (spacemacs//cider-eval-in-repl-no-focus (cider-test-rerun-tests)))

      (evilify cider-stacktrace-mode cider-stacktrace-mode-map)

      ;; open cider-doc directly and close it with q
      (setq cider-prompt-for-symbol nil)
      (evilify cider-docview-mode cider-docview-mode-map
               (kbd "q") 'cider-popup-buffer-quit)
      (evilify cider-inspector-mode cider-inspector-mode-map
               (kbd "L") 'cider-inspector-pop)

      (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
        (evil-leader/set-key-for-mode m
          "mhh" 'cider-doc
          "mhg" 'cider-grimoire
          "mhj" 'cider-javadoc

          "meb" 'cider-eval-buffer
          "mee" 'cider-eval-last-sexp
          "mef" 'cider-eval-defun-at-point
          "mer" 'cider-eval-region
          "mew" 'cider-eval-last-sexp-and-replace

          "mgb" 'cider-jump-back
          "mge" 'cider-jump-to-compilation-error
          "mgg" 'cider-jump-to-var
          "mgr" 'cider-jump-to-resource

          "msb" 'cider-load-buffer
          "msB" 'spacemacs/cider-send-buffer-in-repl-and-focus
          "msc" 'cider-connect
          "mse" 'spacemacs/cider-send-last-sexp-to-repl
          "msE" 'spacemacs/cider-send-last-sexp-to-repl-focus
          "msf" 'spacemacs/cider-send-function-to-repl
          "msF" 'spacemacs/cider-send-function-to-repl-focus
          "msi" 'cider-jack-in
          "msn" 'spacemacs/cider-send-ns-form-to-repl
          "msN" 'spacemacs/cider-send-ns-form-to-repl-focus
          "msq" 'cider-quit
          "msr" 'spacemacs/cider-send-region-to-repl
          "msR" 'spacemacs/cider-send-region-to-repl-focus
          "mss" 'cider-switch-to-repl-buffer

          "mta" 'spacemacs/cider-test-run-all-tests
          "mtr" 'spacemacs/cider-test-rerun-tests
          "mtt" 'spacemacs/cider-test-run-focused-test

          "mdi" 'cider-inspect
          "mdb" 'cider-debug-defun-at-point))
      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'cider-repl-mode)))

    (when (configuration-layer/package-usedp 'evil-jumper)
      (defadvice cider-jump-to-var (before add-evil-jump activate)
        (evil-set-jump)))))

(defun clojure/init-cider-eval-sexp-fu ()
  (eval-after-load 'eval-sexp-fu
    '(require 'cider-eval-sexp-fu)))

(defun clojure/init-clj-refactor ()
 (use-package clj-refactor
      :defer t
      :init
      (add-hook 'clojure-mode-hook 'clj-refactor-mode)
      :config
      (progn
        (cljr-add-keybindings-with-prefix "C-c C-f")
        ;; not supported for now
        ;; (spacemacs/declare-prefix "mr" "clj-refactor")

        (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
          (evil-leader/set-key-for-mode m
            "mrad" 'cljr-add-declaration
            "mrai" 'cljr-add-import-to-ns
            "mram" 'cljr-add-missing-libspec
            "mrap" 'cljr-add-project-dependency
            "mrar" 'cljr-add-require-to-ns
            "mrau" 'cljr-add-use-to-ns
            "mrcc" 'cljr-cycle-coll
            "mrci" 'cljr-cycle-if
            "mrcn" 'cljr-clean-ns
            "mrcp" 'cljr-cycle-privacy
            "mrdk" 'cljr-destructure-keys
            "mref" 'cljr-extract-function
            "mrel" 'cljr-expand-let
            "mrfu" 'cljr-find-usages
            "mrhd" 'cljr-hotload-dependency
            "mril" 'cljr-introduce-let
            "mrmf" 'cljr-move-form
            "mrml" 'cljr-move-to-let
            "mrpc" 'cljr-project-clean
            "mrpf" 'cljr-promote-function
            "mrrd" 'cljr-remove-debug-fns
            "mrrf" 'cljr-rename-file
            "mrrl" 'cljr-remove-let
            "mrrr" 'cljr-remove-unused-requires
            "mrrs" 'cljr-rename-symbol
            "mrru" 'cljr-replace-use
            "mrsn" 'cljr-sort-ns
            "mrsp" 'cljr-sort-project-dependencies
            "mrsr" 'cljr-stop-referring
            "mrtf" 'cljr-thread-first-all
            "mrth" 'cljr-thread
            "mrtl" 'cljr-thread-last-all
            "mrua" 'cljr-unwind-all
            "mruw" 'cljr-unwind)))))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config
    (progn
      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'clojure-mode)))))
(defun clojure/init-rainbow-delimiters ()
  (if (configuration-layer/package-usedp 'cider)
      (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)))

(defun clojure/init-subword ()
  (unless (version< emacs-version "24.4")
    (add-hook 'cider-mode-hook 'subword-mode)))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun clojure/post-init-company ()
    (push 'company-capf company-backends-cider-mode)
    (spacemacs|add-company-hook cider-mode)

    (push 'company-capf company-backends-cider-repl-mode)
    (spacemacs|add-company-hook cider-repl-mode)))
