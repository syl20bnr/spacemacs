(defvar clojure-packages
  '(
    clojure-mode
    cider
    clj-refactor
    ac-cider
    align-cljlet
    rainbow-delimiters
    subword
   )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun clojure/init-ac-cider ()
  (use-package ac-cider
    :defer t
    :if (configuration-layer/package-declaredp 'auto-complete)
    :init
    (progn
      (add-to-hook 'cider-mode-hook '(ac-flyspell-workaround
                                      ac-cider-setup))
      (add-to-hook 'cider-repl-mode-hook '(ac-cider-setup
                                           auto-complete-mode)))
    :config
    (push 'cider-mode ac-modes)))

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
      (add-hook 'clojure-mode-hook 'cider-mode)
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
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

      (defun spacemacs/send-last-sexp-to-repl ()
        "Send last sexp to REPL and evaluate it without changing
the focus."
        (interactive)
        (spacemacs//cider-eval-in-repl-no-focus (cider-last-sexp)))

      (defun spacemacs/send-last-sexp-to-repl-focus ()
        "Send last sexp to REPL and evaluate it and switch to the REPL in
`insert state'."
        (interactive)
        (cider-insert-last-sexp-in-repl t)
        (evil-insert-state))

      (defun spacemacs/send-region-to-repl ()
        "Send region to REPL and evaluate it without changing
the focus."
        (interactive)
        (spacemacs//cider-eval-in-repl-no-focus
         (buffer-substring-no-properties (region-beginning) (region-end))))

      (defun spacemacs/send-region-to-repl-focus ()
        "Send region to REPL and evaluate it and switch to the REPL in
`insert state'."
        (interactive)
        (cider-insert-in-repl
         (buffer-substring-no-properties (region-beginning) (region-end)) t)
        (evil-insert-state))

      (defun spacemacs/send-function-to-repl ()
        "Send current function to REPL and evaluate it without changing
the focus."
        (interactive)
        (spacemacs//cider-eval-in-repl-no-focus (cider-defun-at-point)))

      (defun spacemacs/send-function-to-repl-focus ()
        "Send current function to REPL and evaluate it and switch to the REPL in
`insert state'."
        (interactive)
        (cider-insert-defun-in-repl t)
        (evil-insert-state))

      (defun spacemacs/send-ns-form-to-repl ()
        "Send buffer's ns form to REPL and evaluate it without changing
the focus."
        (interactive)
        (spacemacs//cider-eval-in-repl-no-focus (cider-ns-form)))

      (defun spacemacs/send-function-to-repl-focus ()
        "Send ns form to REPL and evaluate it and switch to the REPL in
`insert state'."
        (interactive)
        (cider-insert-ns-form-in-repl t)
        (evil-insert-state))

      (defun spacemacs/send-buffer-in-repl-and-focus ()
        "Send the current buffer in the REPL and switch to the REPL in
`insert state'."
        (interactive)
        (cider-load-buffer)
        (cider-switch-to-repl-buffer)
        (evil-insert-state))

      (spacemacs/activate-evil-leader-for-map 'cider-stacktrace-mode-map)
      (evil-leader/set-key-for-mode 'clojure-mode
        "mdd" 'cider-doc
        "mdg" 'cider-grimoire
        "mdj" 'cider-javadoc

        "meb" 'cider-eval-buffer
        "mee" 'cider-eval-last-sexp
        "mer" 'cider-eval-region

        "mgb" 'cider-jump-back
        "mge" 'cider-jump-to-compilation-error
        "mgg" 'cider-jump-to-var
        "mgr" 'cider-jump-to-resource

        "msb" 'cider-load-buffer
        "msB" 'spacemacs/send-buffer-in-repl-and-focus
        "mse" 'spacemacs/send-last-sexp-to-repl
        "msE" 'spacemacs/send-last-sexp-to-repl-focus
        "msf" 'spacemacs/send-function-to-repl
        "msF" 'spacemacs/send-function-to-repl-focus
        "msi" 'cider-jack-in
        "msn" 'spacemacs/send-ns-form-to-repl
        "msN" 'spacemacs/send-function-to-repl-focus
        "msr" 'spacemacs/send-region-to-repl
        "msR" 'spacemacs/send-region-to-repl-focus
        "mss" 'cider-switch-to-repl-buffer

        "mtt" 'cider-test-run-tests)
      (when clojure-enable-fancify-symbols 
        (clojure/fancify-symbols 'cider-repl-mode)))))

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
        (evil-leader/set-key-for-mode 'clojure-mode
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
          "mruw" 'cljr-unwind))))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :mode (("\.clj$"      . clojure-mode)
           ("\.cljs$"     . clojure-mode)
           ("\.cljx$"     . clojure-mode)
           ("\.edn$"      . clojure-mode)
           ("\.boot$"     . clojure-mode)
           ("\.cljs\.hl$" . clojure-mode))
    :config
    (progn
      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'clojure-mode)))))

(defun clojure/init-rainbow-delimiters ()
  (if (configuration-layer/package-declaredp 'cider)
      (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)))

(defun clojure/init-subword ()
  (unless (version< emacs-version "24.4")
    (add-hook 'cider-mode-hook 'subword-mode)))
