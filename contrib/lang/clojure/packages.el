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
    :config (add-to-list 'ac-modes 'cider-mode)))

(defun clojure/init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
      (setq cider-stacktrace-default-filters '(tooling dup)
            cider-repl-pop-to-buffer-on-connect nil
            cider-prompt-save-file-on-load nil
            cider-repl-use-clojure-font-lock t)
      (add-to-hook 'cider-mode-hook '(cider-turn-on-eldoc-mode
                                      ac-flyspell-workaround
                                      ac-cider-setup))
      (add-to-hook 'cider-repl-mode-hook '(ac-cider-setup
                                           auto-complete-mode)))
    :config
    (progn
      (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)

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
        "mer" 'cider-eval-region
        "mes" 'cider-eval-last-sexp

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
        "mss" 'cider-switch-to-repl-buffer

        "mtt" 'cider-test-run-tests)
      (when clojure-enable-fancify-symbols 
        (clojure/fancify-symbols 'cider-repl-mode)))))

(defun clojure/init-clj-refactor ()
 (use-package clj-refactor
      :defer t
      :config
      (progn
        (add-hook 'clojure-mode-hook
                  (lambda ()
                    (clj-refactor-mode 1)
                    (cljr-add-keybindings-with-prefix "C-c C-f")))

        (spacemacs/declare-prefix "mr" "clj-refactor")

        (evil-leader/set-key-for-mode 'clojure-mode
          "mr-ad" 'cljr-add-declaration
          "mr-ai" 'cljr-add-import-to-ns
          "mr-am" 'cljr-add-missing-libspec
          "mr-ap" 'cljr-add-project-dependency
          "mr-ar" 'cljr-add-require-to-ns
          "mr-au" 'cljr-add-use-to-ns
          "mr-cc" 'cljr-cycle-coll
          "mr-ci" 'cljr-cycle-if
          "mr-cn" 'cljr-clean-ns
          "mr-cp" 'cljr-cycle-privacy
          "mr-dk" 'cljr-destructure-keys
          "mr-ef" 'cljr-extract-function
          "mr-el" 'cljr-expand-let
          "mr-fu" 'cljr-find-usages
          "mr-hd" 'cljr-hotload-dependency
          "mr-il" 'cljr-introduce-let
          "mr-mf" 'cljr-move-form
          "mr-ml" 'cljr-move-to-let
          "mr-pc" 'cljr-project-clean
          "mr-pf" 'cljr-promote-function
          "mr-rd" 'cljr-remove-debug-fns
          "mr-rf" 'cljr-rename-file
          "mr-rl" 'cljr-remove-let
          "mr-rr" 'cljr-remove-unused-requires
          "mr-rs" 'cljr-rename-symbol
          "mr-ru" 'cljr-replace-use
          "mr-sn" 'cljr-sort-ns
          "mr-sp" 'cljr-sort-project-dependencies
          "mr-sr" 'cljr-stop-referring
          "mr-tf" 'cljr-thread-first-all
          "mr-th" 'cljr-thread
          "mr-tl" 'cljr-thread-last-all
          "mr-ua" 'cljr-unwind-all
          "mr-uw" 'cljr-unwind))))

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
  (if configuration-layer/package-declaredp 'cider
    (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)))

(defun clojure/init-subword ()
  (unless (version< emacs-version "24.4")
    (add-hook 'cider-mode-hook 'subword-mode)))
