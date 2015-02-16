(defvar clojure-packages
  '(
    clojure-mode
    paredit
    cider
    clj-refactor
    ac-cider
    align-cljlet
   )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
    `(("(\\(fn\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "λ"))))
      ("(\\(partial\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "Ƥ"))))
      ("\\(#\\)("
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "ƒ"))))
      ("\\(#\\){"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "∈")))))))

(defun clojure/general-mode ()
  "Start general modes for both clojure-mode and repl"
  (progn
    (subword-mode t)
    (paredit-mode t)
    (rainbow-delimiters-mode t)))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :mode (("\.clj$"      . clojure-mode)
           ("\.cljs$"     . clojure-mode)
           ("\.cljx$"     . clojure-mode)
           ("\.edn$"      . clojure-mode)
           ("\.boot$"     . clojure-mode)
           ("\.cljs\.hl$" . clojure-mode))
    :init
    (progn
      (add-to-hook 'clojure-mode-hook '(clojure/general-mode)))
    :config
    (progn
      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'clojure-mode))
      (evil-leader/set-key-for-mode 'clojure-mode  "mj" 'cider-jack-in))))

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
      (add-to-hook 'cider-repl-mode-hook '(clojure/general-mode
                                           ac-cider-setup
                                           auto-complete-mode)))
    :config
    (progn
      (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)

      (spacemacs/activate-evil-leader-for-map 'cider-stacktrace-mode-map)
      (evil-leader/set-key-for-mode 'clojure-mode
        "meb" 'cider-eval-buffer
        "mer" 'cider-eval-region
        "mes" 'cider-eval-last-sexp
        "mk"  'cider-load-buffer
        "mz"  'cider-switch-to-repl-buffer
        "mdd" 'cider-doc
        "mdg" 'cider-grimoire
        "mdj" 'cider-javadoc
        "mgv" 'cider-jump-to-var
        "mgr" 'cider-jump-to-resource
        "mge" 'cider-jump-to-compilation-error
        "mgb" 'cider-jump-back
        "mtt" 'cider-test-run-tests)
      (when clojure-enable-fancify-symbols 
        (clojure/fancify-symbols 'cider-repl-mode)))))

(defun clojure/init-ac-cider ()
  (use-package ac-cider
    :defer t
    :config (add-to-list 'ac-modes 'cider-mode)))

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
          "mr-ar" 'cljr-add-require-to-ns
          "mr-au" 'cljr-add-use-to-ns
          "mr-cc" 'cljr-cycle-coll
          "mr-ci" 'cljr-cycle-if
          "mr-cp" 'cljr-cycle-privacy
          "mr-dk" 'cljr-destructure-keys
          "mr-el" 'cljr-expand-let
          "mr-il" 'cljr-introduce-let
          "mr-mf" 'cljr-move-form
          "mr-ml" 'cljr-move-to-let
          "mr-pc" 'cljr-project-clean
          "mr-pf" 'cljr-promote-function
          "mr-rf" 'cljr-rename-file
          "mr-rl" 'cljr-remove-let
          "mr-rr" 'cljr-remove-unused-requires
          "mr-ru" 'cljr-replace-use
          "mr-sn" 'cljr-sort-ns
          "mr-sp" 'cljr-sort-project-dependencies
          "mr-sr" 'cljr-stop-referring
          "mr-tf" 'cljr-thread-first-all
          "mr-th" 'cljr-thread
          "mr-tl" 'cljr-thread-last-all
          "mr-ua" 'cljr-unwind-all
          "mr-uw" 'cljr-unwind
          "mr-am" 'cljr-add-missing-libspec
          "mr-ap" 'cljr-add-project-dependency
          "mr-cn" 'cljr-clean-ns
          "mr-ef" 'cljr-extract-function
          "mr-fu" 'cljr-find-usages
          "mr-hd" 'cljr-hotload-dependency
          "mr-rd" 'cljr-remove-debug-fns
          "mr-rs" 'cljr-rename-symbol))))
