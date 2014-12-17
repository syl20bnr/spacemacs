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

(defun clojure/fancify-symbols ()
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords 'clojure-mode
    `(("(\\(fn\\)[\[[:space:]]"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "λ"))))
      ("\\(#\\)("
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "ƒ"))))
      ("\\(#\\){"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1) "∈")))))))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-hook 'clojure-mode-hook '(subword-mode
                                        paredit-mode
                                        rainbow-delimiters-mode)))
    :config
    (progn
      (clojure/fancify-symbols)
      (evil-leader/set-key-for-mode 'clojure-mode  "mj" 'cider-jack-in))))

(defun clojure/init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
      (setq cider-stacktrace-default-filters '(tooling dup)
            cider-repl-pop-to-buffer-on-connect nil
            cider-prompt-save-file-on-load nil)
      (add-to-hook 'cider-mode-hook '(cider-turn-on-eldoc-mode
                                      ac-flyspell-workaround
                                      ac-cider-setup))
      (add-to-hook 'cider-repl-mode-hook '(subword-mode
                                           rainbow-delimiters-mode
                                           ac-cider-setup
                                           auto-complete-mode)))
    :config
    (progn
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
        "mgs" 'cider-jump
        "mtt" 'cider-test-run-tests))))

(defun clojure/init-ac-cider ()
  (use-package ac-cider
    :defer t
    :config (add-to-list 'ac-modes 'cider-mode)))

(defun clojure/init-clj-refactor ()
 (use-package clj-refactor
      :defer t
      :config (add-hook 'clojure-mode-hook
                           (lambda ()
                             (clj-refactor-mode 1)
                             (cljr-add-keybindings-with-prefix "C-c C-m")))))
