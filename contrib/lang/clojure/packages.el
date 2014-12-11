(defvar clojure-packages
  '(
    clojure-mode
    paredit
    cider
    clj-refactor
    ac-cider
   )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :config
    (progn
      (add-hook 'clojure-mode-hook 'subword-mode)
      (add-hook 'clojure-mode-hook 'paredit-mode)
      (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

      (evil-leader/set-key-for-mode 'clojure-mode  "mj" 'cider-jack-in))))

(defun clojure/init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
        (setq cider-stacktrace-default-filters '(tooling dup))
        (setq cider-repl-pop-to-buffer-on-connect nil)
        (setq cider-prompt-save-file-on-load nil)

        (add-hook 'cider-mode-hook         'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook    'subword-mode)
        (add-hook 'cider-repl-mode-hook    'rainbow-delimiters-mode)
        (add-hook 'cider-mode-hook         'ac-flyspell-workaround)
        (add-hook 'cider-mode-hook         'ac-cider-setup)
        (add-hook 'cider-repl-mode-hook    'ac-cider-setup)
        (add-hook 'cider-repl-mode-hook    'auto-complete-mode))

    :config
    (progn
        (evil-leader/set-key-for-mode 'clojure-mode "meb" 'cider-eval-buffer)
        (evil-leader/set-key-for-mode 'clojure-mode "mer" 'cider-eval-region)
        (evil-leader/set-key-for-mode 'clojure-mode "mes" 'cider-eval-last-sexp)

        (evil-leader/set-key-for-mode 'clojure-mode "mk" 'cider-load-buffer)
        (evil-leader/set-key-for-mode 'clojure-mode "mz" 'cider-switch-to-repl-buffer)

        (evil-leader/set-key-for-mode 'clojure-mode "mdd" 'cider-doc)
        (evil-leader/set-key-for-mode 'clojure-mode "mdg" 'cider-grimoire)
        (evil-leader/set-key-for-mode 'clojure-mode "mdj" 'cider-javadoc)

        (evil-leader/set-key-for-mode 'clojure-mode "mgv" 'cider-jump-to-var)
        (evil-leader/set-key-for-mode 'clojure-mode "mgr" 'cider-jump-to-resource)
        (evil-leader/set-key-for-mode 'clojure-mode "mge" 'cider-jump-to-compilation-error)
        (evil-leader/set-key-for-mode 'clojure-mode "mgs" 'cider-jump)
        (evil-leader/set-key-for-mode 'clojure-mode "mtt" 'cider-test-run-tests))))

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
