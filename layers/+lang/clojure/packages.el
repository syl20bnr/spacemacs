(setq clojure-packages
  '(
    cider
    cider-eval-sexp-fu
    clj-refactor
    clojure-mode
    (clojure-snippets :toggle (configuration-layer/layer-usedp 'auto-completion))
    company
    eldoc
    ggtags
    helm-gtags
    popwin
    smartparens
    subword
    org
    sayid
    ))

(defun clojure/init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'cider 'cider-jack-in "cider")
      (setq cider-stacktrace-default-filters '(tooling dup)
            cider-repl-pop-to-buffer-on-connect nil
            cider-prompt-save-file-on-load nil
            cider-repl-use-clojure-font-lock t
            cider-repl-history-file (concat spacemacs-cache-directory "cider-repl-history"))
      (push "\\*cider-repl\.\+\\*" spacemacs-useful-buffers-regexp)
      (add-hook 'clojure-mode-hook 'cider-mode)
      (dolist (x '(spacemacs-jump-handlers-clojure-mode
                   spacemacs-jump-handlers-clojurec-mode
                   spacemacs-jump-handlers-clojurescript-mode
                   spacemacs-jump-handlers-clojurex-mode
                   spacemacs-jump-handlers-cider-repl-mode))
        (add-to-list x 'cider-find-var)))
    :config
    (progn
      ;; add support for golden-ratio
      (with-eval-after-load 'golden-ratio
        (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))
      ;; add support for evil
      (evil-set-initial-state 'cider-stacktrace-mode 'motion)
      (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
      (add-hook 'cider--debug-mode-hook 'spacemacs/cider-debug-setup)

      (evilified-state-evilify cider-stacktrace-mode cider-stacktrace-mode-map
        (kbd "C-j") 'cider-stacktrace-next-cause
        (kbd "C-k") 'cider-stacktrace-previous-cause
        (kbd "TAB") 'cider-stacktrace-cycle-current-cause
        (kbd "0")   'cider-stacktrace-cycle-all-causes
        (kbd "1")   'cider-stacktrace-cycle-cause-1
        (kbd "2")   'cider-stacktrace-cycle-cause-2
        (kbd "3")   'cider-stacktrace-cycle-cause-3
        (kbd "4")   'cider-stacktrace-cycle-cause-4
        (kbd "5")   'cider-stacktrace-cycle-cause-5
        (kbd "a")   'cider-stacktrace-toggle-all
        (kbd "c")   'cider-stacktrace-toggle-clj
        (kbd "d")   'cider-stacktrace-toggle-duplicates
        (kbd "J")   'cider-stacktrace-toggle-java
        (kbd "r")   'cider-stacktrace-toggle-repl
        (kbd "T")   'cider-stacktrace-toggle-tooling)

      ;; open cider-doc directly and close it with q
      (setq cider-prompt-for-symbol nil)

      (evilified-state-evilify cider-docview-mode cider-docview-mode-map
        (kbd "q") 'cider-popup-buffer-quit)

      (evilified-state-evilify cider-inspector-mode cider-inspector-mode-map
        (kbd "L") 'cider-inspector-pop
        (kbd "n") 'cider-inspector-next-page
        (kbd "N") 'cider-inspector-previous-page
        (kbd "r") 'cider-inspector-refresh)

      (evilified-state-evilify cider-test-report-mode cider-test-report-mode-map
        (kbd "C-j") 'cider-test-next-result
        (kbd "C-k") 'cider-test-previous-result
        (kbd "RET") 'cider-test-jump
        (kbd "d")   'cider-test-ediff
        (kbd "e")   'cider-test-stacktrace
        (kbd "q")   'cider-popup-buffer-quit
        (kbd "r")   'cider-test-rerun-tests
        (kbd "t")   'cider-test-run-test
        (kbd "T")   'cider-test-run-ns-tests)

      ;; TODO: having this work for cider-macroexpansion-mode would be nice,
      ;;       but the problem is that it uses clojure-mode as its major-mode

      (setq cider--key-binding-prefixes
            '(("md" . "debug")
              ("me" . "evaluation")
              ("mg" . "goto")
              ("mh" . "documentation")
              ("ms" . "repl")
              ("mt" . "test")
              ("mT" . "toggle")
              ("mf" . "format")))
      (dolist (m '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   cider-repl-mode
                   cider-clojure-interaction-mode))
        (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                           m (car x) (cdr x)))
              cider--key-binding-prefixes)

        (spacemacs/set-leader-keys-for-major-mode m
          "ha" 'cider-apropos
          "hh" 'cider-doc
          "hg" 'cider-grimoire
          "hj" 'cider-javadoc
          "hn" 'cider-browse-ns

          "e;" 'cider-eval-defun-to-comment
          "eb" 'cider-eval-buffer
          "ee" 'cider-eval-last-sexp
          "ef" 'cider-eval-defun-at-point
          "em" 'cider-macroexpand-1
          "eM" 'cider-macroexpand-all
          "er" 'cider-eval-region
          "ew" 'cider-eval-last-sexp-and-replace

          "="  'cider-format-buffer
          "fb" 'cider-format-buffer

          "gb" 'cider-pop-back
          "gc" 'cider-classpath
          "ge" 'cider-jump-to-compilation-error
          "gr" 'cider-jump-to-resource
          "gn" 'cider-browse-ns
          "gN" 'cider-browse-ns-all

          "'"  'cider-jack-in
          "\""  'cider-jack-in-clojurescript
          "sb" 'cider-load-buffer
          "sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
          "sc" (if (eq m 'cider-repl-mode)
                   'cider-repl-clear-buffer
                 'cider-connect)
          "sC" 'cider-find-and-clear-repl-output
          "se" 'spacemacs/cider-send-last-sexp-to-repl
          "sE" 'spacemacs/cider-send-last-sexp-to-repl-focus
          "sf" 'spacemacs/cider-send-function-to-repl
          "sF" 'spacemacs/cider-send-function-to-repl-focus
          "si" 'cider-jack-in
          "sI" 'cider-jack-in-clojurescript
          "sn" 'spacemacs/cider-send-ns-form-to-repl
          "sN" 'spacemacs/cider-send-ns-form-to-repl-focus
          "so" 'cider-repl-switch-to-other
          "sq" 'cider-quit
          "sr" 'spacemacs/cider-send-region-to-repl
          "sR" 'spacemacs/cider-send-region-to-repl-focus
          "ss" (if (eq m 'cider-repl-mode)
                   'cider-switch-to-last-clojure-buffer
                 'cider-switch-to-repl-buffer)
          "sx" 'cider-refresh

          "Te" 'cider-enlighten-mode
          "Tf" 'spacemacs/cider-toggle-repl-font-locking
          "Tp" 'spacemacs/cider-toggle-repl-pretty-printing
          "Tt" 'cider-auto-test-mode

          "ta" 'spacemacs/cider-test-run-all-tests
          "tb" 'cider-test-show-report
          "tl" 'spacemacs/cider-test-run-loaded-tests
          "tp" 'spacemacs/cider-test-run-project-tests
          "tn" 'spacemacs/cider-test-run-ns-tests
          "tr" 'spacemacs/cider-test-rerun-failed-tests
          "tt" 'spacemacs/cider-test-run-focused-test

          "db" 'cider-debug-defun-at-point
          "de" 'spacemacs/cider-display-error-buffer
          "dv" 'cider-inspect

          ;; refactorings from clojure-mode
          "rc{" 'clojure-convert-collection-to-map
          "rc(" 'clojure-convert-collection-to-list
          "rc'" 'clojure-convert-collection-to-quoted-list
          "rc#" 'clojure-convert-collection-to-set
          "rc[" 'clojure-convert-collection-to-vector))

      ;; cider-repl-mode only
      (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
        "," 'cider-repl-handle-shortcut)

      (spacemacs/set-leader-keys-for-major-mode 'cider-clojure-interaction-mode
        "ep" 'cider-eval-print-last-sexp)

      (evil-define-key 'normal cider-repl-mode-map
        (kbd "C-j") 'cider-repl-next-input
        (kbd "C-k") 'cider-repl-previous-input)

      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'cider-repl-mode)
        (clojure/fancify-symbols 'cider-clojure-interaction-mode)))

    (defadvice cider-jump-to-var (before add-evil-jump activate)
      (evil-set-jump))))

(defun clojure/init-cider-eval-sexp-fu ()
  (with-eval-after-load 'eval-sexp-fu
    (require 'cider-eval-sexp-fu)))

(defun clojure/init-clj-refactor ()
  (use-package clj-refactor
    :defer t
    :init
    (add-hook 'clojure-mode-hook 'clj-refactor-mode)
    :config
    (progn
      (cljr-add-keybindings-with-prefix "C-c C-f")

      (setq clj-refactor--key-binding-prefixes
            '(("mr" . "refactor")
              ("mra" . "add")
              ("mrc" . "cycle/clean/convert")
              ("mrd" . "destructure")
              ("mre" . "extract/expand")
              ("mrf" . "find/function")
              ("mrh" . "hotload")
              ("mri" . "introduce/inline")
              ("mrm" . "move")
              ("mrp" . "project/promote")
              ("mrr" . "remove/rename/replace")
              ("mrs" . "show/sort/stop")
              ("mrt" . "thread")
              ("mru" . "unwind/update")))
      (dolist (m '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   cider-repl-mode
                   cider-clojure-interaction-mode))
        (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                           m (car x) (cdr x)))
              clj-refactor--key-binding-prefixes)
        (dolist (r cljr--all-helpers)
          (let* ((binding (car r))
                 (func (car (cdr r))))
            (when (not (string-prefix-p "hydra" (symbol-name func)))
              (spacemacs/set-leader-keys-for-major-mode m
                (concat "r" binding) func))))))))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
      ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
      (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode)))
    :config
    (progn
      (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
        (spacemacs/set-leader-keys-for-major-mode m
          "fl" 'clojure-align))

      (when clojure-enable-fancify-symbols
        (dolist (m '(clojure-mode clojurescript-mode clojurec-mode clojurex-mode))
          (clojure/fancify-symbols m))))))

(defun clojure/post-init-eldoc ()
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-clojure-interaction-mode-hook 'eldoc-mode))

(defun clojure/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*cider-error*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*cider-doc*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)))

(defun clojure/post-init-smartparens ()
  (add-hook 'cider-repl-mode-hook
            (if dotspacemacs-smartparens-strict-mode
                #'smartparens-strict-mode
              #'smartparens-mode))
  (with-eval-after-load 'smartparens
    (sp-local-pair 'clojure-mode "`" nil :actions nil)))

(defun clojure/post-init-subword ()
  (add-hook 'cider-mode-hook 'subword-mode))

(defun clojure/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes
    cider-mode
    cider-repl-mode))

(defun clojure/post-init-ggtags ()
  (add-hook 'clojure-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun clojure/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'clojure-mode))

(defun clojure/init-clojure-snippets ()
  (use-package clojure-snippets
    :defer t))

(defun clojure/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(clojure . t))
    (setq org-babel-clojure-backend 'cider)))

(defun clojure/init-sayid ()
  (use-package sayid
    :defer t
    :init
    (progn
      (setq sayid--key-binding-prefixes
            '(("mdt" . "trace")))
      (dolist (m '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   cider-repl-mode
                   cider-clojure-interaction-mode))
        (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                           m (car x) (cdr x)))
              sayid--key-binding-prefixes)
        (spacemacs/set-leader-keys-for-major-mode m
          ;;These keybindings mostly preserved from the default sayid bindings
          "df" 'sayid-query-form-at-point
          "dw" 'sayid-get-workspace
          "dE" 'sayid-eval-last-sexp ;in default sayid bindings this is lowercase e, but that was already used in clojure mode
          "d!" 'sayid-load-enable-clear
          "dc" 'sayid-clear-log
          "dx" 'sayid-reset-workspace
          "ds" 'sayid-show-traced
          "dS" 'sayid-show-traced-ns
          "dV" 'sayid-set-view
          "dh" 'sayid-show-help
          "dty" 'sayid-trace-all-ns-in-dir
          "dtp" 'sayid-trace-ns-by-pattern
          "dtb" 'sayid-trace-ns-in-file
          "dte" 'sayid-trace-fn-enable
          "dtE" 'sayid-trace-enable-all
          "dtd" 'sayid-trace-fn-disable
          "dtD" 'sayid-trace-disable-all
          "dtn" 'sayid-inner-trace-fn
          "dto" 'sayid-outer-trace-fn
          "dtr" 'sayid-remove-trace-fn
          "dtK" 'sayid-kill-all-traces))

      (evilified-state-evilify sayid-mode sayid-mode-map
        (kbd "H") 'sayid-buf-show-help
        (kbd "n") 'sayid-buffer-nav-to-next
        (kbd "N") 'sayid-buffer-nav-to-prev
        (kbd "C-s v") 'sayid-toggle-view
        (kbd "C-s V") 'sayid-set-view
        (kbd "L") 'sayid-buf-back
        (kbd "e") 'sayid-gen-instance-expr ;Originally this was bound to 'g', but I feel this is still mnemonic and doesn't overlap with evil
        )
      (evilified-state-evilify sayid-pprint-mode sayid-pprint-mode-map
        (kbd "h") 'sayid-pprint-buf-show-help
        (kbd "n") 'sayid-pprint-buf-next
        (kbd "N") 'sayid-pprint-buf-prev
        (kbd "l") 'sayid-pprint-buf-exit)

      (evilified-state-evilify sayid-traced-mode sayid-traced-mode-map
        (kbd "l") 'sayid-show-traced
        (kbd "h") 'sayid-traced-buf-show-help))))
