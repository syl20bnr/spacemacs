(setq clojure-packages
  '(
    align-cljlet
    cider
    cider-eval-sexp-fu
    clj-refactor
    clojure-mode
    company
    popwin
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
        (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
          (setq form (replace-match "" t t form)))
        (with-current-buffer (cider-current-repl-buffer)
          (let ((pt-max (point-max)))
            (goto-char pt-max)
            (insert form)
            (indent-region pt-max (point))
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
        (interactive)
        (setq cider-repl-use-pretty-printing
              (if cider-repl-use-pretty-printing nil t))
        (message "Cider REPL pretty printing: %s"
                 (if cider-repl-use-pretty-printing "ON" "OFF")))

      (defun spacemacs/cider-toggle-repl-font-locking ()
        (interactive)
        (setq cider-repl-use-clojure-font-lock
              (if cider-repl-use-pretty-printing nil t))
        (message "Cider REPL clojure-mode font-lock: %s"
                 (if cider-repl-use-clojure-font-lock "ON" "OFF")))

      (defun spacemacs/cider-debug-setup ()
        (when (eq dotspacemacs-editing-style 'vim)
          (evil-make-overriding-map cider--debug-mode-map 'normal)
          (evil-normalize-keymaps)))

      (add-hook 'cider--debug-mode-hook 'spacemacs/cider-debug-setup)

      (evilify cider-stacktrace-mode cider-stacktrace-mode-map
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

      (evilify cider-docview-mode cider-docview-mode-map
               (kbd "q") 'cider-popup-buffer-quit)

      (evilify cider-inspector-mode cider-inspector-mode-map
               (kbd "L") 'cider-inspector-pop
               (kbd "n") 'cider-inspector-next-page
               (kbd "N") 'cider-inspector-previous-page
               (kbd "r") 'cider-inspector-refresh)

      (evilify cider-test-report-mode cider-test-report-mode-map
               (kbd "C-j") 'cider-test-next-result
               (kbd "C-k") 'cider-test-previous-result
               (kbd "RET") 'cider-test-jump
               (kbd "d")   'cider-test-ediff
               (kbd "e")   'cider-test-stacktrace
               (kbd "q")   'cider-popup-buffer-quit
               (kbd "r")   'cider-test-rerun-tests
               (kbd "t")   'cider-test-run-test
               (kbd "T")   'cider-test-run-tests)

      ;; TODO: having this work for cider-macroexpansion-mode would be nice,
      ;;       but the problem is that it uses clojure-mode as its major-mode

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
          "mgg" 'cider-find-var
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
          "msx" 'cider-refresh

          "mTf" 'spacemacs/cider-toggle-repl-font-locking
          "mTp" 'spacemacs/cider-toggle-repl-pretty-printing

          "mta" 'spacemacs/cider-test-run-all-tests
          "mtr" 'spacemacs/cider-test-rerun-tests
          "mtt" 'spacemacs/cider-test-run-focused-test

          "mdb" 'cider-debug-defun-at-point
          "mde" 'spacemacs/cider-display-error-buffer
          "mdi" 'cider-inspect))

      (evil-leader/set-key-for-mode 'cider-repl-mode
        "mhh" 'cider-doc
        "mhg" 'cider-grimoire
        "mhj" 'cider-javadoc

        "mee" 'cider-eval-last-sexp
        "mef" 'cider-eval-defun-at-point
        "mer" 'cider-eval-region
        "mew" 'cider-eval-last-sexp-and-replace

        "mgb" 'cider-jump-back
        "mge" 'cider-jump-to-compilation-error
        "mgg" 'cider-find-var
        "mgr" 'cider-jump-to-resource

        "msc" 'cider-repl-clear-buffer
        "msn" 'cider-repl-set-ns
        "msq" 'cider-quit
        "mss" 'cider-switch-to-last-clojure-buffer
        "msx" 'cider-refresh

        "mTf" 'spacemacs/cider-toggle-repl-font-locking
        "mTp" 'spacemacs/cider-toggle-repl-pretty-printing

        "mde" 'spacemacs/cider-display-error-buffer
        "mdi" 'cider-inspect)

      (evil-define-key 'normal cider-repl-mode-map
        "C-j" 'cider-repl-next-input
        "C-k" 'cider-repl-previous-input)

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

      (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
        (evil-leader/set-key-for-mode m
          "mr?"  'cljr-describe-refactoring
          "mrad" 'cljr-add-declaration
          "mrai" 'cljr-add-import-to-ns
          "mram" 'cljr-add-missing-libspec
          "mrap" 'cljr-add-project-dependency
          "mrar" 'cljr-add-require-to-ns
          "mras" 'cljr-add-stubs
          "mrau" 'cljr-add-use-to-ns
          "mrcc" 'cljr-cycle-coll
          "mrci" 'cljr-cycle-if
          "mrcn" 'cljr-clean-ns
          "mrcp" 'cljr-cycle-privacy
          "mrdk" 'cljr-destructure-keys
          "mref" 'cljr-extract-function
          "mrec" 'cljr-extract-constant
          "mrel" 'cljr-expand-let
          "mrfu" 'cljr-find-usages
          "mrfe" 'cljr-create-fn-from-example
          "mrhd" 'cljr-hotload-dependency
          "mril" 'cljr-introduce-let
          "mris" 'cljr-inline-symbol
          "mrmf" 'cljr-move-form
          "mrml" 'cljr-move-to-let
          "mrpc" 'cljr-project-clean
          "mrpf" 'cljr-promote-function
          "mrrd" 'cljr-remove-debug-fns
          "mrrf" 'cljr-rename-file-or-dir
          "mrrl" 'cljr-remove-let
          "mrrr" 'cljr-remove-unused-requires
          "mrrs" 'cljr-rename-symbol
          "mrru" 'cljr-replace-use
          "mrsn" 'cljr-sort-ns
          "mrsp" 'cljr-sort-project-dependencies
          "mrsr" 'cljr-stop-referring
          "mrsc" 'cljr-show-changelog
          "mrtf" 'cljr-thread-first-all
          "mrth" 'cljr-thread
          "mrtl" 'cljr-thread-last-all
          "mrua" 'cljr-unwind-all
          "mrup" 'cljr-update-project-dependencies
          "mruw" 'cljr-unwind))

      (evil-leader/set-key-for-mode 'cider-repl-mode
        "mr?"  'cljr-describe-refactoring
        "mrap" 'cljr-add-project-dependency
        "mras" 'cljr-add-stubs
        "mrcc" 'cljr-cycle-coll
        "mrci" 'cljr-cycle-if
        "mrcp" 'cljr-cycle-privacy
        "mrdk" 'cljr-destructure-keys
        "mrel" 'cljr-expand-let
        "mrfu" 'cljr-find-usages
        "mrhd" 'cljr-hotload-dependency
        "mril" 'cljr-introduce-let
        "mrml" 'cljr-move-to-let
        "mrpc" 'cljr-project-clean
        "mrrl" 'cljr-remove-let
        "mrsp" 'cljr-sort-project-dependencies
        "mrsc" 'cljr-show-changelog
        "mrtf" 'cljr-thread-first-all
        "mrth" 'cljr-thread
        "mrtl" 'cljr-thread-last-all
        "mrua" 'cljr-unwind-all
        "mrup" 'cljr-update-project-dependencies
        "mruw" 'cljr-unwind))))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
      (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode)))
    :config
    (progn

      (defun spacemacs/clojure-mode-toggle-default-indent-style ()
        (interactive)
        (setq clojure-defun-style-default-indent
              (if clojure-defun-style-default-indent nil t))
        (message "Clojure-mode default indent style: %s"
                 (if clojure-defun-style-default-indent "ON" "OFF")))

      (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
        (evil-leader/set-key-for-mode m
          "mTi" 'spacemacs/clojure-mode-toggle-default-indent-style))

      (when clojure-enable-fancify-symbols
        (dolist (m '(clojure-mode clojurescript-mode clojurec-mode clojurex-mode))
          (clojure/fancify-symbols m)))

      (define-clojure-indent
        ;; Compojure
        (ANY 2)
        (DELETE 2)
        (GET 2)
        (HEAD 2)
        (POST 2)
        (PUT 2)
        (context 2)
        (defroutes 'defun)
        ;; Cucumber
        (After 1)
        (Before 1)
        (Given 2)
        (Then 2)
        (When 2)
        ;; Schema
        (s/defrecord 2)
        ;; test.check
        (for-all 'defun)))))

(defun clojure/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*cider-error*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*cider-doc*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)))

(defun clojure/post-init-rainbow-delimiters ()
  (if (configuration-layer/package-usedp 'cider)
      (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)))

(defun clojure/post-init-subword ()
  (unless (version< emacs-version "24.4")
    (add-hook 'cider-mode-hook 'subword-mode)))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun clojure/post-init-company ()
    (push 'company-capf company-backends-cider-mode)
    (spacemacs|add-company-hook cider-mode)

    (push 'company-capf company-backends-cider-repl-mode)
    (spacemacs|add-company-hook cider-repl-mode)))
