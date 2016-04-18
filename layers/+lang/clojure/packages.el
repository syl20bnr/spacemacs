(setq clojure-packages
  '(
    cider
    cider-eval-sexp-fu
    clj-refactor
    clojure-mode
    (clojure-snippets :toggle (configuration-layer/layer-usedp 'auto-completion))
    company
    eldoc
    popwin
    subword
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
            cider-repl-use-clojure-font-lock t)
      (push "\\*cider-repl\.\+\\*" spacemacs-useful-buffers-regexp)
      (add-hook 'clojure-mode-hook 'cider-mode)
      (if dotspacemacs-smartparens-strict-mode
          (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)))
    :config
    (progn
      ;; add support for golden-ratio
      (with-eval-after-load 'golden-ratio
        (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))
      ;; add support for evil
      (evil-set-initial-state 'cider-stacktrace-mode 'motion)
      (evil-set-initial-state 'cider-popup-buffer-mode 'motion)

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
        (spacemacs//cider-eval-in-repl-no-focus (cider-test-run-ns-tests nil)))

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
                   cider-repl-mode))
        (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                           m (car x) (cdr x)))
              cider--key-binding-prefixes)
        (spacemacs/set-leader-keys-for-major-mode m
          "ha" 'cider-apropos
          "hh" 'cider-doc
          "hg" 'cider-grimoire
          "hj" 'cider-javadoc

          "eb" 'cider-eval-buffer
          "ee" 'cider-eval-last-sexp
          "ef" 'cider-eval-defun-at-point
          "er" 'cider-eval-region
          "ew" 'cider-eval-last-sexp-and-replace

          "fb" 'cider-format-buffer

          "gb" 'cider-pop-back
          "ge" 'cider-jump-to-compilation-error
          "gg" 'cider-find-var
          "gr" 'cider-jump-to-resource

          "'"  'cider-jack-in
          "sb" 'cider-load-buffer
          "sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
          "sc" 'cider-connect
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
          "ss" 'cider-switch-to-repl-buffer
          "sx" 'cider-refresh

          "Tf" 'spacemacs/cider-toggle-repl-font-locking
          "Tp" 'spacemacs/cider-toggle-repl-pretty-printing

          "ta" 'spacemacs/cider-test-run-all-tests
          "tr" 'spacemacs/cider-test-rerun-tests
          "tt" 'spacemacs/cider-test-run-focused-test

          "db" 'cider-debug-defun-at-point
          "de" 'spacemacs/cider-display-error-buffer
          "di" 'cider-inspect))

      (evil-define-key 'normal cider-repl-mode-map
        "C-j" 'cider-repl-next-input
        "C-k" 'cider-repl-previous-input)

      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'cider-repl-mode)))

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
              ("mrc" . "cycle/clean")
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
                   cider-repl-mode))
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
        (spacemacs/set-leader-keys-for-major-mode m
          "Ti" 'spacemacs/clojure-mode-toggle-default-indent-style
          "fl" 'clojure-align))

      (when clojure-enable-fancify-symbols
        (dolist (m '(clojure-mode clojurescript-mode clojurec-mode clojurex-mode))
          (clojure/fancify-symbols m))))))

(defun clojure/post-init-eldoc ()
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode))

(defun clojure/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*cider-error*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*cider-doc*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)))

(defun clojure/post-init-subword ()
  (add-hook 'cider-mode-hook 'subword-mode))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun clojure/post-init-company ()
    (push 'company-capf company-backends-cider-mode)
    (spacemacs|add-company-hook cider-mode)

    (push 'company-capf company-backends-cider-repl-mode)
    (spacemacs|add-company-hook cider-repl-mode)))

(defun clojure/init-clojure-snippets ()
  (use-package clojure-snippets
    :defer t))
