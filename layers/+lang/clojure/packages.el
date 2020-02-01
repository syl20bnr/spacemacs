;;; packages.el --- Clojure Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq clojure-packages
      '(
        cider
        cider-eval-sexp-fu
        (clj-refactor :toggle clojure-enable-clj-refactor)
        clojure-mode
        (clojure-snippets :toggle (configuration-layer/layer-used-p 'auto-completion))
        company
        eldoc
        evil-cleverparens
        flycheck
        (flycheck-clojure :toggle (memq 'squiggly (if (listp clojure-enable-linters)
                                                      clojure-enable-linters
                                                    (list clojure-enable-linters))))
        (flycheck-clj-kondo :toggle (memq 'clj-kondo (if (listp clojure-enable-linters)
                                                         clojure-enable-linters
                                                       (list clojure-enable-linters))))
        (flycheck-joker :toggle (memq 'joker (if (listp clojure-enable-linters)
                                                 clojure-enable-linters
                                               (list clojure-enable-linters))))
        ggtags
        counsel-gtags
        helm-gtags
        org
        parinfer
        popwin
        (sayid :toggle clojure-enable-sayid)
        smartparens
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
            cider-repl-use-clojure-font-lock t
            cider-repl-history-file (concat spacemacs-cache-directory "cider-repl-history"))
      (add-hook 'clojure-mode-hook 'cider-mode)

      (dolist (x '(spacemacs-jump-handlers-clojure-mode
                   spacemacs-jump-handlers-clojurec-mode
                   spacemacs-jump-handlers-clojurescript-mode
                   spacemacs-jump-handlers-clojurex-mode
                   spacemacs-jump-handlers-cider-repl-mode))
        (add-to-list x '(spacemacs/clj-find-var :async t)))

      ;; TODO: having this work for cider-macroexpansion-mode would be nice,
      ;;       but the problem is that it uses clojure-mode as its major-mode
      (let ((cider--key-binding-prefixes
             '(("m=" . "format")
               ("m=e" . "edn")
               ("md" . "debug")
               ("me" . "evaluation")
               ("mg" . "goto")
               ("mh" . "documentation")
               ("mm" . "manage repls")
               ("mml" . "link session")
               ("mmS" . "sibling sessions")
               ("mmq" . "quit/restart")
               ("mp" . "profile")
               ("ms" . "send to repl")
               ("msc" . "connect external repl")
               ("msj" . "jack-in")
               ("msq" . "quit/restart repl")
               ("mt" . "test")
               ("mT" . "toggle")
               )))
        (spacemacs|forall-clojure-modes m
          (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                              m (car x) (cdr x)))
                cider--key-binding-prefixes)

          (spacemacs/set-leader-keys-for-major-mode m

            ;; shortcuts
            "'"  'sesman-start

            ;; help / documentation
            "ha" 'cider-apropos
            "hc" 'cider-cheatsheet
            "hd" 'cider-clojuredocs
            "hh" 'cider-doc
            "hj" 'cider-javadoc
            "hn" 'cider-browse-ns
            "hN" 'cider-browse-ns-all

            ;; evaluate in source code buffer
            "e;" 'cider-eval-defun-to-comment
            "eb" 'cider-eval-buffer
            "ee" 'cider-eval-last-sexp
            "ef" 'cider-eval-defun-at-point
            "ei" 'cider-interrupt
            "em" 'cider-macroexpand-1
            "eM" 'cider-macroexpand-all
            "en" 'cider-ns-refresh
            "eN" 'cider-ns-reload  ;; SPC u for cider-ns-reload-all
            "ep" 'cider-pprint-eval-defun-at-point
            "eP" 'cider-pprint-eval-last-sexp
            "er" 'cider-eval-region
            "eu" 'cider-undef
            "ev" 'cider-eval-sexp-at-point
            "eV" 'cider-eval-sexp-up-to-point
            "ew" 'cider-eval-last-sexp-and-replace

            ;; format code style
            "==" 'cider-format-buffer
            "=eb" 'cider-format-edn-buffer
            "=ee" 'cider-format-edn-last-sexp
            "=er" 'cider-format-edn-region
            "=f" 'cider-format-defun
            "=r" 'cider-format-region

            ;; goto
            "gb" 'cider-pop-back
            "gc" 'cider-classpath
            "gg" 'spacemacs/clj-find-var
            "ge" 'cider-jump-to-compilation-error
            "gn" 'cider-find-ns
            "gr" 'cider-find-resource
            "gs" 'cider-browse-spec
            "gS" 'cider-browse-spec-all

            ;; manage cider connections / sesman
            "mb" 'sesman-browser
            "mi" 'sesman-info
            "mg" 'sesman-goto
            "mlb" 'sesman-link-with-buffer
            "mld" 'sesman-link-with-directory
            "mlu" 'sesman-unlink
            "mqq" 'sesman-quit
            "mqr" 'sesman-restart
            "mlp" 'sesman-link-with-project
            "mSj" 'cider-connect-sibling-clj
            "mSs" 'cider-connect-sibling-cljs
            "ms" 'sesman-start

            ;; send code - spacemacs convention
            "sa" (if (eq m 'cider-repl-mode)
                     'cider-switch-to-last-clojure-buffer
                   'cider-switch-to-repl-buffer)
            "sb" 'cider-load-buffer
            "sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
            "scj" 'cider-connect-clj
            "scm" 'cider-connect-clj&cljs
            "scs" 'cider-connect-cljs
            "se" 'spacemacs/cider-send-last-sexp-to-repl
            "sE" 'spacemacs/cider-send-last-sexp-to-repl-focus
            "sf" 'spacemacs/cider-send-function-to-repl
            "sF" 'spacemacs/cider-send-function-to-repl-focus
            "si" 'sesman-start
            "sjj" 'cider-jack-in-clj
            "sjm" 'cider-jack-in-clj&cljs
            "sjs" 'cider-jack-in-cljs
            "sl" 'cider-repl-clear-buffer
            "sL" 'cider-find-and-clear-repl-output
            "sn" 'spacemacs/cider-send-ns-form-to-repl
            "sN" 'spacemacs/cider-send-ns-form-to-repl-focus
            "so" 'cider-repl-switch-to-other
            "sqq" 'cider-quit
            "sqr" 'cider-restart
            "sqn" 'cider-ns-reload
            "sqN" 'cider-ns-reload-all
            "sr" 'spacemacs/cider-send-region-to-repl
            "sR" 'spacemacs/cider-send-region-to-repl-focus
            "su" 'cider-repl-require-repl-utils

            ;; toggle options
            "Te" 'cider-enlighten-mode
            "Tf" 'spacemacs/cider-toggle-repl-font-locking
            "Tp" 'spacemacs/cider-toggle-repl-pretty-printing
            "Tt" 'cider-auto-test-mode

            ;; cider-tests
            "ta" 'spacemacs/cider-test-run-all-tests
            "tb" 'cider-test-show-report
            "tl" 'spacemacs/cider-test-run-loaded-tests
            "tn" 'spacemacs/cider-test-run-ns-tests
            "tp" 'spacemacs/cider-test-run-project-tests
            "tr" 'spacemacs/cider-test-rerun-failed-tests
            "tt" 'spacemacs/cider-test-run-focused-test

            ;; cider-debug
            "db" 'cider-debug-defun-at-point
            "de" 'spacemacs/cider-display-error-buffer
            "dv" 'cider-inspect

            ;; profile
            "p+" 'cider-profile-samples
            "pc" 'cider-profile-clear
            "pn" 'cider-profile-ns-toggle
            "ps" 'cider-profile-var-summary
            "pS" 'cider-profile-summary
            "pt" 'cider-profile-toggle
            "pv" 'cider-profile-var-profiled-p
            )))

      ;; cider-repl-mode only
      (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
        "," 'cider-repl-handle-shortcut)
      (spacemacs/set-leader-keys-for-major-mode 'cider-clojure-interaction-mode
        "ep" 'cider-eval-print-last-sexp))
    :config
    (progn
      ;; add support for golden-ratio
      (with-eval-after-load 'golden-ratio
        (add-to-list 'golden-ratio-extra-commands 'cider-popup-buffer-quit-function))
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
        (kbd "N") 'cider-inspector-prev-page
        (kbd "p") 'cider-inspector-prev-page
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

      (evil-define-key 'normal cider-repl-mode-map
        (kbd "C-j") 'cider-repl-next-input
        (kbd "C-k") 'cider-repl-previous-input
        (kbd "RET") 'cider-repl-return)

      (if (package-installed-p 'company)
          (evil-define-key 'insert cider-repl-mode-map
            (kbd "C-j") 'spacemacs//clj-repl-wrap-c-j
            (kbd "C-k") 'spacemacs//clj-repl-wrap-c-k)
        (evil-define-key 'insert cider-repl-mode-map
          (kbd "C-j") 'cider-repl-next-input
          (kbd "C-k") 'cider-repl-previous-input))

      (evil-define-key 'insert cider-repl-mode-map
        (kbd "<C-return>") 'cider-repl-newline-and-indent)

      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'cider-repl-mode)
        (clojure/fancify-symbols 'cider-clojure-interaction-mode)))

    (defadvice cider-find-var (before add-evil-jump activate)
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
      ;; Usually we do not set keybindings in :config, however this must be done
      ;; here because it reads the variable `cljr--all-helpers'. Since
      ;; `clj-refactor-mode' is added to the hook, this should trigger when a
      ;; clojure buffer is opened anyway, so there's no "keybinding delay".
      (spacemacs|forall-clojure-modes m
        (dolist (r cljr--all-helpers)
          (let* ((binding (car r))
                 (func (cadr r)))
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
      (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
      ;; Define all the prefixes here, although most of them apply only to bindings in clj-refactor
      (let ((clj-refactor--key-binding-prefixes
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
               ("mru" . "unwind/update"))))
        (spacemacs|forall-clojure-modes m
          (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                              m (car x) (cdr x)))
                clj-refactor--key-binding-prefixes)
          (spacemacs/set-leader-keys-for-major-mode m
            "=l" 'clojure-align
            "rci" 'clojure-cycle-if
            "rcp" 'clojure-cycle-privacy
            "rc#" 'clojure-convert-collection-to-set
            "rc'" 'clojure-convert-collection-to-quoted-list
            "rc(" 'clojure-convert-collection-to-list
            "rc[" 'clojure-convert-collection-to-vector
            "rc{" 'clojure-convert-collection-to-map
            "rc:" 'clojure-toggle-keyword-string
            "rtf" 'clojure-thread-first-all
            "rth" 'clojure-thread
            "rtl" 'clojure-thread-last-all
            "rua" 'clojure-unwind-all
            "ruw" 'clojure-unwind)
          (unless clojure-enable-clj-refactor
            (spacemacs/set-leader-keys-for-major-mode m
              "r?" 'spacemacs/clj-describe-missing-refactorings)))))
    :config
    (when clojure-enable-fancify-symbols
      (spacemacs|forall-clojure-modes m
        (clojure/fancify-symbols m)))))

(defun clojure/post-init-eldoc ()
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-clojure-interaction-mode-hook 'eldoc-mode))

(defun clojure/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (spacemacs|forall-clojure-modes m
      (add-to-list 'evil-lisp-safe-structural-editing-modes m))))

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

(defun clojure/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'clojure-mode))

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
      (spacemacs|forall-clojure-modes m
        (mapc (lambda (x) (spacemacs/declare-prefix-for-mode m
                            (car x) (cdr x)))
              sayid--key-binding-prefixes)
        (spacemacs/set-leader-keys-for-major-mode m
          ;;These keybindings mostly preserved from the default sayid bindings
          "d!" 'sayid-load-enable-clear
          "dE" 'sayid-eval-last-sexp ;in default sayid bindings this is lowercase e, but that was already used in clojure mode
          "dc" 'sayid-clear-log
          "df" 'sayid-query-form-at-point
          "dh" 'sayid-show-help
          "ds" 'sayid-show-traced
          "dS" 'sayid-show-traced-ns
          "dtb" 'sayid-trace-ns-in-file
          "dtd" 'sayid-trace-fn-disable
          "dtD" 'sayid-trace-disable-all
          "dte" 'sayid-trace-fn-enable
          "dtE" 'sayid-trace-enable-all
          "dtK" 'sayid-kill-all-traces
          "dtn" 'sayid-inner-trace-fn
          "dto" 'sayid-outer-trace-fn
          "dtp" 'sayid-trace-ns-by-pattern
          "dtr" 'sayid-remove-trace-fn
          "dty" 'sayid-trace-all-ns-in-dir
          "dV" 'sayid-set-view
          "dw" 'sayid-get-workspace
          "dx" 'sayid-reset-workspace
          ))

      (evilified-state-evilify sayid-mode sayid-mode-map
        (kbd "H") 'sayid-buf-show-help
        (kbd "n") 'sayid-buffer-nav-to-next
        (kbd "N") 'sayid-buffer-nav-to-prev
        (kbd "C-s v") 'sayid-toggle-view
        (kbd "C-s V") 'sayid-set-view
        (kbd "L") 'sayid-buf-back
        (kbd "e") 'sayid-gen-instance-expr) ;Originally this was bound to 'g', but I feel this is still mnemonic and doesn't overlap with evil

      (evilified-state-evilify sayid-pprint-mode sayid-pprint-mode-map
        (kbd "h") 'sayid-pprint-buf-show-help
        (kbd "n") 'sayid-pprint-buf-next
        (kbd "N") 'sayid-pprint-buf-prev
        (kbd "l") 'sayid-pprint-buf-exit)

      (evilified-state-evilify sayid-traced-mode sayid-traced-mode-map
        (kbd "l") 'sayid-show-traced
        (kbd "h") 'sayid-traced-buf-show-help))))

(defun clojure/post-init-parinfer ()
  (add-hook 'clojure-mode-hook 'parinfer-mode))

(defun clojure/post-init-flycheck ()
  ;; When user has chosen to use multiple linters.
  (when (> (safe-length clojure-enable-linters) 1)
    ;; If adding a linter, you must add to checkers-per-mode for each mode
    ;; it can support the mapping from the linter name in clojure-enable-linters
    ;; to the flycheck checker to use to add to flycheck.
    (let* ((checkers-per-mode '((clj . ((clj-kondo . clj-kondo-clj)
                                        (joker . clojure-joker)
                                        (squiggly . clojure-cider-eastwood)))
                                (cljc . ((clj-kondo . clj-kondo-cljc)
                                         (joker . clojure-joker)))
                                (cljs . ((clj-kondo . clj-kondo-cljs)
                                         (joker . clojurescript-joker)))
                                (edn . ((clj-kondo . clj-kondo-edn)
                                        (joker . edn-joker))))))
      ;; For each checker mode
      (dolist (mode-checkers checkers-per-mode)
        ;; We find the first checker in order from the user configured linters which
        ;; the mode supports and make it the primary-linter. All other linters after that
        ;; the mode support is made a next-linter. Finally, we extract the checkers of the
        ;; primary linter and the next linters.
        (let* ((checkers (cdr mode-checkers))
               (primary-linter (seq-find (lambda (l)
                                           (assq l checkers))
                                         clojure-enable-linters))
               (primary-checker (cdr (assq primary-linter checkers)))
               (next-linters (seq-filter (lambda (l)
                                           (and (not (eq l primary-linter))
                                                (assq l checkers)))
                                         clojure-enable-linters))
               (next-checkers (mapcar (lambda (l)
                                        (cdr (assq l checkers)))
                                      next-linters)))
          ;; Move primary checker to the front of flycheck lists of checkers so that
          ;; it is used as the primary checker, because flycheck picks the first one
          ;; it finds.
          (delq primary-checker flycheck-checkers)
          (push primary-checker flycheck-checkers)
          ;; For every checker, set their next checkers, starting with the primary
          ;; checker which has all others has a next-checker, and then the next
          ;; one has all the ones after it, and so on, until the last one which
          ;; has no next-checker to be added. This is because flycheck next-checkers
          ;; must be nested if we want more than two to run. It will pick the first
          ;; available next-checker from next-checkers and run that after. If we want
          ;; a checker after that one, it must also have next-checkers configured.
          (let ((checkers-to-add next-checkers))
            (dolist (checker (cons primary-checker next-checkers))
              (dolist (next-checker checkers-to-add)
                (flycheck-add-next-checker checker next-checker t))
              (setq checkers-to-add (cdr checkers-to-add))))))))
  (spacemacs|forall-clojure-modes m
    (spacemacs/enable-flycheck m)))

(defun clojure/init-flycheck-clojure ()
  (use-package flycheck-clojure
    :if (configuration-layer/package-usedp 'flycheck)
    :config (progn
              (flycheck-clojure-setup)
              (with-eval-after-load 'cider
                (flycheck-clojure-inject-jack-in-dependencies)))))

(defun clojure/init-flycheck-clj-kondo ()
  (use-package flycheck-clj-kondo
    :if (configuration-layer/package-usedp 'flycheck)))

(defun clojure/init-flycheck-joker ()
  (use-package flycheck-joker
    :if (configuration-layer/package-usedp 'flycheck)))
