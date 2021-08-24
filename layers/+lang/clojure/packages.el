;;; packages.el --- Clojure Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst clojure-packages
  '(
    cider
    cider-eval-sexp-fu
    (clj-refactor :toggle clojure-enable-clj-refactor)
    (helm-cider :toggle (configuration-layer/layer-used-p 'helm))
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
    popwin
    (sayid :toggle clojure-enable-sayid)
    smartparens
    subword))


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
             '(("m=e" . "edn")
               ("md" . "debug")
               ("mdv" . "inspect values")
               ("me" . "evaluation")
               ("men" . "namespace")
               ("mep" . "pretty print")
               ("mm" . "manage repls")
               ("mml" . "link session")
               ("mmS" . "sibling sessions")
               ("mmq" . "quit/restart")
               ("mp" . "profile")
               ("ms" . "send to repl")
               ("msc" . "connect external repl")
               ("msj" . "jack-in")
               ("msq" . "quit/restart repl")
               ("mt" . "test")))
            (cider--key-binding-non-lsp-prefixes
             '(("m=" . "format")
               ("mg" . "goto") ;; no lsp
               ("mh" . "documentation")
               ("mT" . "toggle"))))
        (spacemacs|forall-clojure-modes m
          (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                              m (car x) (cdr x)))
                cider--key-binding-prefixes)
          (unless (eq clojure-backend 'lsp)
            (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                                m (car x) (cdr x)))
                  cider--key-binding-non-lsp-prefixes)
            (spacemacs/set-leader-keys-for-major-mode m
              "hh" 'cider-doc
              "=r" 'cider-format-region
              "ge" 'cider-jump-to-compilation-error
              "gr" 'cider-find-resource
              "gs" 'cider-browse-spec
              "gS" 'cider-browse-spec-all))
          (spacemacs/set-leader-keys-for-major-mode m
            ;; shortcuts
            "'"  'sesman-start
            ;; help / documentation
            "ha" 'cider-apropos
            "hc" 'cider-cheatsheet
            "hd" 'cider-clojuredocs
            "hj" 'cider-javadoc
            "hn" 'cider-browse-ns
            "hN" 'cider-browse-ns-all
            "hs" 'cider-browse-spec
            "hS" 'cider-browse-spec-all
            ;; evaluate in source code buffer
            "e;" 'cider-eval-defun-to-comment
            "e$" 'spacemacs/cider-eval-sexp-end-of-line
            "e(" 'cider-eval-list-at-point
            "eb" 'cider-eval-buffer
            "ee" 'cider-eval-last-sexp
            "ef" 'cider-eval-defun-at-point
            "ei" 'cider-interrupt
            "el" 'spacemacs/cider-eval-sexp-end-of-line
            "em" 'cider-macroexpand-1
            "eM" 'cider-macroexpand-all
            "ena" 'cider-ns-reload-all
            "enn" 'cider-eval-ns-form
            "enr" 'cider-ns-refresh
            "enl" 'cider-ns-reload  ;; SPC u for cider-ns-reload-all
            "ep;" 'cider-pprint-eval-defun-to-comment
            "ep:" 'cider-pprint-eval-last-sexp-to-comment
            "epf" 'cider-pprint-eval-defun-at-point
            "epe" 'cider-pprint-eval-last-sexp
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
            ;; goto
            "gb" 'cider-pop-back
            "gc" 'cider-classpath
            "gg" 'spacemacs/clj-find-var
            "gn" 'cider-find-ns
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
            "sl" 'spacemacs/cider-find-and-clear-repl-buffer
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
            ;; cider-debug and inspect
            "db" 'cider-debug-defun-at-point
            "de" 'spacemacs/cider-display-error-buffer
            "dve" 'cider-inspect-last-sexp
            "dvf" 'cider-inspect-defun-at-point
            "dvi" 'cider-inspect
            "dvl" 'cider-inspect-last-result
            "dvv" 'cider-inspect-expr
            ;; profile
            "p+" 'cider-profile-samples
            "pc" 'cider-profile-clear
            "pn" 'cider-profile-ns-toggle
            "ps" 'cider-profile-var-summary
            "pS" 'cider-profile-summary
            "pt" 'cider-profile-toggle
            "pv" 'cider-profile-var-profiled-p)))

      ;; cider-repl-mode only
      (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
        "," 'cider-repl-handle-shortcut)
      (spacemacs/set-leader-keys-for-major-mode 'cider-clojure-interaction-mode
        "epl" 'cider-eval-print-last-sexp))
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
      (setq cider-prompt-for-symbol t)

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

      (evilified-state-evilify-map cider-repl-history-mode-map
        :mode cider-repl-history-mode
        :bindings
        "j" 'cider-repl-history-forward
        "k" 'cider-repl-history-previous
        "s" (cond ((featurep 'helm-swoop) 'helm-swoop)
                  ((featurep 'swiper) 'swiper)
                  (t 'cider-repl-history-occur))
        "r" 'cider-repl-history-update)

      (spacemacs/set-leader-keys-for-major-mode 'cider-repl-history-mode
        "s" 'cider-repl-history-save)

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
        (kbd "<C-return>") 'cider-repl-newline-and-indent
        (kbd "C-r") 'cider-repl-history)

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
            (unless (string-prefix-p "hydra" (symbol-name func))
              (spacemacs/set-leader-keys-for-major-mode m
                (concat "r" binding) func))))))))

(defun clojure/init-helm-cider ()
  (use-package helm-cider
    :defer t
    :init
    (progn
      (add-hook 'clojure-mode-hook 'helm-cider-mode)
      (setq sayid--key-binding-prefixes
            '(("mhc" . "helm-cider-cheatsheet")))
      (spacemacs|forall-clojure-modes m
        (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                            m (car x) (cdr x)))
              sayid--key-binding-prefixes)
        (spacemacs/set-leader-keys-for-major-mode m
          "hc" 'helm-cider-cheatsheet)))))

(defun clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
      ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
      (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
      (add-hook 'clojure-mode-hook #'spacemacs//clojure-setup-backend)
      ;; Define all the prefixes here, although most of them apply only to bindings in clj-refactor
      (let ((clj-refactor--key-binding-prefixes
             '(("mra" . "add")
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
            (clj-refactor--key-binding-non-lsp-prefixes
             '(("mr" . "refactor"))))
        (spacemacs|forall-clojure-modes m
          (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                              m (car x) (cdr x)))
                clj-refactor--key-binding-prefixes)
          (unless (eq clojure-backend 'lsp)
            (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                                m (car x) (cdr x)))
                  clj-refactor--key-binding-non-lsp-prefixes))
          (spacemacs/set-leader-keys-for-major-mode m
            "=l" 'clojure-align
            "ran" 'clojure-insert-ns-form
            "raN" 'clojure-insert-ns-form-at-point
            "rci" 'clojure-cycle-if
            "rcp" 'clojure-cycle-privacy
            "rc#" 'clojure-convert-collection-to-set
            "rc'" 'clojure-convert-collection-to-quoted-list
            "rc(" 'clojure-convert-collection-to-list
            "rc[" 'clojure-convert-collection-to-vector
            "rc{" 'clojure-convert-collection-to-map
            "rc:" 'clojure-toggle-keyword-string
            "rsn" 'clojure-sort-ns
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
  (add-hook 'cider-repl-mode-hook #'spacemacs//activate-smartparens)
  (with-eval-after-load 'smartparens
    (sp-local-pair 'clojure-mode "`" nil :actions nil)))

(defun clojure/post-init-subword ()
  (add-hook 'cider-mode-hook 'subword-mode))

(defun clojure/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes clojure-mode clojurec-mode clojurescript-mode clojurex-mode cide-clojure-interaction-mode cider-mode cider-repl-mode))

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
          "dx" 'sayid-reset-workspace))


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
        (kbd "h") 'sayid-traced-buf-show-help))
    :config
    (progn
      ;; If sayid-version is null the .elc file
      ;; is corrupted. Then force a reinstall and
      ;; reload the feature.
      (when (null sayid-version)
        (package-reinstall 'sayid)
        (unload-feature 'sayid)
        (require 'sayid)
        (setq cider-jack-in-lein-plugins (delete `("com.billpiel/sayid" nil) cider-jack-in-lein-plugins))))))

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
