;;; packages.el --- e-macs Lisp Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq e-macs-lisp-packages
      '(
        auto-compile
        company
        (debug :location built-in)
        (edebug :location built-in)
        eldoc
        elisp-slime-nav
        (e-macs-lisp :location built-in)
        evil
        evil-cleverparens
        eval-sexp-fu
        flycheck
        flycheck-elsa
        flycheck-package
        ggtags
        counsel-gtags
        helm-gtags
        (ielm :location built-in)
        macrostep
        nameless
        overseer
        parinfer
        rainbow-identifiers
        semantic
        smartparens
        srefactor
        emr
        ))

(defun e-macs-lisp/init-ielm ()
  (use-package ielm
    :defer t
    :init
    (progn
      (space-macs/register-repl 'ielm 'ielm)
      (dolist (mode '(e-macs-lisp-mode lisp-interaction-mode))
        (space-macs/declare-prefix-for-mode mode "ms" "ielm")
        (space-macs/set-leader-keys-for-major-mode mode
          "'" 'ielm
          "si" 'ielm)))
    :config
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))))

(defun e-macs-lisp/post-init-company ()
  (space-macs|add-company-backends :backends company-capf
                                  :modes e-macs-lisp-mode)
  (space-macs|add-company-backends :backends (company-files company-capf)
                                  :modes ielm-mode))

(defun e-macs-lisp/init-debug ()
  (use-package debug
    :defer t
    :init (dolist (mode '(e-macs-lisp-mode lisp-interaction-mode))
            (space-macs/declare-prefix-for-mode mode "md" "debug")
            (space-macs/set-leader-keys-for-major-mode mode
              "dt" 'space-macs/elisp-toggle-debug-expr-and-eval-func))
    :config (evilified-state-evilify-map debugger-mode-map
              :mode debugger-mode)))

(defun e-macs-lisp/init-edebug ()
  (use-package edebug
    :defer t
    :init
    (progn
      ;; key bindings
      (dolist (mode '(e-macs-lisp-mode lisp-interaction-mode))
        (space-macs/set-leader-keys-for-major-mode mode
          "df" 'space-macs/edebug-instrument-defun-on
          "dF" 'space-macs/edebug-instrument-defun-off))
      ;; since we evilify `edebug-mode-map' we don't need to intercept it to
      ;; make it work with evil
      (evil-set-custom-state-maps
       'evil-intercept-maps
       'evil-pending-intercept-maps
       'intercept-state
       'evil-make-intercept-map
       (delq (assq 'edebug-mode-map evil-intercept-maps)
             evil-intercept-maps))
      (evilified-state-evilify-map edebug-mode-map
        :eval-after-load edebug
        :bindings
        "a" 'edebug-stop
        "c" 'edebug-go-mode
        "s" 'edebug-step-mode
        "S" 'edebug-next-mode)
      (evilified-state-evilify-map edebug-eval-mode-map
        :eval-after-load edebug
        :bindings
        "a" 'edebug-stop
        "c" 'edebug-go-mode
        "s" 'edebug-step-mode
        "S" 'edebug-next-mode)
      (advice-add 'edebug-mode :after 'space-macs//edebug-mode))))

(defun e-macs-lisp/post-init-eldoc ()
  (add-hook 'e-macs-lisp-mode-hook 'eldoc-mode))

(defun e-macs-lisp/init-auto-compile ()
  (use-package auto-compile
    :defer (space-macs/defer)
    :init
    (progn
      (space-macs|require-when-dumping 'auto-compile)
      (setq auto-compile-display-buffer nil
            ;; lets spaceline manage the mode-line
            auto-compile-use-mode-line nil
            auto-compile-mode-line-counter t)
      (add-hook 'e-macs-lisp-mode-hook 'auto-compile-mode))
    :config
    (progn
      (space-macs|hide-lighter auto-compile-mode)
      (space-macs/set-leader-keys-for-major-mode 'e-macs-lisp-mode
        "cl" 'auto-compile-display-log))))

(defun e-macs-lisp/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer (space-macs/defer)
    :init
    (progn
      (space-macs|require-when-dumping 'elisp-slime-nav)
      (add-hook 'e-macs-lisp-mode-hook 'elisp-slime-nav-mode)
      (dolist (mode '(e-macs-lisp-mode lisp-interaction-mode))
        (space-macs/declare-prefix-for-mode mode "mg" "find-symbol")
        (space-macs/set-leader-keys-for-major-mode mode
          "gb" 'xref-pop-marker-stack)
        (space-macs/declare-prefix-for-mode mode "mh" "help")

        ;; Load better help mode if helpful is installed
        (if (configuration-layer/layer-used-p 'helpful)
            (space-macs/set-leader-keys-for-major-mode mode
              "hh" 'helpful-at-point)
          (space-macs/set-leader-keys-for-major-mode mode
            "hh" 'elisp-slime-nav-describe-elisp-thing-at-point))
        (let ((jumpl (intern (format "space-macs-jump-handlers-%S" mode))))
          (add-to-list jumpl 'elisp-slime-nav-find-elisp-thing-at-point))))
    :config (space-macs|hide-lighter elisp-slime-nav-mode)))

(defun e-macs-lisp/init-e-macs-lisp ()
  (dolist (mode '(e-macs-lisp-mode lisp-interaction-mode))
    (space-macs/declare-prefix-for-mode mode "mc" "compile")
    (space-macs/declare-prefix-for-mode mode "me" "eval")
    (space-macs/declare-prefix-for-mode mode "mt" "tests")
    (space-macs/set-leader-keys-for-major-mode mode
      "cc" 'e-macs-lisp-byte-compile
      "e$" 'lisp-state-eval-sexp-end-of-line
      "eb" 'eval-buffer
      "eC" 'space-macs/eval-current-form
      "ee" 'eval-last-sexp
      "er" 'eval-region
      "ef" 'eval-defun
      "el" 'lisp-state-eval-sexp-end-of-line
      "gG" 'space-macs/nav-find-elisp-thing-at-point-other-window
      ","  'lisp-state-toggle-lisp-state
      "tb" 'space-macs/ert-run-tests-buffer
      "tq" 'ert)))

(defun e-macs-lisp/init-macrostep ()
  (use-package macrostep
    :defer t
    :mode (("\\*.el\\'" . e-macs-lisp-mode)
           ("Cask\\'" . e-macs-lisp-mode))
    :init
    (progn
      (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all)
      (space-macs|define-transient-state macrostep
        :title "MacroStep Transient State"
        :doc "\n[_e_] expand [_c_] collapse [_n_/_N_] next/previous [_q_] quit"
        :foreign-keys run
        :bindings
        ("e" macrostep-expand)
        ("c" macrostep-collapse)
        ("n" macrostep-next-macro)
        ("N" macrostep-prev-macro)
        ("q" macrostep-collapse-all :exit t))
      (space-macs/set-leader-keys-for-major-mode 'e-macs-lisp-mode
        "dm" 'space-macs/macrostep-transient-state/body))))

(defun e-macs-lisp/init-nameless ()
    (use-package nameless
      :defer (space-macs/defer)
      :init
      (progn
        (space-macs|require-when-dumping 'nameless)
        (setq
         ;; always show the separator since it can have a semantic purpose
         ;; like in Space-macs where - is variable and / is a function.
         ;; moreover it makes nameless work for all kind of separators.
         nameless-separator nil
         ;; Use > as the defautl prefix : is already used for
         ;; keywords
         nameless-prefix ">")
        ;; some default aliases for Space-macs source code
        (setq nameless-global-aliases '(("SB" . "space-macs-buffer")
                                        ("S"  . "space-macs")
                                        (".S"  . "dotspace-macs")
                                        ("CL" . "configuration-layer")))
        ;; make `nameless-current-name' safe as a local variable for string
        ;; values
        (put 'nameless-current-name 'safe-local-variable #'stringp)
        (space-macs|diminish nameless-mode " ðŸ…½" " [n]")
        (space-macs|add-toggle nameless
          :status nameless-mode
          :on (nameless-mode)
          :off (nameless-mode -1)
          :documentation "Hide package namespaces in your e-macs-lisp code."
          :evil-leader-for-mode (e-macs-lisp-mode . "Tn"))
        ;; activate nameless only when in a GUI
        ;; in a terminal nameless triggers all sorts of graphical glitches.
        (space-macs|unless-dumping-and-eval-after-loaded-dump nameless
          (space-macs|do-after-display-system-init
           (when e-macs-lisp-hide-namespace-prefix
             (space-macs/toggle-nameless-on-register-hook-e-macs-lisp-mode)))))))

(defun e-macs-lisp/init-overseer ()
  (use-package overseer
    :defer t
    :init (space-macs/set-leader-keys-for-major-mode 'e-macs-lisp-mode
            "ta" 'overseer-test
            "tt" 'overseer-test-run-test
            "tb" 'overseer-test-this-buffer
            "tf" 'overseer-test-file
            "tg" 'overseer-test-tags
            "tp" 'overseer-test-prompt
            "tA" 'overseer-test-debug
            "tq" 'overseer-test-quiet
            "tv" 'overseer-test-verbose
            "th" 'overseer-help)))

(defun e-macs-lisp/post-init-evil ()
  (add-hook 'e-macs-lisp-mode-hook #'space-macs//define-elisp-comment-text-object))

(defun e-macs-lisp/pre-init-evil-cleverparens ()
  (space-macs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'e-macs-lisp-mode)))

(defun e-macs-lisp/post-init-eval-sexp-fu ()
  (add-hook 'e-macs-lisp-mode-hook 'eval-sexp-fu-flash-mode))

(defun e-macs-lisp/post-init-flycheck ()
  ;; Don't activate flycheck by default in elisp
  ;; because of too much false warnings
  ;; (space-macs/enable-flycheck 'e-macs-lisp-mode)

  ;; Make flycheck recognize packages in loadpath
  ;; i.e (require 'company) will not give an error now
  (setq flycheck-e-macs-lisp-load-path 'inherit))

(defun e-macs-lisp/init-flycheck-package ()
  (use-package flycheck-package
    :hook (e-macs-lisp-mode . flycheck-package-setup)))

(defun e-macs-lisp/init-flycheck-elsa ()
  (use-package flycheck-elsa
    :hook (e-macs-lisp-mode . flycheck-elsa-setup)))

(defun e-macs-lisp/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'e-macs-lisp-mode))

(defun e-macs-lisp/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'e-macs-lisp-mode))

(defun e-macs-lisp/post-init-ggtags ()
  (add-hook 'e-macs-lisp-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun e-macs-lisp/post-init-parinfer ()
  (add-hook 'e-macs-lisp-mode-hook 'parinfer-mode))

(defun e-macs-lisp/post-init-rainbow-identifiers ()
  (add-hook 'e-macs-lisp-mode-hook #'colors//rainbow-identifiers-ignore-keywords))

(defun e-macs-lisp/post-init-semantic ()
  (add-hook 'e-macs-lisp-mode-hook 'semantic-mode)
  (with-eval-after-load 'semantic
    (semantic-default-elisp-setup)))

(defun e-macs-lisp/post-init-srefactor ()
  (add-hook 'e-macs-lisp-mode-hook 'space-macs/load-srefactor)
  (use-package srefactor-lisp
    :commands (srefactor-lisp-format-buffer
               srefactor-lisp-format-defun
               srefactor-lisp-format-sexp
               srefactor-lisp-one-line)
    :init
    (dolist (mode '(e-macs-lisp-mode lisp-interaction-mode))
      (space-macs/declare-prefix-for-mode mode "m=" "srefactor")
      (space-macs/set-leader-keys-for-major-mode mode
        "=b" 'srefactor-lisp-format-buffer
        "=d" 'srefactor-lisp-format-defun
        "=o" 'srefactor-lisp-one-line
        "=s" 'srefactor-lisp-format-sexp))))

(defun e-macs-lisp/post-init-smartparens ()
  (dolist (mode '(e-macs-lisp-mode lisp-interaction-mode))
    (space-macs/set-leader-keys-for-major-mode mode
      "ec" 'space-macs/eval-current-form-sp
      "es" 'space-macs/eval-current-symbol-sp)))

(defun e-macs-lisp/init-emr ()
  (use-package emr
    :config
    (let ((key-binding-prefixes
           '(("mr" . "refactor")
             ("mrd" . "delete")
             ("mre" . "extract/expand")
             ("mrf" . "find/function")
             ("mri" . "insert/inline"))))
      (mapc (lambda (x) (space-macs/declare-prefix-for-mode
                          'e-macs-lisp-mode (car x) (cdr x)))
            key-binding-prefixes))
    (space-macs/set-leader-keys-for-major-mode 'e-macs-lisp-mode
      "rfe" #'emr-el-implement-function
      "rfd" #'emr-el-find-unused-definitions

      "ref" #'emr-el-extract-function
      "rev" #'emr-el-extract-variable
      "rel" #'emr-el-extract-to-let
      "rec" #'emr-el-extract-constant
      "rea" #'emr-el-extract-autoload

      "riv" #'emr-el-inline-variable
      "ris" #'emr-el-inline-let-variable
      "rif" #'emr-el-inline-function
      "ria" #'emr-el-insert-autoload-directive

      "rdl" #'emr-el-delete-let-binding-form
      "rdd" #'emr-el-delete-unused-definition

      "ew"  #'emr-el-eval-and-replace)))


