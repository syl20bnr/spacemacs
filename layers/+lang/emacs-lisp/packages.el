;;; packages.el --- Emacs Lisp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defconst emacs-lisp-packages
  '(
    auto-compile
    company
    (debug :location built-in)
    (edebug :location built-in)
    eldoc
    elisp-def
    elisp-slime-nav
    (emacs-lisp :location built-in)
    evil
    evil-cleverparens
    eval-sexp-fu
    flycheck
    flycheck-elsa
    flycheck-package
    ggtags
    counsel-gtags
    (ielm :location built-in)
    (inspector :location (recipe
                          :fetcher github
                          :repo "mmontone/emacs-inspector"))
    macrostep
    nameless
    overseer
    rainbow-identifiers
    semantic
    smartparens
    srefactor
    emr))

(defun emacs-lisp/init-ielm ()
  (use-package ielm
    :defer t
    :init
    (spacemacs/register-repl 'ielm 'ielm)
    ;; Load better help mode if helpful is installed
    (if (configuration-layer/layer-used-p 'helpful)
        (spacemacs/set-leader-keys-for-major-mode 'inferior-emacs-lisp-mode
          "hh" 'helpful-at-point)
      (spacemacs/set-leader-keys-for-major-mode 'inferior-emacs-lisp-mode
        "hh" 'elisp-slime-nav-describe-elisp-thing-at-point))
    (add-to-list 'spacemacs-jump-handlers-inferior-emacs-lisp-mode
                 'elisp-slime-nav-find-elisp-thing-at-point)
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/declare-prefix-for-mode mode "ms" "ielm")
      (spacemacs/set-leader-keys-for-major-mode mode
        "'" 'ielm
        "si" 'ielm))
    :config
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>") (goto-char current-point))
          (lisp-indent-line))))))

(defun emacs-lisp/post-init-company ()
  (spacemacs|add-company-backends :backends (company-capf company-dabbrev-code)
                                  :modes emacs-lisp-mode)
  (spacemacs|add-company-backends :backends (company-files company-capf)
                                  :modes ielm-mode))

(defun emacs-lisp/init-debug ()
  (use-package debug
    :defer t
    :init (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
            (spacemacs/declare-prefix-for-mode mode "md" "debug")
            (spacemacs/set-leader-keys-for-major-mode mode
              "dt" 'spacemacs/elisp-toggle-debug-expr-and-eval-func))
    :config (evilified-state-evilify-map debugger-mode-map
              :mode debugger-mode)))

(defun emacs-lisp/init-edebug ()
  (use-package edebug
    :defer t
    :init
    ;; key bindings
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "df" 'spacemacs/edebug-instrument-defun-on
        "dF" 'spacemacs/edebug-instrument-defun-off))
    (spacemacs/declare-prefix-for-mode 'edebug-eval-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'edebug-eval-mode "me" "eval")
    (spacemacs/set-leader-keys-for-major-mode 'edebug-eval-mode
      "gw" 'edebug-where
      "a" 'edebug-delete-eval-item
      "k" 'edebug-delete-eval-item
      "," 'edebug-update-eval-list
      "c" 'edebug-update-eval-list
      "ee" 'edebug-eval-last-sexp
      "eE" 'edebug-eval-print-last-sexp)
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
    (advice-add 'edebug-mode :after 'spacemacs//edebug-mode)))

(defun emacs-lisp/post-init-eldoc ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun emacs-lisp/init-auto-compile ()
  (use-package auto-compile
    :defer (spacemacs/defer)
    :init
    (spacemacs|require-when-dumping 'auto-compile)
    (setq auto-compile-display-buffer nil
          ;; lets spaceline manage the mode-line
          auto-compile-use-mode-line nil
          auto-compile-mode-line-counter t)
    (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode)
    :config
    (spacemacs|hide-lighter auto-compile-mode)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
      "cl" 'auto-compile-display-log)))

(defun emacs-lisp/init-elisp-def ()
  (use-package elisp-def
    :defer t))

(defun emacs-lisp/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer (spacemacs/defer)
    :init
    (spacemacs|require-when-dumping 'elisp-slime-nav)
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/declare-prefix-for-mode mode "mg" "find-symbol")
      (spacemacs/set-leader-keys-for-major-mode mode
        "gb" 'xref-pop-marker-stack)
      (spacemacs/declare-prefix-for-mode mode "mh" "help")

      ;; Load better help mode if helpful is installed
      (if (configuration-layer/layer-used-p 'helpful)
          (spacemacs/set-leader-keys-for-major-mode mode
            "hh" 'helpful-at-point)
        (spacemacs/set-leader-keys-for-major-mode mode
          "hh" 'elisp-slime-nav-describe-elisp-thing-at-point))
      (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode))))
        (add-to-list jumpl 'elisp-def)
        (add-to-list jumpl 'elisp-slime-nav-find-elisp-thing-at-point)))
    :config (spacemacs|hide-lighter elisp-slime-nav-mode)))

(defun emacs-lisp/init-emacs-lisp ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/declare-prefix-for-mode mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode mode "me" "eval")
    (spacemacs/declare-prefix-for-mode mode "mt" "tests")
    (spacemacs/set-leader-keys-for-major-mode mode
      "cc" 'emacs-lisp-byte-compile
      "e$" 'lisp-state-eval-sexp-end-of-line
      "eb" 'eval-buffer
      "eC" 'spacemacs/eval-current-form
      "ee" 'eval-last-sexp
      "er" 'eval-region
      "ef" 'eval-defun
      "el" 'lisp-state-eval-sexp-end-of-line
      "gG" 'spacemacs/nav-find-elisp-thing-at-point-other-window
      ","  'lisp-state-toggle-lisp-state
      "tb" 'spacemacs/ert-run-tests-buffer
      "tq" 'ert)))

(defun emacs-lisp/init-macrostep ()
  (use-package macrostep
    :defer t
    :mode (("\\*.el\\'" . emacs-lisp-mode)
           ("Cask\\'" . emacs-lisp-mode))
    :init
    (evil-define-key 'normal macrostep-keymap "q" 'macrostep-collapse-all)
    (spacemacs|define-transient-state macrostep
      :title "MacroStep Transient State"
      :doc "\n[_e_] expand [_c_] collapse [_n_/_N_] next/previous [_q_] quit"
      :foreign-keys run
      :bindings
      ("e" macrostep-expand)
      ("c" macrostep-collapse)
      ("n" macrostep-next-macro)
      ("N" macrostep-prev-macro)
      ("q" macrostep-collapse-all :exit t))
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
      "dm" 'spacemacs/macrostep-transient-state/body)))

(defun emacs-lisp/init-nameless ()
  (use-package nameless
    :defer (spacemacs/defer)
    :init
    (spacemacs|require-when-dumping 'nameless)
    (setq
     ;; always show the separator since it can have a semantic purpose
     ;; like in Spacemacs where - is variable and / is a function.
     ;; moreover it makes nameless work for all kind of separators.
     nameless-separator nil
     ;; Use > as the defautl prefix : is already used for
     ;; keywords
     nameless-prefix ">")
    ;; some default aliases for Spacemacs source code
    (setq nameless-global-aliases '(("SB" . "spacemacs-buffer")
                                    ("S"  . "spacemacs")
                                    (".S"  . "dotspacemacs")
                                    ("CL" . "configuration-layer")))
    ;; make `nameless-current-name' safe as a local variable for string
    ;; values
    (put 'nameless-current-name 'safe-local-variable #'stringp)
    (spacemacs|diminish nameless-mode " 🅽" " [n]")
    (spacemacs|add-toggle nameless
      :status nameless-mode
      :on (nameless-mode)
      :off (nameless-mode -1)
      :documentation "Hide package namespaces in your emacs-lisp code."
      :evil-leader-for-mode (emacs-lisp-mode . "Tn"))
    ;; activate nameless only when in a GUI
    ;; in a terminal nameless triggers all sorts of graphical glitches.
    (spacemacs|unless-dumping-and-eval-after-loaded-dump nameless
      (spacemacs|do-after-display-system-init
       (when emacs-lisp-hide-namespace-prefix
         (spacemacs/toggle-nameless-on-register-hook-emacs-lisp-mode))))))

(defun emacs-lisp/init-overseer ()
  (use-package overseer
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
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

(defun emacs-lisp/post-init-evil ()
  (add-hook 'emacs-lisp-mode-hook #'spacemacs//define-elisp-comment-text-object))

(defun emacs-lisp/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init
    (add-to-list 'evil-lisp-safe-structural-editing-modes 'emacs-lisp-mode)))

(defun emacs-lisp/post-init-eval-sexp-fu ()
  (add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode))

(defun emacs-lisp/post-init-flycheck ()
  ;; Don't activate flycheck by default in elisp
  ;; because of too much false warnings
  ;; (spacemacs/enable-flycheck 'emacs-lisp-mode)

  ;; Make flycheck recognize packages in loadpath
  ;; i.e (require 'company) will not give an error now
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun emacs-lisp/init-flycheck-package ()
  (use-package flycheck-package
    :hook (emacs-lisp-mode . flycheck-package-setup)))

(defun emacs-lisp/init-flycheck-elsa ()
  (use-package flycheck-elsa
    :hook (emacs-lisp-mode . flycheck-elsa-setup)))

(defun emacs-lisp/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'emacs-lisp-mode))

(defun emacs-lisp/post-init-ggtags ()
  (add-hook 'emacs-lisp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun emacs-lisp/post-init-rainbow-identifiers ()
  (add-hook 'emacs-lisp-mode-hook #'colors//rainbow-identifiers-ignore-keywords))

(defun emacs-lisp/post-init-semantic ()
  (add-hook 'emacs-lisp-mode-hook 'semantic-mode)
  (with-eval-after-load 'semantic
    (semantic-default-elisp-setup)))

(defun emacs-lisp/post-init-srefactor ()
  (add-hook 'emacs-lisp-mode-hook 'spacemacs/load-srefactor)
  (use-package srefactor-lisp
    :commands (srefactor-lisp-format-buffer
               srefactor-lisp-format-defun
               srefactor-lisp-format-sexp
               srefactor-lisp-one-line)
    :init
    (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (spacemacs/declare-prefix-for-mode mode "m=" "srefactor")
      (spacemacs/set-leader-keys-for-major-mode mode
        "=b" 'srefactor-lisp-format-buffer
        "=d" 'srefactor-lisp-format-defun
        "=o" 'srefactor-lisp-one-line
        "=s" 'srefactor-lisp-format-sexp))))

(defun emacs-lisp/post-init-smartparens ()
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "ec" 'spacemacs/eval-current-form-sp
      "e;" 'spacemacs/eval-current-form-to-comment-sp
      "es" 'spacemacs/eval-current-symbol-sp)))

(defun emacs-lisp/init-emr ()
  (use-package emr
    :defer t
    :init
    (let ((key-binding-prefixes
           '(("mr" . "refactor")
             ("mrd" . "delete")
             ("mre" . "extract/expand")
             ("mrf" . "find/function")
             ("mri" . "insert/inline"))))
      (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                          'emacs-lisp-mode (car x) (cdr x)))
            key-binding-prefixes))
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
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

(defun emacs-lisp/init-inspector ()
  (use-package inspector
    :defer t
    :config
    (evilified-state-evilify-map inspector-mode-map
      :mode inspector-mode)))
