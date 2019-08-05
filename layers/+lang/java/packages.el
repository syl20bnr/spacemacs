;;; packages.el --- Java Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq java-packages
      '(
        company
        (company-emacs-eclim :toggle
                             (configuration-layer/package-used-p 'company))
        eclim
        eldoc
        flycheck
        (flycheck-eclim :location local
                        :requires flycheck)
        flyspell
        ggtags
        gradle-mode
        counsel-gtags
        helm-gtags
        (java-mode :location built-in)
        maven-test-mode
        (meghanada :toggle (not (version< emacs-version "25.1")))
        mvn
        (lsp-java :requires lsp-mode lsp-ui company-lsp dap-mode)
        org
        ))

(defun java/post-init-company ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-company))

(defun java/init-company-emacs-eclim ()
  (use-package company-emacs-eclim
    :defer t
    :init
    (setq company-emacs-eclim-ignore-case nil)
    ;; see `spacemacs//java-setup-eclim-company'
    ))

(defun java/init-eclim ()
  (use-package eclim
    :defer t
    :if (eq java-backend 'eclim)
    ;; :init (setq eclim-auto-save nil)
    :config
    (progn
      (spacemacs|hide-lighter eclim-mode)
      (require 'eclimd)
      ;; enable help at point
      (setq help-at-pt-display-when-idle t
            help-at-pt-timer-delay 0.1)
      (help-at-pt-set-timer)
      (add-to-list 'minor-mode-alist
                   '(eclim-mode (:eval (eclim-modeline-string))))
      ;; hack to support Maven multi-modules
      (defun my-eclim-fix-relative-path (path)
        (replace-regexp-in-string "^.*src/" "src/" path))
      (advice-add 'eclim--project-current-file :filter-return
                  #'my-eclim-fix-relative-path)
      ;; key bindings
      (dolist (prefix '(("ma" . "ant")
                        ("mD" . "daemon")
                        ("mg" . "goto")
                        ("mh" . "help/doc")
                        ("mi" . "issues")
                        ("mp" . "project")
                        ("mr" . "refactor")
                        ("mt" . "test")))
        (spacemacs/declare-prefix-for-mode
          'java-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        ;; ant
        "aa" 'eclim-ant-run
        "ac" 'eclim-ant-clear-cache
        "ar" 'eclim-ant-run
        "av" 'eclim-ant-validate
        ;; daemon
        "Dk" 'stop-eclimd
        "Ds" 'start-eclimd
        ;; errors (problems)
        "Ee" 'eclim-problems-correct
        ;; find
        "ff" 'eclim-java-find-generic
        ;; goto
        "gt" 'eclim-java-find-type
        ;; help/doc
        "hc" 'eclim-java-call-hierarchy
        "hh" 'eclim-java-show-documentation-for-current-element
        "hi" 'eclim-java-hierarchy
        "hu" 'eclim-java-find-references
        ;; project
        "pb" 'eclim-project-build
        "pc" 'eclim-project-create
        "pd" 'eclim-project-delete
        "pg" 'eclim-project-goto
        "pi" 'eclim-project-import
        "pj" 'eclim-project-info-mode
        "pk" 'eclim-project-close
        "po" 'eclim-project-open
        "pp" 'eclim-project-mode
        "pr" 'eclim-java-run-run
        "pu" 'eclim-project-update
        ;; refactor
        "rc" 'eclim-java-constructor
        "rf" 'eclim-java-format
        "rg" 'eclim-java-generate-getter-and-setter
        "ri" 'eclim-java-import-organize
        "rj" 'eclim-java-implement
        "rn" 'eclim-java-new
        "rr" 'eclim-java-refactor-rename-symbol-at-point
        ;; test
        "tt" 'eclim-run-junit)
      (evil-define-key 'insert java-mode-map
        (kbd ".") 'spacemacs/java-eclim-completing-dot
        (kbd ":") 'spacemacs/java-eclim-completing-double-colon
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)
      (evil-define-key 'normal java-mode-map
        (kbd "M-.") 'eclim-java-find-declaration
        (kbd "M-,") 'pop-tag-mark
        (kbd "M-<mouse-3>") 'eclim-java-find-declaration
        (kbd "<mouse-8>") 'pop-tag-mark)
      (evil-define-key 'normal eclim-problems-mode-map
        (kbd "a") 'eclim-problems-show-all
        (kbd "e") 'eclim-problems-show-errors
        (kbd "g") 'eclim-problems-buffer-refresh
        (kbd "q") 'eclim-quit-window
        (kbd "w") 'eclim-problems-show-warnings
        (kbd "f") 'eclim-problems-toggle-filefilter
        (kbd "c") 'eclim-problems-correct
        (kbd "RET") 'eclim-problems-open-current)
      (evil-define-key 'normal eclim-project-mode-map
        (kbd "N") 'eclim-project-create
        (kbd "m") 'eclim-project-mark-current
        (kbd "M") 'eclim-project-mark-all
        (kbd "u") 'eclim-project-unmark-current
        (kbd "U") 'eclim-project-unmark-all
        (kbd "o") 'eclim-project-open
        (kbd "c") 'eclim-project-close
        (kbd "i") 'eclim-project-info-mode
        (kbd "I") 'eclim-project-import
        (kbd "RET") 'eclim-project-goto
        (kbd "D") 'eclim-project-delete
        (kbd "p") 'eclim-project-update
        (kbd "g") 'eclim-project-mode-refresh
        (kbd "R") 'eclim-project-rename
        (kbd "q") 'eclim-quit-window))))

(defun java/post-init-eldoc ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-eldoc))

(defun java/post-init-flycheck ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-flycheck))

(defun java/init-flycheck-eclim ()
  (use-package flycheck-eclim
    :commands flycheck-eclim-setup
    ;; see `spacemacs//java-setup-eclim-flycheck'
    ))

(defun java/post-init-flyspell ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-flyspell))

(defun java/post-init-ggtags ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun java/init-gradle-mode ()
  (use-package gradle-mode
    :defer t
    :init
    (progn
      (when (configuration-layer/package-used-p 'groovy-mode)
        (add-hook 'groovy-mode-hook 'gradle-mode)
        (spacemacs/declare-prefix-for-mode 'groovy-mode "ml" "gradle")
        (spacemacs/declare-prefix-for-mode 'groovy-mode "mlc" "compile")
        (spacemacs/declare-prefix-for-mode 'groovy-mode "mlt" "tests"))
      (when (configuration-layer/package-used-p 'java-mode)
        (add-hook 'java-mode-hook 'gradle-mode)
        (spacemacs/declare-prefix-for-mode 'java-mode "ml" "gradle")
        (spacemacs/declare-prefix-for-mode 'java-mode "mlc" "compile")
        (spacemacs/declare-prefix-for-mode 'java-mode "mlt" "tests")))
    :config
    (progn
      (spacemacs|hide-lighter gradle-mode)
      (spacemacs/set-leader-keys-for-minor-mode 'gradle-mode
        "lcc" 'gradle-build
        "lcC" 'spacemacs/gradle-clean
        "lcr" 'spacemacs/gradle-clean-build
        "lta" 'gradle-test
        "ltb" 'spacemacs/gradle-test-buffer
        "ltt" 'gradle-single-test
        "lx" 'gradle-execute))))

(defun java/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'java-mode))

(defun java/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'java-mode))

(defun java/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(java . t))))

(defun java/init-java-mode ()
  (use-package java-mode
    :defer t
    :init
    (progn
      (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-backend)
      (put 'java-backend 'safe-local-variable 'symbolp)
      (spacemacs//java-define-command-prefixes))))

(defun java/init-maven-test-mode ()
  (use-package maven-test-mode
    :defer t
    :init
    (when (configuration-layer/package-used-p 'java-mode)
      (add-hook 'java-mode-hook 'maven-test-mode)
      (spacemacs/declare-prefix-for-mode 'java-mode "mm" "maven")
      (spacemacs/declare-prefix-for-mode 'java-mode "mmg" "goto")
      (spacemacs/declare-prefix-for-mode 'java-mode "mmt" "tests"))
    :config
    (progn
      (spacemacs|hide-lighter maven-test-mode)
      (spacemacs/set-leader-keys-for-minor-mode 'maven-test-mode
        "mga"  'maven-test-toggle-between-test-and-class
        "mgA"  'maven-test-toggle-between-test-and-class-other-window
        "mta"   'maven-test-all
        "mtC-a" 'maven-test-clean-test-all
        "mtb"   'maven-test-file
        "mti"   'maven-test-install
        "mtt"   'maven-test-method))))

(defun java/init-meghanada ()
  (use-package meghanada
    :defer t
    :if (eq java-backend 'meghanada)
    :init
    (progn
      (setq meghanada-server-install-dir (concat spacemacs-cache-directory
                                                 "meghanada/")
            company-meghanada-prefix-length 1
            ;; let spacemacs handle company and flycheck itself
            meghanada-use-company nil
            meghanada-use-flycheck nil))
    :config
    (progn
      ;; key bindings
      (dolist (prefix '(("mc" . "compile")
                        ("mD" . "daemon")
                        ("mg" . "goto")
                        ("mr" . "refactor")
                        ("mt" . "test")
                        ("mx" . "execute")))
        (spacemacs/declare-prefix-for-mode
          'java-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "cb" 'meghanada-compile-file
        "cc" 'meghanada-compile-project

        "Dc" 'meghanada-client-direct-connect
        "Dd" 'meghanada-client-disconnect
        "Di" 'meghanada-install-server
        "Dk" 'meghanada-server-kill
        "Dl" 'meghanada-clear-cache
        "Dp" 'meghanada-ping
        "Dr" 'meghanada-restart
        "Ds" 'meghanada-client-connect
        "Du" 'meghanada-update-server
        "Dv" 'meghanada-version

        "gb" 'meghanada-back-jump

        "=" 'meghanada-code-beautify
        "ri" 'meghanada-optimize-import
        "rI" 'meghanada-import-all

        "ta" 'meghanada--run-junit
        "tc" 'meghanada-run-junit-class
        "tl" 'meghanada-run-junit-recent
        "tt" 'meghanada-run-junit-test-case

        ;; meghanada-switch-testcase
        ;; meghanada-local-variable

        "x:" 'meghanada-run-task))))

(defun java/init-lsp-java ()
  (use-package lsp-java
    :defer t
    :if (eq java-backend 'lsp)
    :config
    (progn
      ;; key bindings
      (dolist (prefix '(("mc" . "compile/create")
                        ("mg" . "goto")
                        ("mr" . "refactor")
                        ("mra" . "add/assign")
                        ("mrc" . "create/convert")
                        ("mrg" . "generate")
                        ("mre" . "extract")
                        ("mq" . "lsp")
                        ("mt" . "test")
                        ("mx" . "execute")))
        (spacemacs/declare-prefix-for-mode
          'java-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "pu"  'lsp-java-update-user-settings

        ;; refactoring
        "ro" 'lsp-java-organize-imports
        "rcp" 'lsp-java-create-parameter
        "rcf" 'lsp-java-create-field
        "rci" 'lsp-java-conver-to-static-import
        "rec" 'lsp-java-extract-to-constant
        "rel" 'lsp-java-extract-to-local-variable
        "rem" 'lsp-java-extract-method

        ;; assign/add
        "rai" 'lsp-java-add-import
        "ram" 'lsp-java-add-unimplemented-methods
        "rat" 'lsp-java-add-throws
        "raa" 'lsp-java-assign-all
        "raf" 'lsp-java-assign-to-field

        ;; generate
        "rgt" 'lsp-java-generate-to-string
        "rge" 'lsp-java-generate-equals-and-hash-code
        "rgo" 'lsp-java-generate-overrides
        "rgg" 'lsp-java-generate-getters-and-setters

        ;; create/compile
        "cc"  'lsp-java-build-project
        "cp"  'lsp-java-spring-initializr

        "an"  'lsp-java-actionable-notifications

        ;; dap-mode

        ;; debug
        "ddj" 'dap-java-debug
        "dtt" 'dap-java-debug-test-method
        "dtc" 'dap-java-debug-test-class
        ;; run
        "tt" 'dap-java-run-test-method
        "tc" 'dap-java-run-test-class)

      (setq lsp-highlight-symbol-at-point nil
            lsp-ui-sideline-update-mode 'point
            lsp-eldoc-render-all nil
            lsp-java-completion-guess-arguments t))))

(defun java/init-mvn ()
  (use-package mvn
    :defer t
    :init
    (when (configuration-layer/package-used-p 'java-mode)
      (spacemacs/declare-prefix-for-mode 'java-mode "mm" "maven")
      (spacemacs/declare-prefix-for-mode 'java-mode "mmc" "compile")
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "mcc" 'mvn-compile
        "mcC" 'mvn-clean
        "mcr" 'spacemacs/mvn-clean-compile))))
