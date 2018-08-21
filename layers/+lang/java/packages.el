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
        ensime
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
        (lsp-java :requires lsp-mode lsp-ui company-lsp)
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

(defun java/init-ensime ()
  (use-package ensime
    :defer t
    :commands ensime-mode
    :init
    (progn
      (setq ensime-startup-dirname (concat spacemacs-cache-directory "ensime/"))
      (spacemacs/register-repl 'ensime 'ensime-inf-switch "ensime"))
    :config
    (progn
      ;; This function was renamed in ensime. Usually we don't need to do this,
      ;; but documentation recommends the stable version of ensime, so we must
      ;; try to support it, too.
      (unless (fboundp 'ensime-type-at-point)
        (defalias 'ensime-type-at-point 'ensime-print-type-at-point))

      ;; key bindings
      (dolist (mode java--ensime-modes)
        (dolist (prefix '(("mb" . "build")
                          ("mc" . "check")
                          ("md" . "debug")
                          ("mD" . "daemon")
                          ("mE" . "errors")
                          ("mg" . "goto")
                          ("mh" . "docs")
                          ("mi" . "inspect")
                          ("mr" . "refactor")
                          ("mt" . "test")
                          ("ms" . "repl")
                          ("my" . "yank")))
          (spacemacs/declare-prefix-for-mode mode (car prefix) (cdr prefix)))
        (spacemacs/set-leader-keys-for-major-mode mode
          "/"      'ensime-search
          "'"      'ensime-inf-switch

          "bc"     'ensime-sbt-do-compile
          "bC"     'ensime-sbt-do-clean
          "bi"     'ensime-sbt-switch
          "bp"     'ensime-sbt-do-package
          "br"     'ensime-sbt-do-run

          "ct"     'ensime-typecheck-current-buffer

          "dA"     'ensime-db-attach
          "db"     'ensime-db-set-break
          "dB"     'ensime-db-clear-break
          "dC"     'ensime-db-clear-all-breaks
          "dc"     'ensime-db-continue
          "di"     'ensime-db-inspect-value-at-point
          "dn"     'ensime-db-next
          "do"     'ensime-db-step-out
          "dq"     'ensime-db-quit
          "dr"     'ensime-db-run
          "ds"     'ensime-db-step
          "dt"     'ensime-db-backtrace

          "Df"     'ensime-reload-open-files
          "Dr"     'spacemacs/ensime-gen-and-restart
          "Ds"     'ensime

          "Ee"     'ensime-print-errors-at-point
          "Es"     'ensime-stacktrace-switch

          "gp"     'ensime-pop-find-definition-stack

          "hh"     'ensime-show-doc-for-symbol-at-point
          "hT"     'ensime-type-at-point-full-name
          "ht"     'ensime-type-at-point
          "hu"     'ensime-show-uses-of-symbol-at-point

          "ra"     'ensime-refactor-add-type-annotation
          "rd"     'ensime-refactor-diff-inline-local
          "rD"     'ensime-undo-peek
          "ri"     'ensime-refactor-diff-organize-imports
          "rm"     'ensime-refactor-diff-extract-method
          "rr"     'ensime-refactor-diff-rename
          "rt"     'ensime-import-type-at-point
          "rv"     'ensime-refactor-diff-extract-local

          "ta"     'ensime-sbt-do-test-dwim
          "tr"     'ensime-sbt-do-test-quick-dwim
          "tt"     'ensime-sbt-do-test-only-dwim

          "sa"     'ensime-inf-load-file
          "sb"     'ensime-inf-eval-buffer
          "sB"     'spacemacs/ensime-inf-eval-buffer-switch
          "si"     'ensime-inf-switch
          "sr"     'ensime-inf-eval-region
          "sR"     'spacemacs/ensime-inf-eval-region-switch

          "yT"     'spacemacs/ensime-yank-type-at-point-full-name
          "yt"     'spacemacs/ensime-yank-type-at-point

          "z"      'ensime-expand-selection-command))
      (evil-define-key 'insert ensime-mode-map
        (kbd ".") 'spacemacs/ensime-completing-dot
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)
      (evil-define-key 'normal ensime-mode-map
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)
      (evil-define-key 'normal ensime-popup-buffer-map
        (kbd "q") 'ensime-popup-buffer-quit-function)
      (evil-define-key 'normal ensime-inspector-mode-map
        (kbd "q") 'ensime-popup-buffer-quit-function)
      (evil-define-key 'normal ensime-refactor-info-map
        (kbd "q") 'spacemacs/ensime-refactor-cancel
        (kbd "c") 'spacemacs/ensime-refactor-accept
        (kbd "RET") 'spacemacs/ensime-refactor-accept)
      (evil-define-key 'normal ensime-compile-result-map
        (kbd "g") 'ensime-show-all-errors-and-warnings
        (kbd "TAB") 'forward-button
        (kbd "<backtab>") 'backward-button
        (kbd "M-n") 'forward-button
        (kbd "M-p") 'backward-button
        (kbd "n") 'forward-button
        (kbd "N") 'backward-button)
      (evil-define-key '(insert normal) ensime-search-mode-map
        (kbd "C-q") 'ensime-search-quit
        (kbd "C-j") 'ensime-search-next-match
        (kbd "C-k") 'ensime-search-prev-match
        (kbd "RET") 'ensime-search-choose-current-result
        (kbd "C-i") 'ensime-search-insert-import-of-current-result))))

;; (defun java/post-init-ensime ()
;;   (when (eq 'ensime java-backend)
;;     (use-package ensime
;;       :defer t
;;       :init
;;       (progn
;;         (spacemacs//ensime-init 'java-mode t nil)
;;         (when (configuration-layer/package-used-p 'company)
;;           (add-to-list 'company-backends-java-mode 'ensime-company)))
;;       :config
;;       (progn
;;         (spacemacs/ensime-configure-keybindings 'java-mode)))))

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
        (spacemacs/declare-prefix-for-mode 'groovy-mode "mc" "compile")
        (spacemacs/declare-prefix-for-mode 'groovy-mode "mlt" "tests"))
      (when (configuration-layer/package-used-p 'java-mode)
        (add-hook 'java-mode-hook 'gradle-mode)
        (spacemacs/declare-prefix-for-mode 'java-mode "ml" "gradle")
        (spacemacs/declare-prefix-for-mode 'groovy-mode "mc" "compile")
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
    :config
    (progn
      ;; key bindings
      (dolist (prefix '(("mc" . "compile")
                        ("mg" . "goto")
                        ("mr" . "refactor")
                        ("mq" . "lsp")))
      (spacemacs/set-leader-keys-for-major-mode 'java-mode
        "gg"  'xref-find-definitions
        "gr"  'xref-find-references
        "gR"  'lsp-ui-peek-find-references
        "ga"  'xref-find-apropos
        "gA"  'lsp-ui-peek-find-workspace-symbol
        "gd"  'lsp-goto-type-definition
        "hh"  'lsp-describe-thing-at-point
        "el"  'lsp-ui-flycheck-list
        "pu"  'lsp-java-update-user-settings
        "ea"  'lsp-execute-code-action
        "qr"  'lsp-restart-workspace
        "roi" 'lsp-java-organize-imports
        "rr" 'lsp-rename
        "rai" 'lsp-java-add-import
        "ram" 'lsp-java-add-unimplemented-methods
        "rcp" 'lsp-java-create-parameter
        "rcf" 'lsp-java-create-field
        "rec" 'lsp-java-extract-to-constant
        "rel" 'lsp-java-extract-to-local-variable
        "rem" 'lsp-java-extract-method
        "cc"  'lsp-java-build-project
        "an"  'lsp-java-actionable-notifications
        "="   'lsp-format-buffer)

      (setq lsp-highlight-symbol-at-point nil
            lsp-ui-sideline-update-mode 'point
            lsp-eldoc-render-all nil
            lsp-java-completion-guess-arguments t)))))

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
