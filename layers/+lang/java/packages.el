;;; packages.el --- Java Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
                             (configuration-layer/package-usedp 'company))
        eclim
        eldoc
        ensime
        flycheck
        (flycheck-eclim :location local
                        :toggle (configuration-layer/package-usedp 'flycheck))
        flyspell
        ggtags
        gradle-mode
        groovy-imports
        groovy-mode
        helm-gtags
        (java-mode :location built-in)
        (meghanada :toggle (not (version< emacs-version "25.1")))
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
                        ("mm" . "maven")
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
        "ee" 'eclim-problems-correct
        ;; find
        "ff" 'eclim-java-find-generic
        ;; goto
        "gt" 'eclim-java-find-type
        ;; help/doc
        "hc" 'eclim-java-call-hierarchy
        "hh" 'eclim-java-show-documentation-for-current-element
        "hi" 'eclim-java-hierarchy
        "hu" 'eclim-java-find-references
        ;; maven
        "mi" 'spacemacs/java-maven-clean-install
        "mI" 'spacemacs/java-maven-install
        "mp" 'eclim-maven-lifecycle-phases
        "mr" 'eclim-maven-run
        "mR" 'eclim-maven-lifecycle-phase-run
        "mt" 'spacemacs/java-maven-test
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
        "pu" 'eclim-project-update
        ;; refactor
        "rc" 'eclim-java-constructor
        "rg" 'eclim-java-generate-getter-and-setter
        "rf" 'eclim-java-format
        "ri" 'eclim-java-import-organize
        "rj" 'eclim-java-implement
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
        (kbd "q") 'eclim-quit-window)
      )))

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
      ;; key bindings
      (dolist (mode java--ensime-modes)
        (dolist (prefix '(("mb" . "build")
                          ("mc" . "check")
                          ("md" . "debug")
                          ("mD" . "daemon")
                          ("me" . "errors")
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
          "cT"     'ensime-typecheck-all

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

          "ee"     'ensime-print-errors-at-point
          "el"     'ensime-show-all-errors-and-warnings
          "es"     'ensime-stacktrace-switch

          "gp"     'ensime-pop-find-definition-stack
          "gi"     'ensime-goto-impl
          "gt"     'ensime-goto-test

          "hh"     'ensime-show-doc-for-symbol-at-point
          "hT"     'ensime-type-at-point-full-name
          "ht"     'ensime-type-at-point
          "hu"     'ensime-show-uses-of-symbol-at-point

          "ii"     'ensime-inspect-type-at-point
          "iI"     'ensime-inspect-type-at-point-other-frame
          "ip"     'ensime-inspect-project-package

          "ra"     'ensime-refactor-add-type-annotation
          "rd"     'ensime-refactor-diff-inline-local
          "rD"     'ensime-undo-peek
          "rf"     'ensime-format-source
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
        (kbd "N") 'backward-button))))

;; (defun java/post-init-ensime ()
;;   (when (eq 'ensime java-backend)
;;     (use-package ensime
;;       :defer t
;;       :init
;;       (progn
;;         (spacemacs//ensime-init 'java-mode t nil)
;;         (when (configuration-layer/package-usedp 'company)
;;           (push 'ensime-company company-backends-java-mode)))
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
    :defer t))

(defun java/init-groovy-imports ()
  (use-package groovy-imports
    :defer t))

(defun java/init-groovy-mode ()
  (use-package groovy-mode
    :defer t
    ))

(defun java/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'java-mode))

(defun java/init-java-mode ()
  (use-package java-mode
    :defer t
    :init
    (progn
      (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-backend)
      (put 'java-backend 'safe-local-variable 'symbolp)
      (spacemacs//java-define-command-prefixes))))

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
