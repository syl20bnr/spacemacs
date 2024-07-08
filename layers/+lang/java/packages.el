;;; packages.el --- Java Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
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


(defconst java-packages
  '(
    company
    dap-mode
    flycheck
    ggtags
    counsel-gtags
    (java-mode :location built-in)
    maven-test-mode
    (meghanada :toggle (eq java-backend 'meghanada))
    mvn
    (lsp-java :requires lsp-mode)
    org
    smartparens))

(defun java/post-init-company ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-company))

(defun java/pre-init-dap-mode ()
  (when (eq java-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'java-mode))
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-dap))

(defun java/post-init-flycheck ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-flycheck))

(defun java/post-init-ggtags ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun java/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (sp-local-pair 'java-mode "/** " " */" :trigger "/**")))

(defun java/post-init-counsel-gtags nil)

(defun java/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(java . t))))

(defun java/init-java-mode ()
  (use-package java-mode
    :defer t
    :init
    (add-hook 'java-mode-local-vars-hook #'spacemacs//java-setup-backend)
    (put 'java-backend 'safe-local-variable 'symbolp)))

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
    (spacemacs|hide-lighter maven-test-mode)
    (spacemacs/set-leader-keys-for-minor-mode 'maven-test-mode
      "mga"    'maven-test-toggle-between-test-and-class
      "mgA"    'maven-test-toggle-between-test-and-class-other-window
      "mta"    'maven-test-all
      "mt C-a" 'maven-test-clean-test-all
      "mtb"    'maven-test-file
      "mti"    'maven-test-install
      "mtt"    'maven-test-method)))

(defun java/init-meghanada ()
  (use-package meghanada
    :defer t
    :init
    (setq meghanada-server-install-dir (concat spacemacs-cache-directory
                                               "meghanada/")
          company-meghanada-prefix-length 1
          ;; let spacemacs handle company and flycheck itself
          meghanada-use-company nil
          meghanada-use-flycheck nil)
    :config
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

      "x:" 'meghanada-run-task)))

(defun java/init-lsp-java ()
  (use-package lsp-java
    :defer t
    :if (eq java-backend 'lsp)
    :config
    ;; key bindings
    (dolist (prefix '(("mc" . "compile/create")
                      ("mgk" . "type hierarchy")
                      ("mra" . "add/assign")
                      ("mrc" . "create/convert")
                      ("mrg" . "generate")
                      ("mre" . "extract")
                      ("mt" . "test")))
      (spacemacs/declare-prefix-for-mode
        'java-mode (car prefix) (cdr prefix)))
    (spacemacs/set-leader-keys-for-major-mode 'java-mode
      "wu"  'lsp-java-update-project-configuration

      ;; refactoring
      "ro" 'lsp-java-organize-imports
      "rcp" 'lsp-java-create-parameter
      "rcf" 'lsp-java-create-field
      "rci" 'lsp-java-convert-to-static-import
      "rec" 'lsp-java-extract-to-constant
      "rel" 'lsp-java-extract-to-local-variable
      "rem" 'lsp-java-extract-method

      ;; assign/add
      "rai" 'lsp-java-add-import
      "ram" 'lsp-java-add-unimplemented-methods
      "rat" 'lsp-java-add-throws
      "raa" 'lsp-java-assign-all
      "raf" 'lsp-java-assign-to-field
      "raF" 'lsp-java-assign-statement-to-field
      "ral" 'lsp-java-assign-statement-to-local

      ;; generate
      "rgt" 'lsp-java-generate-to-string
      "rge" 'lsp-java-generate-equals-and-hash-code
      "rgo" 'lsp-java-generate-overrides
      "rgg" 'lsp-java-generate-getters-and-setters

      ;; create/compile
      "cc"  'lsp-java-build-project
      "cp"  'lsp-java-spring-initializr

      "gkk" 'lsp-java-type-hierarchy
      "gku" 'spacemacs/lsp-java-super-type
      "gks" 'spacemacs/lsp-java-sub-type

      ;; test
      "tb" 'lsp-jt-browser)))

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
