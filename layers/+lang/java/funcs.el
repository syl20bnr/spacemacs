;;; packages.el --- Java functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//java-define-command-prefixes ()
  "Define command prefixes for java-mode."
  (setq java/key-binding-prefixes '(("md" . "eclimd")
                                    ("mE" . "errors")
                                    ("mf" . "find")
                                    ("mg" . "goto")
                                    ("mr" . "refactor")
                                    ("mh" . "documentation")
                                    ("ma" . "ant")
                                    ("mp" . "project")
                                    ("mt" . "test")))
  (mapc (lambda(x) (spacemacs/declare-prefix-for-mode
                    'java-mode (car x) (cdr x)))
        java/key-binding-prefixes))

(defun spacemacs//java-setup-backend ()
  "Conditionally setup java backend."
  (pcase java-backend
    (`meghanada (spacemacs//java-setup-meghanada))
    (`eclim (spacemacs//java-setup-eclim))
    (`lsp (spacemacs//java-setup-lsp))))

(defun spacemacs//java-setup-company ()
  "Conditionally setup company based on backend."
  (pcase java-backend
    (`meghanada (spacemacs//java-setup-meghanada-company))
    (`eclim (spacemacs//java-setup-eclim-company))
    (`lsp (spacemacs//java-setup-lsp-company))))

(defun spacemacs//java-setup-flycheck ()
  "Conditionally setup flycheck based on backend."
  (pcase java-backend
    (`meghanada (spacemacs//java-setup-meghanada-flycheck))
    (`eclim (spacemacs//java-setup-eclim-flycheck))
    (`lsp (spacemacs//java-setup-lsp-flycheck))))



;; eclim

(defun spacemacs//java-setup-eclim ()
  "Setup Eclim."
  ;; jump handler
  (add-to-list 'spacemacs-jump-handlers '(eclim-java-find-declaration :async t))
  ;; enable eclim
  (eclim-mode))

(defun spacemacs//java-setup-eclim-company ()
  "Setup Eclim auto-completion."
  (spacemacs|add-company-backends
    :backends company-emacs-eclim
    :modes eclim-mode
    :variables
    company-idle-delay 0.5
    company-minimum-prefix-length 1
    :append-hooks nil
    :call-hooks t)
  (company-mode))

(defun spacemacs//java-setup-eclim-flycheck ()
  "Setup Eclim syntax checking."
  (flycheck-eclim-setup)
  ;; disable auto check, use `SPC e e'
  (setq eclim-autoupdate-problems nil)
  (set (make-local-variable 'flycheck-check-syntax-automatically) nil)
  (flycheck-mode))

(defun spacemacs/java-eclim-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ".")
  (company-emacs-eclim 'interactive))

(defun spacemacs/java-eclim-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-emacs-eclim 'interactive))))


;; meghanada

(defun spacemacs//java-setup-meghanada ()
  "Setup Meghanada."
  (require 'meghanada)
  ;; jump handler
  (add-to-list 'spacemacs-jump-handlers
               '(meghanada-jump-declaration
                 :async spacemacs//java-meghanada-server-livep))
  ;; auto-install meghanada server
  (let ((dest-jar (meghanada--locate-server-jar)))
    (unless dest-jar
      (meghanada-install-server)))
  ;; enable meghanada
  (meghanada-mode))

(defun spacemacs//java-setup-meghanada-company ()
  "Setup Meghanada auto-completion."
  (meghanada-company-enable))

(defun spacemacs//java-setup-meghanada-flycheck ()
  "Setup Meghanada syntax checking."
  (when (spacemacs/enable-flycheck 'java-mode)
    (require 'flycheck-meghanada)
    (add-to-list 'flycheck-checkers 'meghanada)
    (flycheck-mode)))

(defun spacemacs//java-meghanada-server-livep ()
  "Return non-nil if the Meghanada server is up."
  (and meghanada--client-process (process-live-p meghanada--client-process)))


;; Maven

(defun spacemacs/mvn-clean-compile ()
  "Recompile using maven."
  (interactive)
  (mvn-clean)
  (mvn-compile))


;; Gradle

(defun spacemacs/gradle-clean ()
  "Execute 'gradle clean' command."
  (interactive)
  (gradle-execute "clean"))

(defun spacemacs/gradle-clean-build ()
  "Execute 'gradle clean build' command."
  (interactive)
  (gradle-execute "clean build"))

(defun spacemacs/gradle-test-buffer ()
  "Execute 'gradle test' command against current buffer tests."
  (interactive)
  (gradle-single-test (file-name-base (buffer-file-name))))


;; Misc

(defun spacemacs//java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))


;; LSP Java

(defun spacemacs//java-setup-lsp ()
  "Setup LSP Java."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (require 'lsp-java)
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile."))
  (if (configuration-layer/layer-used-p 'dap)
      (progn
        (require 'dap-java)
        (spacemacs/dap-bind-keys-for-mode 'java-mode))
    (message "`dap' layer is not installed, please add `dap' layer to your dotfile.")))

(defun spacemacs//java-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes java-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//java-setup-lsp-flycheck ()
  "Setup LSP Java syntax checking."
  (if (configuration-layer/layer-used-p 'lsp)
      (when (spacemacs/enable-flycheck 'java-mode)
        (require 'lsp-ui-flycheck)
        (lsp-ui-flycheck-enable nil)
        (flycheck-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
