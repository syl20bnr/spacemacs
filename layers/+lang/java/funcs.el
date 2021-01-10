;;; funcs.el --- Java functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//java-backend ()
  "Returns selected backend."
  (if java-backend
      java-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'meghanada))))

(defun spacemacs//java-setup-backend ()
  "Conditionally setup java backend."
  (pcase (spacemacs//java-backend)
    (`meghanada (spacemacs//java-setup-meghanada))
    (`lsp (spacemacs//java-setup-lsp))))

(defun spacemacs//java-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//java-backend)
    (`meghanada (spacemacs//java-setup-meghanada-company))))

(defun spacemacs//java-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//java-backend)
    (`lsp (spacemacs//java-setup-lsp-dap))))

(defun spacemacs//java-setup-flycheck ()
  "Conditionally setup flycheck based on backend."
  (pcase (spacemacs//java-backend)
    (`meghanada (spacemacs//java-setup-meghanada-flycheck))
    (`lsp (spacemacs//java-setup-lsp-flycheck))))


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
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//java-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-java)
  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    ;; debug
    "ddj" 'dap-java-debug
    "dtt" 'dap-java-debug-test-method
    "dtc" 'dap-java-debug-test-class
    ;; run
    "tt" 'dap-java-run-test-method
    "tc" 'dap-java-run-test-class))

(defun spacemacs//java-setup-lsp-flycheck ()
  "Setup LSP Java syntax checking."
  (unless (configuration-layer/layer-used-p 'lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
