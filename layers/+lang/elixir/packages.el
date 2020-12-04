;;; packages.el --- Elixir Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst elixir-packages
  '(
    alchemist
    company
    counsel-gtags
    dap-mode
    elixir-mode
    evil-matchit
    flycheck
    flycheck-credo
    ggtags
    helm-gtags
    ob-elixir
    popwin
    smartparens))

(defun elixir/init-alchemist ()
  (use-package alchemist
    :if (eq elixir-backend 'alchemist)
    :defer t
    :init
    (progn
      (space-macs/register-repl 'alchemist 'alchemist-iex-run "alchemist")
      (add-hook 'elixir-mode-hook 'alchemist-mode)
      (setq alchemist-project-compile-when-needed t
            alchemist-test-status-modeline nil)
      (add-to-list 'space-macs-jump-handlers-elixir-mode
                   '(alchemist-goto-definition-at-point :async t)))
    :config
    (space-macs/declare-prefix-for-mode 'elixir-mode "mX" "hex")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mc" "compile")
    (space-macs/declare-prefix-for-mode 'elixir-mode "me" "eval")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mg" "goto")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mh" "help")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mm" "mix")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mo" "macroexpand")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mp" "project")
    (space-macs/declare-prefix-for-mode 'elixir-mode "ms" "iex")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mt" "test")
    (space-macs/declare-prefix-for-mode 'elixir-mode "mx" "execute")
    (space-macs/declare-prefix-for-mode 'elixir-mode "md" "debug")
    (space-macs/set-leader-keys-for-major-mode 'elixir-mode
      "el" 'alchemist-eval-current-line
      "eL" 'alchemist-eval-print-current-line
      "er" 'alchemist-eval-region
      "eR" 'alchemist-eval-print-region
      "eb" 'alchemist-eval-buffer
      "eB" 'alchemist-eval-print-buffer
      "ej" 'alchemist-eval-quoted-current-line
      "eJ" 'alchemist-eval-print-quoted-current-line
      "eu" 'alchemist-eval-quoted-region
      "eU" 'alchemist-eval-print-quoted-region
      "ev" 'alchemist-eval-quoted-buffer
      "eV" 'alchemist-eval-print-quoted-buffer

      "gt" 'alchemist-project-toggle-file-and-tests
      "gT" 'alchemist-project-toggle-file-and-tests-other-window

      "h:" 'alchemist-help
      "hH" 'alchemist-help-history
      "hh" 'alchemist-help-search-at-point
      "hr" 'alchemist-help--search-marked-region

      "m:" 'alchemist-mix
      "mc" 'alchemist-mix-compile
      "mx" 'alchemist-mix-run

      "'"  'alchemist-iex-run
      "sc" 'alchemist-iex-compile-this-buffer
      "si" 'alchemist-iex-run
      "sI" 'alchemist-iex-project-run
      "sl" 'alchemist-iex-send-current-line
      "sL" 'alchemist-iex-send-current-line-and-go
      "sm" 'alchemist-iex-reload-module
      "sr" 'alchemist-iex-send-region
      "sR" 'alchemist-iex-send-region-and-go

      "ta" 'alchemist-mix-test
      "tb" 'alchemist-mix-test-this-buffer
      "tB" 'alchemist-project-run-tests-for-current-file
      "tt" 'alchemist-mix-test-at-point
      "tF" 'alchemist-project-find-test
      "tf" 'alchemist-mix-test-file
      "tn" 'alchemist-test-mode-jump-to-next-test
      "tN" 'alchemist-test-mode-jump-to-previous-test
      "tr" 'alchemist-mix-rerun-last-test
      "ts" 'alchemist-mix-test-stale
      "tR" 'alchemist-test-toggle-test-report-display

      "xb" 'alchemist-execute-this-buffer
      "xf" 'alchemist-execute-file
      "x:" 'alchemist-execute

      "cb" 'alchemist-compile-this-buffer
      "cf" 'alchemist-compile-file
      "c:" 'alchemist-compile

      "gg" 'alchemist-goto-definition-at-point
      "." 'alchemist-goto-definition-at-point
      "gb" 'alchemist-goto-jump-back
      ","  'alchemist-goto-jump-back
      "gN" 'alchemist-goto-jump-to-previous-def-symbol
      "gn" 'alchemist-goto-jump-to-next-def-symbol
      "gj" 'alchemist-goto-list-symbol-definitions

      "Xi" 'alchemist-hex-info-at-point
      "Xr" 'alchemist-hex-releases-at-point
      "XR" 'alchemist-hex-releases
      "XI" 'alchemist-hex-info
      "Xs" 'alchemist-hex-search

      "ol" 'alchemist-macroexpand-once-current-line
      "oL" 'alchemist-macroexpand-once-print-current-line
      "ok" 'alchemist-macroexpand-current-line
      "oK" 'alchemist-macroexpand-print-current-line
      "oi" 'alchemist-macroexpand-once-region
      "oI" 'alchemist-macroexpand-once-print-region
      "or" 'alchemist-macroexpand-region
      "oR" 'alchemist-macroexpand-print-region

      "db" 'space-macs/elixir-toggle-breakpoint)

    (dolist (mode (list alchemist-compile-mode-map
                        alchemist-eval-mode-map
                        alchemist-execute-mode-map
                        alchemist-message-mode-map
                        alchemist-help-minor-mode-map
                        alchemist-mix-mode-map
                        alchemist-macroexpand-mode-map
                        alchemist-refcard-mode-map
                        alchemist-test-report-mode-map))
      (evil-define-key 'normal mode
        (kbd "q") 'quit-window))))

(defun elixir/post-init-company ()
  ;; backend specific
  (add-hook 'elixir-mode-local-vars-hook #'space-macs//elixir-setup-company))

(defun elixir/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'elixir-mode))

(defun elixir/pre-init-dap-mode ()
  (pcase (space-macs//elixir-backend)
    (`lsp (add-to-list 'space-macs--dap-supported-modes 'elixir-mode)))
  (add-hook 'elixir-mode-local-vars-hook #'space-macs//elixir-setup-dap))

(defun elixir/post-init-evil-matchit ()
  (add-hook 'elixir-mode-hook `turn-on-evil-matchit-mode))

(defun elixir/init-elixir-mode ()
  (use-package elixir-mode
    :defer t
    :init (space-macs/add-to-hook 'elixir-mode-hook
                                 '(space-macs//elixir-setup-backend
                                   space-macs//elixir-default))
    :config
    (space-macs/set-leader-keys-for-major-mode 'elixir-mode
      "=" 'elixir-format)))

(defun elixir/post-init-flycheck ()
  (space-macs/enable-flycheck 'elixir-mode))

(defun elixir/init-flycheck-credo ()
  (use-package flycheck-credo
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-credo-setup)))

(defun elixir/post-init-ggtags ()
  (add-hook 'elixir-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun elixir/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'elixir-mode))

(defun elixir/pre-init-ob-elixir ()
  (space-macs|use-package-add-hook org
    :post-config
    (use-package ob-elixir
      :init (add-to-list 'org-babel-load-languages '(elixir . t)))))
(defun elixir/init-ob-elixir ())

(defun elixir/pre-init-popwin ()
  (space-macs|use-package-add-hook popwin
    :post-config
    (push '("*mix*" :tail t :noselect t) popwin:special-display-config)))

(defun elixir/pre-init-smartparens ()
  (space-macs|use-package-add-hook smartparens
    :post-config
    (progn
      (sp-with-modes '(elixir-mode)
        (sp-local-pair
         "(" ")"
         :unless '(:add space-macs//elixir-point-after-fn-p))
        (sp-local-pair
         "fn" "end"
         :when '(("SPC" "RET" "-" "("))
         :post-handlers '(:add space-macs//elixir-do-end-close-action)
         :actions '(insert)))
      (sp-with-modes '(elixir-mode)
        (sp-local-pair
         "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(:add space-macs//elixir-do-end-close-action)
         :actions '(insert))))))


