;;; packages.el --- Scala Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst scala-packages
  '(
    lsp-mode
    (lsp-metals :toggle (space-macs//scala-backend-metals-p))
    dap-mode
    eldoc
    flycheck
    flyspell
    counsel-gtags
    ggtags
    helm-gtags
    (ensime :toggle (space-macs//scala-backend-ensime-p))
    sbt-mode
    scala-mode))

(defun scala/post-init-eldoc ()
  (when (and scala-enable-eldoc (space-macs//scala-backend-ensime-p))
    (add-hook 'scala-mode-hook #'space-macs//scala-setup-ensime-eldoc)))

(defun scala/init-ensime ()
  (use-package ensime
    :defer t
    :if (space-macs//scala-backend-ensime-p)
    :init
    (progn
      (setq ensime-startup-dirname (concat space-macs-cache-directory "ensime/"))
      (space-macs/register-repl 'ensime 'ensime-inf-switch "ensime")
      (add-hook 'scala-mode-hook #'space-macs//scala-setup-ensime)
      (when scala-auto-start-backend
        (add-hook 'scala-mode-hook 'space-macs//ensime-maybe-start)))
    :config
    (progn
      ;; This function was renamed in ensime. Usually we don't need to do this,
      ;; but documentation recommends the stable version of ensime, so we must
      ;; try to support it, too.
      (unless (fboundp 'ensime-type-at-point)
        (defalias 'ensime-type-at-point 'ensime-print-type-at-point))

      ;; key bindings
      (dolist (mode '(scala-mode))
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
          (space-macs/declare-prefix-for-mode mode (car prefix) (cdr prefix)))
        (space-macs/set-leader-keys-for-major-mode mode
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
          "Dr"     'space-macs/ensime-gen-and-restart
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
          "sB"     'space-macs/ensime-inf-eval-buffer-switch
          "si"     'ensime-inf-switch
          "sr"     'ensime-inf-eval-region
          "sR"     'space-macs/ensime-inf-eval-region-switch

          "yT"     'space-macs/ensime-yank-type-at-point-full-name
          "yt"     'space-macs/ensime-yank-type-at-point

          "z"      'ensime-expand-selection-command))
      (evil-define-key 'insert ensime-mode-map
        (kbd ".") 'space-macs/ensime-completing-dot
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
        (kbd "q") 'space-macs/ensime-refactor-cancel
        (kbd "c") 'space-macs/ensime-refactor-accept
        (kbd "RET") 'space-macs/ensime-refactor-accept)
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
        (kbd "C-i") 'ensime-search-insert-import-of-current-result)

      ;; Enable Expand Region integration from Ensime.  Ignore load errors to
      ;; handle older Ensime versions gracefully.
      (when (configuration-layer/package-used-p 'expand-region)
        (require 'ensime-expand-region nil 'noerror)))))

(defun scala/post-init-flycheck ()
  (space-macs/enable-flycheck 'scala-mode)
  ;; Don't use scala checker if ensime mode is active, since it provides
  ;; better error checking.
  (when (space-macs//scala-backend-ensime-p)
    (with-eval-after-load 'flycheck
      (add-hook 'ensime-mode-hook 'space-macs//scala-disable-flycheck-scala))))

(defun scala/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'scala-mode)
  (when (space-macs//scala-backend-ensime-p)
    (add-hook 'scala-mode-hook #'space-macs//scala-setup-ensime-flyspell)))

(defun scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :config
    ;; WORKAROUND: https://github.com/ensime/e-macs-sbt-mode/issues/31
    ;; allows for using SPACE in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/e-macs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false"))
    :init
    (progn
      (space-macs/declare-prefix-for-mode 'scala-mode "mb" "sbt")
      (space-macs/declare-prefix-for-mode 'scala-mode "mg" "goto")
      (space-macs/set-leader-keys-for-major-mode 'scala-mode
        "b." 'sbt-hydra
        "bb" 'sbt-command))))

(defun scala/init-scala-mode ()
  (use-package scala-mode
    :defer t
    :init
    (progn
      (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
        (add-to-list 'completion-ignored-extensions ext)))
    :config
    (progn
      ;; Ensure only one of metals and ensime is loaded
      (unless (space-macs//scala-backend-ensime-p)
        (progn
          (fmakunbound 'ensime)
          (remove-hook 'after-change-functions 'ensime-after-change-function)
          (remove-hook 'window-configuration-change-hook
                       'ensime-show-left-margin-hook)))

      ;; Automatically insert asterisk in a comment when enabled
      (defun scala/newline-and-indent-with-asterisk ()
        (interactive)
        (newline-and-indent)
        (when scala-auto-insert-asterisk-in-comments
          (scala-indent:insert-asterisk-on-multiline-comment)))

      (evil-define-key 'insert scala-mode-map
        (kbd "RET") 'scala/newline-and-indent-with-asterisk)

      (evil-define-key 'normal scala-mode-map "J" 'space-macs/scala-join-line)

      ;; Compatibility with `aggressive-indent'
      (setq scala-indent:align-forms t
            scala-indent:align-parameters t
            scala-indent:default-run-on-strategy
            scala-indent:operator-strategy))))

(defun scala/pre-init-dap-mode ()
  (when (space-macs//scala-backend-metals-p)
    (add-to-list 'space-macs--dap-supported-modes 'scala-mode))
  (space-macs//scala-setup-dap))

(defun scala/post-init-lsp-mode ()
  (when (space-macs//scala-backend-metals-p)
    (space-macs//scala-setup-metals)))

(defun scala/init-lsp-metals ()
  (use-package lsp-metals
    :defer t
    :init
    (space-macs//scala-setup-treeview)))

(defun scala/post-init-ggtags ()
  (when scala-enable-gtags
    (add-hook 'scala-mode-local-vars-hook #'space-macs/ggtags-mode-enable)))

(defun scala/post-init-counsel-gtags ()
  (when scala-enable-gtags
    (space-macs/counsel-gtags-define-keys-for-mode 'scala-mode)))

(defun scala/post-init-helm-gtags ()
  (when scala-enable-gtags
    (space-macs/helm-gtags-define-keys-for-mode 'scala-mode)))


