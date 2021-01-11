;;; packages.el --- Scala Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst scala-packages
  '(
    lsp-mode
    lsp-metals
    dap-mode
    flycheck
    flyspell
    counsel-gtags
    ggtags
    helm-gtags
    sbt-mode
    scala-mode))

(defun scala/post-init-flycheck ()
  (spacemacs/enable-flycheck 'scala-mode))

(defun scala/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'scala-mode))

(defun scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :config
    ;; WORKAROUND: https://github.com/hvesalai/emacs-sbt-mode/issues/31
    ;; allows for using SPACE in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false"))
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'scala-mode "mb" "sbt")
      (spacemacs/declare-prefix-for-mode 'scala-mode "mg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'scala-mode
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
      ;; Automatically insert asterisk in a comment when enabled
      (defun scala/newline-and-indent-with-asterisk ()
        (interactive)
        (newline-and-indent)
        (when scala-auto-insert-asterisk-in-comments
          (scala-indent:insert-asterisk-on-multiline-comment)))

      (evil-define-key 'insert scala-mode-map
        (kbd "RET") 'scala/newline-and-indent-with-asterisk)

      (evil-define-key 'normal scala-mode-map "J" 'spacemacs/scala-join-line)

      (pcase scala-sbt-window-position
        ('bottom (setq sbt:display-buffer-action
                       (list #'spacemacs//scala-display-sbt-at-bottom))))

      ;; Compatibility with `aggressive-indent'
      (setq scala-indent:align-forms t
            scala-indent:align-parameters t
            scala-indent:default-run-on-strategy
            scala-indent:operator-strategy))))

(defun scala/pre-init-dap-mode ()
  (when (spacemacs//scala-backend-metals-p)
    (add-to-list 'spacemacs--dap-supported-modes 'scala-mode))
  (spacemacs//scala-setup-dap))

(defun scala/post-init-lsp-mode ()
  (when (spacemacs//scala-backend-metals-p)
    (spacemacs//scala-setup-metals)))

(defun scala/init-lsp-metals ()
  (use-package lsp-metals
    :defer t
    :init
    (spacemacs//scala-setup-treeview)))

(defun scala/post-init-ggtags ()
  (when scala-enable-gtags
    (add-hook 'scala-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)))

(defun scala/post-init-counsel-gtags ()
  (when scala-enable-gtags
    (spacemacs/counsel-gtags-define-keys-for-mode 'scala-mode)))

(defun scala/post-init-helm-gtags ()
  (when scala-enable-gtags
    (spacemacs/helm-gtags-define-keys-for-mode 'scala-mode)))
