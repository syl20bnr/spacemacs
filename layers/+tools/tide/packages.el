;;; packages.el --- Tide Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Ting Zhou <ztlevi1993@gmail.coom>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq tide-packages
      '(tide))

(defun tide/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
    :config
    (progn
      (setq tide-completion-detailed t
            tide-always-show-documentation t)

      (spacemacs/declare-prefix-for-mode 'typescript-mode "mE" "errors")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mE" "errors")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "name")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mn" "name")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "mS" "server")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "mS" "server")
      (spacemacs/declare-prefix-for-mode 'typescript-mode "ms" "send")
      (spacemacs/declare-prefix-for-mode 'typescript-tsx-mode "ms" "send")

      (spacemacs|hide-lighter tide-mode)
      ;; Set jump handler for tide with the given MODE.
      (dolist (m tide--key-bindings-modes)
        (add-to-list (intern (format "spacemacs-jump-handlers-%S" m))
                     '(tide-jump-to-definition :async t)))

      (if tide-remap-xref-keybindings
          (progn
            (define-key tide-mode-map [remap xref-find-definitions] #'tide-jump-to-definition)
            (define-key tide-mode-map [remap xref-find-references] #'tide-references)))

      (advice-add #'tide-project-root :override #'spacemacs//tide-project-root)

      ;; cleanup tsserver when no tide buffers are left
      (add-hook 'tide-mode-hook 'kill-tide-hook))))
