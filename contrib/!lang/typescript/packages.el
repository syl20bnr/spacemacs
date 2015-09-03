;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Chris Bowdon & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typescript-private-packages '(
                                    tss
                                    flycheck))

(setq test-layer-excluded-packages '(flymake))

(defun typescript-private/post-init-flycheck ()
  "Add hook for flycheck"
  (use-package flycheck
    :defer t
    :config (progn
              ;; ;;; remove hook for flymake, as it seems to conflict.
              ;; (add-hook 'typescript-mode-hook 'flycheck-mode (flymake-mode nil))
              ;; (add-hook 'typescript-mode-hook (lambda flymake-mode nil))
              ;;; This Code Snippet was from [Here](https://github.com/caisah/flycheck-typescript) and is licensed under the GPL
              (flycheck-define-checker typescript
                "A TypeScript syntax checker using tsc command."
                :command ("tsc" "--out" "/dev/null" source-inplace)
                :error-patterns
                ((error line-start (file-name) "(" line "," column "): error " (message) line-end))
                :modes (typescript-mode))

              (add-to-list 'flycheck-checkers 'typescript)
              )
    )
  )


(defun typescript-private/init-tss ()
  "Initialize tss"
  (use-package tss
    :defer t
    :mode (
           ("\\.ts\\'" . typescript-mode)
           ))
  :init (evil-leader/set-key-for-mode 'flycheck-mode
          "mgg" 'tss-jump-to-definition
          "mhh" 'tss-popup-help)

  )

(defun typescript-private/post-init-tss ()
  "Initialize tss"
  (use-package tss
    :defer t
    :config (progn
              ;; Remove Flymake mode from hook.
              (advice-add 'tss-setup-current-buffer :before (lambda flymake-mode nil))

              (add-hook 'typescript-mode-hook 'tss-setup-current-buffer t)
              (add-hook 'kill-buffer-hook 'tss--delete-process t)
              )))
