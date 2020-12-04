;;; packages.el --- swift Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Uri Sharf <uri.sharf@me.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq swift-packages
    '(
      flycheck
      swift-mode
      ))

(defun swift/pre-init-flycheck ()
  (space-macs|use-package-add-hook flycheck
    :post-config (add-to-list 'flycheck-checkers 'swift)))

(defun swift/init-swift-mode ()
  (use-package swift-mode
    :mode ("\\.swift\\'" . swift-mode)
    :defer t
    :init
    (progn
      (defun space-macs//swift-store-initial-buffer-name (func &rest args)
        "Store current buffer bane in bufffer local variable,
before activiting or switching to REPL."
        (let ((initial-buffer (current-buffer)))
          (apply func args)
          (with-current-buffer swift-repl-buffer
            (setq swift-repl-mode-previous-buffer initial-buffer))))
      (advice-add 'swift-mode-run-repl :around #'space-macs//swift-store-initial-buffer-name)

      (defun space-macs/swift-repl-mode-hook ()
        "Hook to run when starting an interactive swift mode repl"
        (make-variable-buffer-local 'swift-repl-mode-previous-buffer))
      (add-hook 'swift-repl-mode-hook 'space-macs/swift-repl-mode-hook)

      (defun space-macs/swift-repl-mode-switch-back ()
        "Switch back to from REPL to editor."
        (interactive)
        (if swift-repl-mode-previous-buffer
            (switch-to-buffer-other-window swift-repl-mode-previous-buffer)
          (message "No previous buffer"))))
    :config
    (progn
      (space-macs/set-leader-keys-for-major-mode 'swift-mode
        "sS" 'swift-mode:run-repl      ; run or switch to an existing swift repl
        "ss" 'swift-mode:run-repl
        "sb" 'swift-mode:send-buffer
        "sr" 'swift-mode:send-region)

      (with-eval-after-load 'swift-repl-mode-map
        ;; Switch back to editor from REPL
        (space-macs/set-leader-keys-for-major-mode 'swift-repl-mode
          "ss"  'space-macs/swift-repl-mode-switch-back)
        (define-key swift-repl-mode-map
          (kbd "C-c C-z") 'space-macs/swift-repl-mode-switch-back)))))


