;;; packages.el --- swift Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Uri Sharf <uri.sharf@me.com>
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


(setq swift-packages
    '(
      flycheck
      swift-mode
      ))

(defun swift/pre-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    :post-config (add-to-list 'flycheck-checkers 'swift)))

(defun swift/init-swift-mode ()
  (use-package swift-mode
    :mode ("\\.swift\\'" . swift-mode)
    :defer t
    :init
    (defun spacemacs//swift-store-initial-buffer-name (func &rest args)
      "Store current buffer bane in bufffer local variable,
before activiting or switching to REPL."
      (let ((initial-buffer (current-buffer)))
        (apply func args)
        (with-current-buffer swift-repl-buffer
          (setq swift-repl-mode-previous-buffer initial-buffer))))
    (advice-add 'swift-mode-run-repl :around #'spacemacs//swift-store-initial-buffer-name)

    (defun spacemacs/swift-repl-mode-hook ()
      "Hook to run when starting an interactive swift mode repl"
      (make-variable-buffer-local 'swift-repl-mode-previous-buffer))
    (add-hook 'swift-repl-mode-hook 'spacemacs/swift-repl-mode-hook)

    (defun spacemacs/swift-repl-mode-switch-back ()
      "Switch back to from REPL to editor."
      (interactive)
      (if swift-repl-mode-previous-buffer
          (switch-to-buffer-other-window swift-repl-mode-previous-buffer)
        (message "No previous buffer")))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'swift-mode
      "sS" 'swift-mode:run-repl      ; run or switch to an existing swift repl
      "ss" 'swift-mode:run-repl
      "sb" 'swift-mode:send-buffer
      "sr" 'swift-mode:send-region)

    (with-eval-after-load 'swift-repl-mode-map
      ;; Switch back to editor from REPL
      (spacemacs/set-leader-keys-for-major-mode 'swift-repl-mode
        "ss"  'spacemacs/swift-repl-mode-switch-back)
      (define-key swift-repl-mode-map
        (kbd "C-c C-z") 'spacemacs/swift-repl-mode-switch-back))))
