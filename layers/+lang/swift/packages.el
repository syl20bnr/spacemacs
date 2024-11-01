;;; packages.el --- Swift Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


(defconst swift-packages
  '(
    company
    flycheck
    (lsp-sourcekit :requires lsp-mode :toggle (eq swift-backend 'lsp))
    swift-mode))

(defun swift/init-lsp-sourcekit ()
  (use-package lsp-sourcekit
    :defer t
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable swift-lsp-executable-path)))

(defun swift/post-init-company ()
  (add-hook 'swift-mode-local-vars-hook 'spacemacs//swift-setup-company))

(defun swift/post-init-flycheck ()
  (spacemacs/enable-flycheck 'swift-mode))

(defun swift/init-swift-mode ()
  (use-package swift-mode
    :mode ("\\.swift\\'" . swift-mode)
    :hook (swift-mode-local-vars . spacemacs//swift-setup-backend)
    :defer t
    :init
    (spacemacs/declare-prefix-for-mode 'swift-mode "ms" "REPL")
    (defun spacemacs//swift-store-initial-buffer-name (func &rest args)
      "Store current buffer bane in buffer local variable,
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
      "'"  'swift-mode:run-repl
      "ss" 'swift-mode:run-repl
      "sb" 'swift-mode:send-buffer
      "sr" 'swift-mode:send-region)

    (with-eval-after-load 'swift-repl-mode-map
      ;; Switch back to editor from REPL
      (spacemacs/declare-prefix-for-mode 'swift-repl-mode "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'swift-repl-mode
        "ss"  'spacemacs/swift-repl-mode-switch-back)
      (define-key swift-repl-mode-map
                  (kbd "C-c C-z") 'spacemacs/swift-repl-mode-switch-back))))
