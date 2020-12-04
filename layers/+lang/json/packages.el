;;; packages.el --- JSON Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq json-packages
      '(
        company
        add-node-modules-path
        flycheck
        json-mode
        json-navigator
        json-reformat
        json-snatcher
        prettier-js
        web-beautify))

(defun json/post-init-company ()
  (space-macs//json-setup-company))

(defun json/post-init-add-node-modules-path ()
  (add-hook 'json-mode-hook #'add-node-modules-path))

(defun json/post-init-flycheck ()
  (space-macs/enable-flycheck 'json-mode))

(defun json/init-json-mode ()
  (use-package json-mode
    :defer t
    :init
    (progn
      (unless (eq (space-macs//json-backend) 'lsp)
        (space-macs/declare-prefix-for-mode 'json-mode "mT" "toggle")
        (space-macs/declare-prefix-for-mode 'json-mode "mh" "help")
        (space-macs/declare-prefix-for-mode 'json-mode "m=" "format"))
      (add-hook 'json-mode-hook #'space-macs//json-setup-backend))))

(defun json/init-json-navigator ()
  (use-package json-navigator
    :defer t
    :init
    (progn
      (evilified-state-evilify-map tabulated-list-mode-map
        :mode special-mode)
      (space-macs/set-leader-keys-for-major-mode 'json-mode
        "Th" 'space-macs/json-navigator-dwim))))

(defun json/init-json-reformat ()
  (use-package json-reformat
    :defer t
    :init
    (space-macs/set-leader-keys-for-major-mode 'json-mode
      "==" 'space-macs/json-reformat-dwim)))

(defun json/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (space-macs/set-leader-keys-for-major-mode 'json-mode
      "hp" 'jsons-print-path)))

(defun json/pre-init-prettier-js ()
  (when (eq json-fmt-tool 'prettier)
    (add-to-list 'space-macs--prettier-modes 'json-mode)
    (add-hook 'json-mode-hook #'space-macs/json-setup-prettier)
    (when (eq json-fmt-on-save t)
      (add-hook 'json-mode-hook 'prettier-js-mode))))

(defun json/pre-init-web-beautify ()
  (when (eq json-fmt-tool 'web-beautify)
    (add-to-list 'space-macs--web-beautify-modes
                 (cons 'json-mode 'web-beautify-js))
    (when (eq json-fmt-on-save t)
      (add-hook 'json-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))))


