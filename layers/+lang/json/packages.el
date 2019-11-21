;;; packages.el --- JSON Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq json-packages
      '(
        add-node-modules-path
        flycheck
        json-mode
        json-navigator
        json-reformat
        json-snatcher
        ))

(defun json/post-init-add-node-modules-path ()
  (add-hook 'json-mode-hook #'add-node-modules-path))

(defun json/post-init-flycheck ()
  (spacemacs/enable-flycheck 'json-mode))

(defun json/init-json-mode ()
  (use-package json-mode
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'json-mode "m=" "format")
      (spacemacs/set-leader-keys-for-major-mode 'json-mode
        "==" #'spacemacs/json-format)
      (when json-fmt-on-save
        (add-hook 'json-mode-hook #'spacemacs/json-setup-fmt-on-save)))))

(defun json/init-json-navigator ()
  (use-package json-navigator
    :defer t
    :init
    (progn
      (evilified-state-evilify-map tabulated-list-mode-map
        :mode special-mode)
      (spacemacs/set-leader-keys-for-major-mode 'json-mode
        "hh" 'spacemacs/json-navigator-dwim))))

(defun json/init-json-reformat ()
  (use-package json-reformat
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'json-mode
      "=b" #'spacemacs/json-reformat-dwim
      "=r" #'json-reformat-region)))

(defun json/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'json-mode
      "hp" 'jsons-print-path)))
