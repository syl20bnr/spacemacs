;;; extensions.el --- Windows Scripts Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Post extensions are loaded *after* the packages
(setq windows-scripts-post-extensions '(dos))

;; Initialize the extensions

(defun windows-scripts/init-dos ()
  (use-package dos
    :commands dos-mode
    :mode ("\\.bat$" . dos-mode)
    :init
    (progn
      (defun windows-scripts/dos-outline-hook ()
        (defun outline-mouse-select ()
          "Select position and return to `dos-mode'."
          (interactive)
          (dos-mode)
          (beginning-of-line)))
      (defun windows-scripts/dos-outline ()
        "Set a local binding to be able to return easily in dos-mode."
        (interactive)
        (dos-outline)
        (define-key evil-normal-state-local-map (kbd "SPC m z") 'dos-mode))
      (add-hook 'outline-mode-hook 'windows-scripts/dos-outline-hook))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'dos-mode
      "hD" 'dos-help-cmd
      "eb" 'dos-run
      "eB" 'dos-run-args
      "s"  'dos-sep
      "t"  'dos-template-mini
      "T"  'dos-template
      "z"  'windows-scripts/dos-outline)))
