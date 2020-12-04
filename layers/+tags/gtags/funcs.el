;;; funcs.el --- gtags functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/counsel-gtags-maybe-dwim ()
  "Runs `counsel-gtags-dwim' if `gtags-enable-by-default' is on.
Otherwise does nothing."
  (interactive)
  (when gtags-enable-by-default
    (setq space-macs--counsel-gtags-dwim-success nil)
    (setq space-macs--counsel-gtags-dwim-success
          (call-interactively 'counsel-gtags-dwim))))

(defun space-macs//counsel-gtags-dwim-success ()
  "Returns whether or not the last invocation of
  `space-macs/counsel-gtags-maybe-dwim' was a success"
  space-macs--counsel-gtags-dwim-success)

(defun space-macs/counsel-gtags-define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  ;; `counsel-gtags-dwim' is added to the end of the mode-specific jump handlers
  ;; Some modes have more sophisticated jump handlers that go to the beginning
  ;; It might be possible to add `counsel-gtags-dwim' instead to the default
  ;; handlers, if it does a reasonable job in ALL modes.
  (let ((jumpl (intern (format "space-macs-jump-handlers-%S" mode)))
        (handler '(space-macs/counsel-gtags-maybe-dwim
                   :async space-macs//counsel-gtags-dwim-success)))
    (when (boundp jumpl) (add-to-list jumpl handler 'append)))

  ;; TODO: Add missing keybindings when new functions get added
  (space-macs/set-leader-keys-for-major-mode mode
    "gC" 'counsel-gtags-create-tags
    "gd" 'counsel-gtags-dwim
    ;; "gD" 'helm-gtags-find-tag-other-window
    "gf" 'counsel-gtags-find-file
    ;; "gG" 'helm-gtags-dwim-other-window
    ;; "gi" 'helm-gtags-tags-in-this-function
    ;; "gl" 'helm-gtags-parse-file
    "gn" 'counsel-gtags-go-forward
    "gp" 'counsel-gtags-go-backward
    "gr" 'counsel-gtags-find-reference
    ;; "gR" 'helm-gtags-resume
    ;; "gs" 'helm-gtags-select
    ;; "gS" 'helm-gtags-show-stack
    "gy" 'counsel-gtags-find-symbol
    "gu" 'counsel-gtags-update-tags))

(defun helm-gtags-dwim-other-window ()
  "helm-gtags-dwim in the other window"
  (interactive)
  (let ((helm-gtags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-dwim)))

(defun space-macs/helm-gtags-maybe-dwim ()
  "Runs `helm-gtags-dwim' if `gtags-enable-by-default' is on.
Otherwise does nothing."
  (interactive)
  (when gtags-enable-by-default
    (call-interactively 'helm-gtags-dwim)))

(defun space-macs/helm-gtags-define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  ;; The functionality of `helm-gtags-mode' is pretty much entirely superseded
  ;; by `ggtags-mode', so we don't add this hook
  ;; (let ((hook (intern (format "%S-hook" mode))))
  ;;   (add-hook hook 'helm-gtags-mode))

  ;; `helm-gtags-dwim' is added to the end of the mode-specific jump handlers
  ;; Some modes have more sophisticated jump handlers that go to the beginning
  ;; It might be possible to add `helm-gtags-dwim' instead to the default
  ;; handlers, if it does a reasonable job in ALL modes.
  (let ((jumpl (intern (format "space-macs-jump-handlers-%S" mode))))
    (when (boundp jumpl)
      (add-to-list jumpl 'space-macs/helm-gtags-maybe-dwim 'append)))

  (space-macs/set-leader-keys-for-major-mode mode
    "gC" 'helm-gtags-create-tags
    "gd" 'helm-gtags-find-tag
    "gD" 'helm-gtags-find-tag-other-window
    "gf" 'helm-gtags-select-path
    "gG" 'helm-gtags-dwim-other-window
    "gi" 'helm-gtags-tags-in-this-function
    "gl" 'helm-gtags-parse-file
    "gn" 'helm-gtags-next-history
    "gp" 'helm-gtags-previous-history
    "gr" 'helm-gtags-find-rtag
    "gR" 'helm-gtags-resume
    "gs" 'helm-gtags-select
    "gS" 'helm-gtags-show-stack
    "gy" 'helm-gtags-find-symbol
    "gu" 'helm-gtags-update-tags))

(defun space-macs/ggtags-mode-enable ()
  "Enable ggtags and eldoc mode.

For eldoc, ggtags advises the eldoc function at the lowest priority
so that if the major mode has better support it will use it first."
  (when gtags-enable-by-default
    (ggtags-mode 1)
    (eldoc-mode 1)))


