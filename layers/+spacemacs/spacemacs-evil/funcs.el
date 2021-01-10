;;; funcs.el --- Spacemacs Evil Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs--evil-iedit-insert-states-default nil
  "Default value of the list of additional states enabled in \
`evil-iedit-insert-state'.")

(defvar spacemacs--evil-iedit-insert-states-hybrid nil
  "List of additional states enabled in `evil-iedit-insert-state' when
`hybrid-mode' is active.")

(defun spacemacs//enable-hs-minor-mode ()
  "Enable hs-minor-mode for code folding."
  (ignore-errors
    (hs-minor-mode)
    (spacemacs|hide-lighter hs-minor-mode)))

(defun spacemacs//iedit-insert-state-hybrid (style)
  "If STYLE is hybrid, update `evil-iedit-insert-state' definition to enable
`evil-hybrid-state' instead of `evil-insert-state'.
Otherwise, revert to the default behavior (i.e. enable `evil-insert-state')."
  ;; Populate variables on the first invocation.
  (unless spacemacs--evil-iedit-insert-states-default
    (setq spacemacs--evil-iedit-insert-states-default
          (evil-get-property evil-state-properties 'iedit-insert :enable))
    (setq spacemacs--evil-iedit-insert-states-hybrid
          (mapcar (lambda (item)
                    (if (eq item 'insert) 'hybrid item))
                  spacemacs--evil-iedit-insert-states-default)))
  (let ((states (if (eq style 'hybrid)
                    spacemacs--evil-iedit-insert-states-hybrid
                  spacemacs--evil-iedit-insert-states-default)))
    (evil-put-property 'evil-state-properties 'iedit-insert
                       :enable states)))

(defun spacemacs//iedit-state-TAB-key-bindings (style)
  "Set the action for TAB key in iedit state."
  (if (memq style '(vim hybrid))
      (progn
        (define-key iedit-occurrence-keymap-default
          (kbd "TAB") 'iedit-toggle-selection)
        (define-key iedit-occurrence-keymap-default
          [tab] 'iedit-toggle-selection))
    (progn
      (define-key iedit-occurrence-keymap-default
        (kbd "TAB") 'iedit-next-occurrence)
      (define-key iedit-occurrence-keymap-default
        [tab] 'iedit-next-occurrence))))

(defun spacemacs//evil-escape-deactivate-in-holy-mode  (style)
  "Deactivate `evil-escape' if STYLE is `emacs' otherwise enable it."
  (if (memq style '(vim hybrid))
      (evil-escape-mode t)
    (evil-escape-mode -1)))

(defun spacemacs/linum-relative-toggle ()
  (interactive)
  (if (not (bound-and-true-p linum-relative-mode))
      (linum-mode))
  (linum-relative-toggle))


;; vi-tilde-fringe

(defun spacemacs/disable-vi-tilde-fringe ()
  "Disable `vi-tilde-fringe' in the current buffer."
  (vi-tilde-fringe-mode -1))

(defun spacemacs/disable-vi-tilde-fringe-read-only ()
  "Disable `vi-tilde-fringe' in the current buffer if it is read only."
  (when buffer-read-only
    (spacemacs/disable-vi-tilde-fringe)))


;; lisp state

(defun spacemacs//load-evil-lisp-state ()
  "Load evil-lisp-state lazily"
  (require 'evil-lisp-state))
