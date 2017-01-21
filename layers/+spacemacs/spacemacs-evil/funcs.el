;;; funcs.el --- Spacemacs Evil Layer functions File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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


;; evil-search-highlight-persist

(defun spacemacs/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (case evil-search-module
    ('isearch (evil-search-highlight-persist-remove-all))
    ('evil-search (evil-ex-nohighlight))))

(defun spacemacs//adaptive-evil-highlight-persist-face ()
  (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                      :inherit 'lazy-highlight
                      :background nil
                      :foreground nil))

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
