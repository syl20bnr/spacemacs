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



(defun spacemacs//evil-lisp-more-keys ()
  "Some evil-lisp-state changes:
* Insert-mode returns to evil-lisp-state
* [C g] returns to normal-mode from evil-lisp-state or evil-lisp-insert-state
* Leader key available"
  (progn
    (defun lisp-state-insert-sexp-after ()
      "Insert sexp after the current one."
      (interactive)
      (let ((sp-navigate-consider-symbols nil))
        (if (char-equal (char-after) ?\() (forward-char))
        (sp-up-sexp)
        (evil-lisp-insert-state
         (sp-newline)
         (sp-insert-pair "("))))

    (defun lisp-state-insert-sexp-before ()
      "Insert sexp before the current one."
      (interactive)
      (let ((sp-navigate-consider-symbols nil))
        (if (char-equal (char-after) ?\() (forward-char))
        (sp-backward-sexp)
        (evil-lisp-insert-state)
        (sp-newline)
        (evil-previous-visual-line)
        (evil-end-of-line)
        (insert " ")
        (sp-insert-pair "(")
        (indent-for-tab-command)))
    (define-key evil-lisp-state-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (define-key evil-lisp-state-map "i"   'evil-lisp-insert-state)
    (define-key evil-lisp-state-map (kbd "C-g") 'evil-lisp-state/quit)
    (define-key evil-lisp-insert-state-map (kbd "C-g") 'evil-lisp-state/quit)
    (define-key evil-lisp-insert-state-map [escape]    'evil-lisp-state)
    (spacemacs//lisp-insert-state-hybrid dotspacemacs-editing-style)
    (add-hook 'spacemacs-editing-style-hook
              #'spacemacs//lisp-insert-state-hybrid)))

(defvar spacemacs--evil-lisp-insert-states-default nil
  "Default value of the list of additional states enabled in \
`evil-lisp-insert-state'.")

(defvar spacemacs--evil-lisp-insert-states-hybrid nil
  "List of additional states enabled in `evil-lisp-insert-state' when
`hybrid-mode' is active.")

(defun spacemacs//lisp-insert-state-hybrid (style)
  "If STYLE is hybrid, update `evil-lisp-insert-state' definition to enable
`evil-hybrid-state' instead of `evil-insert-state'.
Otherwise, revert to the default behavior (i.e. enable `evil-insert-state')."
  ;; Populate variables on the first invocation.
  (unless spacemacs--evil-lisp-insert-states-default
    (setq spacemacs--evil-lisp-insert-states-default
          (evil-get-property evil-state-properties 'lisp-insert :enable))
    (setq spacemacs--evil-lisp-insert-states-hybrid
          (mapcar (lambda (item)
                    (if (eq item 'insert) 'hybrid item))
                  spacemacs--evil-lisp-insert-states-default)))
  (let ((states (if (eq style 'hybrid)
                    spacemacs--evil-lisp-insert-states-hybrid
                  spacemacs--evil-lisp-insert-states-default)))
    (evil-put-property 'evil-state-properties 'lisp-insert
                       :enable states)))



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
