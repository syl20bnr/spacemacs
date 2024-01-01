;;; funcs.el --- Auto-completion functions File -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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




(spacemacs|add-toggle auto-completion
  :status
  (if (eq 'company auto-completion-front-end)
      (bound-and-true-p company-mode)
    (bound-and-true-p auto-complete-mode))
  :on
  (progn
    (if (eq 'company auto-completion-front-end)
        (company-mode)
      (auto-complete-mode))
    (message "Enabled auto-completion (using %S)."
             auto-completion-front-end))
  :off
  (progn
    (if (eq 'company auto-completion-front-end)
        (company-mode -1)
      (auto-complete-mode -1))
    (message "Disabled auto-completion."))
  :documentation "Enable auto-completion."
  :evil-leader "ta")


;; company backends declaration macro

(defmacro spacemacs|add-company-backends (&rest props)
  "Add and enable company backends.
This function should be called exclusively in `post-init-company' functions or
`init-company-xxx' function where xxx is company backend package.

Available PROPS:

`:backends BACKENDS'
   One or several symbols or lists representing a company backend or a list of
   company backends.

`:modes MODES'
    One or several modes where BACKENDS will be added.

`:variables VAR VALUE'
    One or several VAR VALUE pairs (similar to layer variables).
    These variables are made buffer local so their values are set only for
    the given MODES.

`:from SYMBOL'
    Advanced property aimed at avoiding hook function name conflicts when
    `:variables' property is used in several calls to this macro for the same
    MODES.

`:append-hook BOOLEAN'
    Advanced property to control whether hooks functions are hooked or not,
    if non-nil hook functions are appended to modes hooks passed as `:modes'.

`:call-hooks BOOLEAN'
    if non-nil then hooked functions are called right away."
  (declare (indent 0))
  (let* ((backends (spacemacs/mplist-get-values props :backends))
         (modes (spacemacs/mplist-get-values props :modes))
         (variables (spacemacs/mplist-get-values props :variables))
         (from (spacemacs/mplist-get-value props :from))
         (hooks (if (memq :append-hooks props)
                    (spacemacs/mplist-get-value props :append-hooks)
                  t))
         (call-hooks (when (memq :call-hooks props)
                       (spacemacs/mplist-get-value props :call-hooks)))
         (result '(progn)))
    (dolist (mode modes)
      (let ((backends-var-name (intern (format "company-backends-%S" mode)))
            (raw-backends-var-name (intern (format "company-backends-%S-raw"
                                                   mode)))
            (init-func-name (intern (format "spacemacs//init-company-%S" mode)))
            (vars-func-name (intern
                             (format "spacemacs//init-company-vars-%S%s" mode
                                     (if from (format "-%S" from) ""))))
            (mode-hook-name (intern (format "%S-hook" mode))))
        ;; declare buffer local company-backends variable
        (push `(defvar ,raw-backends-var-name
                 spacemacs-default-company-backends
                 ,(format "Company backend list for %S." mode)) result)
        (push `(defvar ,backends-var-name ,raw-backends-var-name
                 ,(format "Company backend list for %S." mode)) result)
        ;; add backends
        (dolist (backend backends)
          (push `(add-to-list ',raw-backends-var-name ',backend) result))
        ;; define initialization hook function
        (push `(defun ,init-func-name ()
                 ,(format "Initialize company for %S." mode)
                 (if auto-completion-enable-snippets-in-popup
                     (setq ,backends-var-name
                           (mapcar 'spacemacs//show-snippets-in-company
                                   ,raw-backends-var-name))
                   (setq ,backends-var-name ,raw-backends-var-name))
                 (set (make-variable-buffer-local 'auto-completion-front-end)
                      'company)
                 (set (make-variable-buffer-local 'company-backends)
                      ,backends-var-name)) result)
        (when call-hooks
          (push `(,init-func-name) result))
        (when hooks
          (push `(add-hook ',mode-hook-name ',init-func-name t) result))
        ;; define variables hook function
        (when variables
          (let ((variables-copy variables)
                (vars-func `(defun ,vars-func-name ()
                              ,(format "Define company local variables for %S."
                                       mode)))
                vars)
            (while variables-copy
              (let* ((var (pop variables-copy))
                     (forms
                      (when (consp variables-copy)
                        `(set (make-variable-buffer-local ',var)
                              ,(eval (pop variables-copy))))))
                (when forms (push forms vars))))
            (push (append vars-func vars) result))
          (when call-hooks
            (push `(,vars-func-name) result))
          (when hooks
            (push `(add-hook ',mode-hook-name ',vars-func-name t) result)))
        (when hooks
          (push `(add-hook ',mode-hook-name 'company-mode t) result))))
    ;; return the expanded macro in correct order
    (reverse result)))

(defmacro spacemacs|disable-company (mode)
  "Disable company for the given MODE.
MODE parameter must match the :modes values used in the call to
`spacemacs|add-company-backends'."
  (let ((mode-hook-name (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-company-%S" mode))))
    `(progn
       (remove-hook ',mode-hook-name ',func)
       (remove-hook ',mode-hook-name 'company-mode))))

(defun spacemacs//show-snippets-in-company (backend)
  (if (or (not auto-completion-enable-snippets-in-popup)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))


;; auto-completion key bindings functions

(defun spacemacs//auto-completion-set-RET-key-behavior (package)
  "Bind RET key appropriately for the given PACKAGE and value of
`auto-completion-return-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-return-key-behavior)
        (define-key map [return] 'company-complete-selection)
        (define-key map (kbd "RET") 'company-complete-selection))
       (t
        (define-key map [return] 'nil)
        (define-key map (kbd "RET") 'nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun spacemacs//auto-completion-set-TAB-key-behavior (package)
  "Bind TAB key appropriately for the given PACKAGE and value of
`auto-completion-tab-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-selection)
        (define-key map (kbd "<tab>") 'company-complete-selection))
       ((eq 'cycle auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key map (kbd "<S-tab>")
          'spacemacs//company-complete-common-or-cycle-backward)
        (define-key map (kbd "<backtab>")
          'spacemacs//company-complete-common-or-cycle-backward))
       (t
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "<tab>") nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun spacemacs//auto-completion-setup-key-sequence (package)
  "Setup the key sequence to complete current selection."
  (when auto-completion-complete-with-key-sequence
    (let ((first-key (elt auto-completion-complete-with-key-sequence 0)))
      (cond ((eq 'company package)
             (define-key company-active-map (kbd (char-to-string first-key))
               'spacemacs//auto-completion-key-sequence-start))
            (t (message "Not yet implemented for package %S" package))))))


;; key sequence to complete selection

(defvar spacemacs--auto-completion-time nil)
(defvar spacemacs--auto-completion-complete-last-candidate nil)
(defvar spacemacs--auto-completion-shadowed-insert-binding nil)
(defvar spacemacs--auto-completion-shadowed-emacs-binding nil)
(defvar spacemacs--auto-completion-shadowed-hybrid-binding nil)

(defun spacemacs//auto-completion-key-sequence-start ()
  "Initiate auto-completion sequence."
  (interactive)
  (self-insert-command 1)
  (setq spacemacs--auto-completion-complete-last-candidate
        (cond
         ((bound-and-true-p company-mode)
          (nth company-selection company-candidates))))
  ;; enable second key of the sequence
  (let ((second-key (kbd (char-to-string
                          (elt auto-completion-complete-with-key-sequence 1)))))
    (setq spacemacs--auto-completion-shadowed-insert-binding
          (lookup-key evil-insert-state-map second-key))
    (setq spacemacs--auto-completion-shadowed-emacs-binding
          (lookup-key evil-emacs-state-map second-key))
    (setq spacemacs--auto-completion-shadowed-hybrid-binding
          (lookup-key evil-hybrid-state-map second-key))
    (define-key
      evil-insert-state-map
      second-key
      'spacemacs//auto-completion-key-sequence-end)
    (define-key
      evil-emacs-state-map
      second-key
      'spacemacs//auto-completion-key-sequence-end)
    (define-key
      evil-hybrid-state-map
      second-key
      'spacemacs//auto-completion-key-sequence-end))
  ;; set a timer to restore the old bindings
  (run-at-time auto-completion-complete-with-key-sequence-delay
               nil
               'spacemacs//auto-completion-key-sequence-restore)
  (when spacemacs--auto-completion-complete-last-candidate
    (setq spacemacs--auto-completion-time (current-time))))

(defun spacemacs//auto-completion-key-sequence-end ()
  "Check if the auto-completion key sequence has been entered."
  (interactive)
  (if (or (null spacemacs--auto-completion-time)
          (< auto-completion-complete-with-key-sequence-delay
             (float-time (time-since spacemacs--auto-completion-time))))
      (self-insert-command 1)
    (cond
     ((bound-and-true-p company-mode)
      (unless company-candidates
        ;; if the auto-completion menu is still active then we don't need to
        ;; delete the last inserted first key of the sequence
        (delete-char -1))
      (let ((company-idle-delay))
        (company-auto-begin)
        (company-finish spacemacs--auto-completion-complete-last-candidate)))))
  (spacemacs//auto-completion-key-sequence-restore)
  (setq spacemacs--auto-completion-time nil))

(defun spacemacs//auto-completion-key-sequence-restore ()
  "Restore the shadowed key bindings used to auto-complete."
  (let ((second-key (kbd (char-to-string
                          (elt auto-completion-complete-with-key-sequence 1)))))
    (define-key
      evil-insert-state-map
      second-key
      spacemacs--auto-completion-shadowed-insert-binding)
    (define-key
      evil-emacs-state-map
      second-key
      spacemacs--auto-completion-shadowed-emacs-binding)
    (define-key
      evil-hybrid-state-map
      second-key
      spacemacs--auto-completion-shadowed-hybrid-binding)))


;; Editing style

(defun spacemacs//company-active-navigation (style)
  "Set navigation for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-style-enable-hjkl-bindings))
    (dolist (map (list company-active-map company-search-map))
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection))
    ;; Fix company-quickhelp Evil C-k
    (let ((prev nil))
      (defun spacemacs//set-C-k-company-select-previous (&rest args)
        (setf prev (lookup-key evil-insert-state-map (kbd "C-k")))
        (define-key evil-insert-state-map (kbd "C-k") 'company-select-previous))
      (defun spacemacs//restore-C-k-evil-insert-digraph (&rest args)
        (define-key evil-insert-state-map (kbd "C-k") prev)))
    (add-hook 'company-completion-started-hook 'spacemacs//set-C-k-company-select-previous)
    (add-hook 'company-completion-finished-hook 'spacemacs//restore-C-k-evil-insert-digraph)
    (add-hook 'company-completion-cancelled-hook 'spacemacs//restore-C-k-evil-insert-digraph))
   (t
    (dolist (map (list company-active-map company-search-map))
      (define-key map (kbd "C-n") 'company-select-next)
      (define-key map (kbd "C-p") 'company-select-previous)))))



(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))


;; helm-yas

(defun spacemacs/helm-yas ()
  "Properly lazy load helm-c-yasnipper."
  (interactive)
  (spacemacs/load-yasnippet)
  (require 'helm-c-yasnippet)
  (call-interactively 'helm-yas-complete))


;; ivy-yas

(defun spacemacs/ivy-yas ()
  "Lazy load ivy-yasnippet"
  (interactive)
  (spacemacs/load-yasnippet)
  (require 'ivy-yasnippet)
  (call-interactively 'ivy-yasnippet))


;; Yasnippet

(defun spacemacs/load-yasnippet ()
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(defun spacemacs/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))


;; Auto-Yasnippet

(defun spacemacs/auto-yasnippet-expand ()
  "Call `yas-expand' and switch to `insert state'"
  (interactive)
  (call-interactively 'aya-expand)
  (evil-insert-state))


;; Yasnippet and Smartparens

;; If enabled, smartparens will mess snippets expanded by `hippie-expand`.
;; We want to temporarily disable Smartparens during the snippet expansion and
;; switch it back to the initial state when done.
;;
;; However, there is an asymmetry in Yasnippet's hooks:
;; * `yas-before-expand-snippet-hook' is called for all snippet expansions,
;; including the nested ones.
;; * `yas-after-exit-snippet-hook' is called only for the top level snippet,
;; but NOT for the nested ones.
;;
;; That's why we introduce `spacemacs--yasnippet-expanding' below.
;;
;; MWO 2021-03-16
;; I have removed spacemacs--yasnippet-expanding as it prevents
;; default yasnippet expansions from seeing the value of smartparens-mode.
;; This will effectively disable smartparens with the first yasnippet expand.
;; As `hippie-expand' is less frequently used than yasnippet I think it is
;; better to have smartparens state preserved with the default case.

(defvar spacemacs--smartparens-enabled-initially nil
  "Stored whether smartparens is originally enabled or not.")

(defun spacemacs//smartparens-disable-before-expand-snippet ()
  "Handler for `yas-before-expand-snippet-hook'.
Disable smartparens and remember its initial state."
  ;; Remember the initial smartparens state only once, when expanding a top-level snippet.
  (setq spacemacs--smartparens-enabled-initially (or spacemacs--smartparens-enabled-initially smartparens-mode smartparens-strict-mode))
  (spacemacs//deactivate-smartparens))

(defun spacemacs//smartparens-restore-after-exit-snippet ()
  "Handler for `yas-after-exit-snippet-hook'.
 Restore the initial state of smartparens."
  (when spacemacs--smartparens-enabled-initially
    (spacemacs//activate-smartparens)))
