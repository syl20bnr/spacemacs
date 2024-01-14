;;; company-tng.el --- company-mode configuration for single-button interaction  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021, 2023  Free Software Foundation, Inc.

;; Author: Nikita Leshenko

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; company-tng (Tab and Go) allows you to perform completion using just TAB.
;; Pressing it will both select the next completion candidate in the list and
;; insert it into the buffer (or make it look like it's inserted, in fact).
;;
;; It cycles the candidates like `yank-pop' or `dabbrev-expand' or Vim:
;; Pressing TAB selects the first item in the completion menu and inserts it in
;; the buffer. Pressing TAB again selects the second item and replaces the
;; "inserted" item with the second one. This can continue as long as the user
;; wishes to cycle through the menu. You can also press S-TAB to select the
;; previous candidate, of course.
;;
;; The benefits are that you only have to use one shortcut key and there is no
;; need to confirm the entry.
;;
;; Usage:
;;
;; Enable `company-tng-mode' with:
;;
;;   (add-hook 'after-init-hook 'company-tng-mode)
;;
;; in your init script. It will set up the required frontend, as well as make a
;; number of recommended configuration changes described below.
;;
;; To avoid these changes, if you want to tweak everything yourself, customize
;;`company-tng-auto-configure' to nil.
;;
;; We recommend to bind TAB to `company-select-next', S-TAB to
;; `company-select-previous', and unbind RET and other now-unnecessary
;; keys from `company-active-map':
;;
;;   (define-key company-active-map (kbd "TAB") 'company-select-next)
;;   (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
;;   (define-key company-active-map (kbd "RET") nil)
;;
;; Note that it's not necessary to rebind keys to use this frontend,
;; you can use the arrow keys or M-n/M-p to select and insert
;; candidates. You also need to decide which keys to unbind, depending
;; on whether you want them to do the Company action or the default
;; Emacs action (for example C-s or C-w).
;;
;; We recommend to disable `company-require-match' to allow free typing at any
;; point.
;;
;; By default, company-tng doesn't work well with backends that insert function
;; arguments into the buffer and (optionally) expand them into a snippet
;; (usually performed in `post-completion' using yasnippet or company-template).
;; In company-tng, completion candidates
;; are inserted into the buffer as the user selects them and the completion is
;; finished implicitly when the user continues typing after selecting a
;; candidate. Modifying the buffer (by expanding a snippet) when the user
;; continues typing would be surprising and undesirable, since the candidate was
;; already inserted into the buffer.
;;
;; For this reason `company-tng-mode' by default disables arguments insertion
;; for a number of popular backends. If the backend you are using is not among
;; them, you might have to configure it not to do that yourself.
;;
;; YASnippet and company-tng both use TAB, which causes conflicts. The
;; recommended way to use YASnippet with company-tng is to choose a different
;; key for expanding a snippet and moving to the next snippet field:
;;
;;   (define-key yas-minor-mode-map "\C-j" 'yas-expand)
;;   (define-key yas-keymap "\C-j" 'yas-next-field-or-maybe-expand)
;;   (dolist (keymap (list yas-minor-mode-map yas-keymap))
;;     (define-key keymap (kbd "TAB") nil)
;;     (define-key keymap [(tab)] nil))

;;; Code:

(require 'company)
(require 'cl-lib)

(defvar-local company-tng--overlay nil)

;;;###autoload
(defun company-tng-frontend (command)
  "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion."
  (cl-case command
    (show
     (let ((ov (make-overlay (point) (point))))
       (setq company-tng--overlay ov)
       (overlay-put ov 'priority 2)))
    (update
     (let* ((ov company-tng--overlay)
            (selected (and company-selection
                           (nth company-selection company-candidates)))
            (prefix (length company-prefix)))
       (move-overlay ov (- (point) prefix) (point))
       (overlay-put ov
                    (if (= prefix 0) 'after-string 'display)
                    selected)))
    (hide
     (when company-tng--overlay
       (delete-overlay company-tng--overlay)
       (kill-local-variable 'company-tng--overlay)))
    (pre-command
     (when (and company-selection
                (not (company--company-command-p (this-command-keys))))
       (company--unread-this-command-keys)
       (setq this-command 'company-complete-selection)))))

(defvar company-clang-insert-arguments)
(defvar company-semantic-insert-arguments)
(defvar company-rtags-insert-arguments)
(defvar lsp-enable-snippet)

(defgroup company-tng nil
  "Company Tab and Go."
  :group 'company)

(defcustom company-tng-auto-configure t
  "Automatically apply default configure when enable `company-tng-mode'."
  :type 'boolean)

;;;###autoload
(define-obsolete-function-alias 'company-tng-configure-default 'company-tng-mode "0.10.0"
  "Applies the default configuration to enable company-tng.")

(declare-function eglot--snippet-expansion-fn "eglot")

(defvar company-tng-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap company-active-map)
    (define-key keymap [return] nil)
    (define-key keymap (kbd "RET") nil)
    (define-key keymap [tab] 'company-select-next)
    (define-key keymap (kbd "TAB") 'company-select-next)
    (define-key keymap [backtab] 'company-select-previous)
    (define-key keymap (kbd "S-TAB") 'company-select-previous)
    keymap))

;;;###autoload
(define-minor-mode company-tng-mode
 "This minor mode enables `company-tng-frontend'."
  :init-value nil
  :global t
  (cond
   (company-tng-mode
    (setq company-frontends
          (add-to-list 'company-frontends 'company-tng-frontend))
    (when company-tng-auto-configure
      (setq company-frontends '(company-tng-frontend
                                company-pseudo-tooltip-frontend
                                company-echo-metadata-frontend))
      (setq company-require-match nil
            company-clang-insert-arguments nil
            company-semantic-insert-arguments nil
            company-rtags-insert-arguments nil
            lsp-enable-snippet nil)
      (advice-add #'eglot--snippet-expansion-fn :override #'ignore)
      (setq company-active-map company-tng-map))
    (setq company-selection-default nil))
   (t
    (setq company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-preview-if-just-one-frontend
            company-echo-metadata-frontend))
    (when company-tng-auto-configure
      (setq company-require-match 'company-explicit-action-p
            company-clang-insert-arguments t
            company-semantic-insert-arguments t
            company-rtags-insert-arguments t
            lsp-enable-snippet t)
      (advice-remove #'eglot--snippet-expansion-fn #'ignore)
      (setq company-active-map (keymap-parent company-tng-map)))
    (setq company-selection-default 0))))

(provide 'company-tng)
;;; company-tng.el ends here
