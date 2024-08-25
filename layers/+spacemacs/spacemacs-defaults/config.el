;;; config.el --- Spacemacs Defaults Layer configuration File
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


;; ---------------------------------------------------------------------------
;; Navigation
;; ---------------------------------------------------------------------------

;; Auto refresh
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Make dired "guess" target directory for some operations, like copy to
;; directory visited in other split buffer.
(setq dired-dwim-target t)

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar spacemacs-useless-buffers-regexp '()
  "Regexp used to determine if a buffer is not useful.")
(defvar spacemacs-useful-buffers-regexp '()
  "Regexp used to define buffers that are useful despite matching
`spacemacs-useless-buffers-regexp'.")

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)

(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  (tabulated-list-print t))

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; Scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Don't accept SPC as a yes for prompts
(unless dotspacemacs-use-SPC-as-y
  (define-key query-replace-map (kbd "SPC") nil))

;; ---------------------------------------------------------------------------
;; Mouse
;; ---------------------------------------------------------------------------

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

(when (boundp 'mouse-wheel-scroll-amount)
  ;; scroll two line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(2)
        ;; don't accelerate scrolling
        mouse-wheel-progressive-speed nil))

;; ---------------------------------------------------------------------------
;; Edit
;; ---------------------------------------------------------------------------

;; bump of the undo limits to avoid issues with premature
;; Emacs GC which truncates the undo history very aggressively
(setq-default
 undo-limit 80000000
 undo-strong-limit 120000000
 undo-outer-limit 360000000)

;; Start with the *scratch* buffer in text mode (speeds up Emacs load time,
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Text
(setq longlines-show-hard-newlines t)

;; Use system trash for file deletion.
;; This should work on Windows and Linux distros.
;; For macOS, see the osx layer.
(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)
(spacemacs|diminish auto-fill-function " Ⓕ" " F")

;; persistent abbreviation file
(setq abbrev-file-name (concat spacemacs-cache-directory "abbrev_defs"))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; The C-d rebinding that most shell-like buffers inherit from
;; comint-mode assumes non-evil configuration with its
;; `comint-delchar-or-maybe-eof' function, so we disable it
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))

;; Prompt to open file literally if large file.
(add-hook 'find-file-hook 'spacemacs/check-large-file)

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

;; important for golden-ratio to better work
(setq window-combination-resize t)
;; Show column number in mode line
(setq column-number-mode t)

;; highlight current line
(global-hl-line-mode t)
;; no blink
(blink-cursor-mode 0)
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; Fullscreen/maximize frame on startup
(when (and (not spacemacs-initialized)
           dotspacemacs-fullscreen-at-startup)
  ;; spacemacs/toggle-fullscreen-frame-on is NOT available during the startup,
  ;; but IS available during the subsequent config reloads
  (if (fboundp 'spacemacs/toggle-fullscreen-frame-on)
      (spacemacs/toggle-fullscreen-frame-on)
    (spacemacs/toggle-frame-fullscreen)))

(setq ns-use-native-fullscreen (not dotspacemacs-fullscreen-use-non-native))

;; make `next-buffer', `other-buffer', etc. ignore useless buffers (see
;; `spacemacs/useless-buffer-p')
(let ((buf-pred-entry (assq 'buffer-predicate default-frame-alist)))
  (if buf-pred-entry
      ;; `buffer-predicate' entry exists, modify it
      (setcdr buf-pred-entry #'spacemacs/useful-buffer-p)
    ;; `buffer-predicate' entry doesn't exist, create it
    (push '(buffer-predicate . spacemacs/useful-buffer-p) default-frame-alist)))

(add-to-list 'window-persistent-parameters '(spacemacs-max-state . writable))

;; ---------------------------------------------------------------------------
;; Session
;; ---------------------------------------------------------------------------

;; scratch buffer empty
(setq initial-scratch-message dotspacemacs-initial-scratch-message)
;; don't create backup~ files
(setq make-backup-files nil)

;; Auto-save file
(setq auto-save-default (not (null dotspacemacs-auto-save-file-location)))
(setq auto-save-list-file-prefix (concat spacemacs-auto-save-directory))
;; always save TRAMP URLs to cache directory no matter what is the value
;; of `dotspacemacs-auto-save-file-location'
(let ((autosave-dir (concat spacemacs-auto-save-directory "dist/")))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir  t)))
  (unless (or (file-exists-p autosave-dir)
              (null dotspacemacs-auto-save-file-location))
    (make-directory autosave-dir t)))
;; Choose auto-save location
(cl-case dotspacemacs-auto-save-file-location
  (cache (let ((autosave-dir (concat spacemacs-auto-save-directory "site/")))
           (add-to-list 'auto-save-file-name-transforms
                        `(".*" ,autosave-dir t) 'append)
           (unless (file-exists-p autosave-dir)
             (make-directory autosave-dir t))))
  (original (auto-save-visited-mode t))
  (_ (setq auto-save-default nil
           auto-save-list-file-prefix nil)))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; cache files
(setq tramp-persistency-file-name (concat spacemacs-cache-directory "tramp"))

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;; remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

;; ---------------------------------------------------------------------------
;; Other
;; ---------------------------------------------------------------------------

;; hook into `hack-local-variables' in order to allow switching spacemacs
;; configurations based on local variables
(add-hook 'hack-local-variables-hook #'spacemacs//run-local-vars-mode-hook)

;; Add buffer reference to internal list of killed buffers on `kill-buffer',
;; used for restoring recently killed buffers.
(add-hook 'kill-buffer-hook #'spacemacs//add-buffer-to-killed-list)

;; Don't load outdated compiled files.
(setq load-prefer-newer t)

;; Suppress the *Warnings* buffer when native compilation shows warnings.
(setq native-comp-async-report-warnings-errors 'silent)
