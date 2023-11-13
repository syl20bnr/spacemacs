;;; evil-collection-geiser.el --- Bindings for `geiser' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, geiser, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Bindings for `geiser'.

;;; Code:
(require 'evil-collection)
(require 'geiser nil t)

(defvar geiser-debug-mode-map)
(defvar geiser-doc-mode-map)

(defconst evil-collection-geiser-maps '(geiser-debug-mode-map
                                        geiser-doc-mode-map
                                        geiser-repl-mode-map
                                        geiser-mode-map))

(defun evil-collection-geiser-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

;;;###autoload
(defun evil-collection-geiser-setup ()
  "Set up bindings for `geiser'."
  (unless evil-move-beyond-eol
    (advice-add 'geiser-eval-last-sexp :around 'evil-collection-geiser-last-sexp)
    (advice-add 'geiser-eval-last-sexp-and-print :around 'evil-collection-geiser-last-sexp))

  (evil-set-initial-state 'geiser-debug-mode 'normal)
  (evil-set-initial-state 'geiser-doc-mode 'normal)

  (evil-collection-define-key 'normal 'geiser-debug-mode-map
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'geiser-doc-mode-map
    (kbd "<tab>") 'forward-button
    (kbd "<S-tab>") 'backward-button
    "gd" 'geiser-edit-symbol-at-point
    (kbd "C-t") 'geiser-pop-symbol-stack
    "gr" 'geiser-doc-refresh
    "q" 'View-quit
    "gz" 'geiser-doc-switch-to-repl
    ">" 'geiser-doc-next
    "<" 'geiser-doc-previous
    "gj" 'forward-button
    "gk" 'backward-button
    (kbd "C-j") 'forward-button
    (kbd "C-k") 'backward-button
    "]]" 'geiser-doc-next-section
    "[[" 'geiser-doc-previous-section
    "x" 'geiser-doc-kill-page
    "X" 'geiser-doc-clean-history)

  (evil-collection-define-key 'insert 'geiser-repl-mode-map
    (kbd "S-<return>") 'geiser-repl--newline-and-indent)

  (evil-collection-define-key 'normal 'geiser-repl-mode-map
    "gd" 'geiser-edit-symbol-at-point
    (kbd "C-t") 'geiser-pop-symbol-stack
    "gj" 'geiser-repl-next-prompt
    "gk" 'geiser-repl-previous-prompt
    (kbd "C-j") 'geiser-repl-next-prompt
    (kbd "C-k") 'geiser-repl-previous-prompt
    "]]" 'geiser-repl-next-prompt
    "[[" 'geiser-repl-previous-prompt
    "K" 'geiser-doc-symbol-at-point)

  (evil-collection-define-key 'normal 'geiser-mode-map
    "gd" 'geiser-edit-symbol-at-point
    (kbd "C-t") 'geiser-pop-symbol-stack
    "gZ" 'geiser-mode-switch-to-repl-and-enter
    "gz" 'geiser-mode-switch-to-repl
    "K" 'geiser-doc-symbol-at-point))

(provide 'evil-collection-geiser)
;;; evil-collection-geiser.el ends here
